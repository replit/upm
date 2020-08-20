package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"net/http"
	"os"
	"sort"
	"strings"
	"sync"
)

func GetPackageMetadata(packageName string) (PackageData, error) {
	resp, err := http.Get("https://pypi.org/pypi/" + packageName + "/json")
	if err != nil {
		return PackageData{}, err
	}
	defer resp.Body.Close()

	decoder := json.NewDecoder(resp.Body)

	data := PackageData{}
	decoder.Decode(&data)

	return data, nil
}

// NOTE: cache is read only
func ProcessPackage(packageName string, cached PackageInfo) (PackageInfo, error) {
	// Get the package metadata from pypi
	metadata, err := GetPackageMetadata(packageName)

	if err != nil {
		return PackageInfo{}, PypiError{DownloadFailure, "", err}
	}

	// Determine which dist we want to use to determine modules
	latest := metadata.Releases[metadata.Info.Version]
	if len(latest) == 0 {
		return PackageInfo{}, PypiError{NoDistributions, metadata.Info.Version, nil}
	}

	// Sort the releases by priority we want to parse
	distPriorities := map[string]int{
		"bdist_wheel": 2,
		"sdist":       1,
	}

	sort.Slice(latest, func(a, b int) bool {
		return distPriorities[latest[a].PackageType] < distPriorities[latest[b].PackageType]
	})

	// Check if cached module extraction is out of date
	if latest[0].MD5 != cached.MD5 {

		// Download the distribution and extract the modules
		modules, err := GetModules(latest[0])
		if err != nil {
			return PackageInfo{}, err
		}

		// TODO(@zabot) Pypi stats cannot be fetched from the API and have to be
		// preinserted into the cache
		metadata.Info.Downloads = cached.Downloads

		// Copy the information from the specific dist into the info
		metadata.Info.Modules = modules
		metadata.Info.MD5 = latest[0].MD5
		return metadata.Info, nil
	}

	// If we hit in the cache, no need to download the distribution
	return cached, nil
}

type PackageResults struct {
	info PackageInfo
	err  error
}

func main() {
	cacheFile := flag.String("cache", "cache.json", "A json file to seed the map from. This file will be rewritten with up to date information.")
	bigqueryFile := flag.String("bq", "bq.json", "The result of a BigQuery against the pypi downloads dataset.")
	gcp := flag.String("gcp", "", "A GCP project ID to use to query bigquery directly. The result will be written to bq.")
	pkg := flag.String("pkg", "python", "the pkg name for the output source")
	out := flag.String("out", "pypi_map.gen.go", "the destination file for the generated code")
	flag.Parse()

	// Load info from the package cache
	packageCache, err := LoadCache(*cacheFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Skippping cache: %v\n", err)
	}

	var bqCache PackageCache
	if *gcp != "" {
		bqCache, err = GetPypiStats(*gcp)
		DumpCache(*bigqueryFile, bqCache)
	} else {
		bqCache, err = LoadCache(*bigqueryFile)
	}

	if err != nil {
		fmt.Fprintf(os.Stderr, "Skippping bq data, downloads will not be available: %v\n", err)
	} else {
		MergeCache(packageCache, bqCache)
	}

	// Scan pypi for all packages
	discoveredPackages := 0
	packages, _ := NewPackageIndex("https://pypi.org/simple/", -1)

	workers := 200

	// Each package is handled in a seperate goroutine, the total number
	// concurrent is limited by the buffer size of this channel
	resultQueue := make(chan PackageResults, workers)
	concurrencyLimiter := make(chan struct{}, workers)
	var wg sync.WaitGroup

	// Once goroutines start returning, we need to update the cache while still
	// reading it for future packages
	packageCacheLock := sync.RWMutex{}

	for packages.Next() {
		discoveredPackages++
		packageName := packages.Package()

		// Register every goroutine with the wait group before we start it
		wg.Add(1)
		go func() {
			// Notify the wait group when finished
			defer wg.Done()

			// Block until there is room for more goroutines. This way we don't
			// overload the number of open connections. Release our spot when finished
			concurrencyLimiter <- struct{}{}
			defer func() { <-concurrencyLimiter }()

			// Get the package info for this package
			// Sync note: We have to explicitly release the lock instead of releasing
			// it in a defer because the other end of infoQueue is read in a function
			// that must aquire a write lock. Explicitly releasing the lock before
			// writing to the channel prevents this possible deadlock
			packageCacheLock.RLock()
			cached := packageCache[packageName]
			packageCacheLock.RUnlock()

			packageInfo, err := ProcessPackage(packageName, cached)
			packageInfo.Name = packageName
			resultQueue <- PackageResults{packageInfo, err}
		}()
	}

	fmt.Printf("Discovered %v packages\n", discoveredPackages)

	// Open a JSON encoder to stream the package list to a file as it comes in
	cacheWriter, err := os.Create(*cacheFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to open cache file for writing\n")
	}
	defer cacheWriter.Close()
	cacheEncoder := json.NewEncoder(cacheWriter)

	cacheBuffer := make([]PackageInfo, 0, workers)
	for processedPackages := 0; processedPackages < discoveredPackages; processedPackages++ {
		result := <-resultQueue

		if result.err != nil {
			fmt.Fprintf(os.Stderr, "{\"package\": \"%v\", \"error\": %v\n", result.info.Name, result.err)
		} else {
			// Grabbing the cache lock after every message is written to the channel
			// destroys any multithreading benefit. Buffer up cache updates and write
			// them all at once
			cacheBuffer = append(cacheBuffer, result.info)

			// Update the disk cache
			cacheEncoder.Encode(result.info)
		}

		// Print progress updates to stdout
		ppu := discoveredPackages / 100
		if ppu < 1 {
			ppu = 1
		}

		if processedPackages%ppu == 0 {
			fmt.Printf("%v/%v %v%%\n", processedPackages, discoveredPackages, 100*float64(processedPackages)/float64(discoveredPackages))
		}

		// Grab the write lock and update the cache
		if len(cacheBuffer) > int(float64(workers)*0.8) {
			packageCacheLock.Lock()
			for _, info := range cacheBuffer {
				packageCache[info.Name] = info
				cacheBuffer = cacheBuffer[:0]
			}
			packageCacheLock.Unlock()
		}
	}

	// After all packages have been processed, close channels
	close(resultQueue)

	// packageCache is now a map of every python package name to the PackageInfo
	// for that package, with downloads and modules populated

	// Map every module to a list of packages that can provide it
	var moduleToPackageList = map[string][]PackageInfo{}
	for _, info := range packageCache {
		for _, module := range info.Modules {
			moduleToPackageList[module] = append(moduleToPackageList[module], info)
		}
	}

	var moduleToPypiPackage = map[string]string{}
	var pypiPackageToModules = map[string]string{}

	// Guess at every module, add the guess and the package that was guessed to
	// the masp
	for module, packages := range moduleToPackageList {
		if guess, guessable := GuessPackage(module, packages); guessable {
			moduleToPypiPackage[module] = guess.Name
			pypiPackageToModules[guess.Name] = strings.Join(guess.Modules, ",")
		}
	}

	codeWriter, err := os.Create(*out)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to open output file for writing\n")
		return
	}
	defer codeWriter.Close()

	fmt.Fprintf(codeWriter, "package %v\n\n", *pkg)
	DumpMapToGoVar("moduleToPypiPackage", moduleToPypiPackage, codeWriter)
	DumpMapToGoVar("pypiPackageToModules", pypiPackageToModules, codeWriter)
}
