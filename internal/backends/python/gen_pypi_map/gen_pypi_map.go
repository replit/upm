package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"io/ioutil"
	"net/http"
	"os"
	"strings"
	"sync"
	"time"
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
func ProcessPackage(packageName string, cached PackageInfo, distMods bool) (PackageInfo, error) {
	// Get the package metadata from pypi
	metadata, err := GetPackageMetadata(packageName)

	if err != nil {
		return PackageInfo{}, PypiError{DownloadFailure, "", err}
	}

	// Check if cached module is out of date
	if metadata.Info.Version == cached.Version {
		// If we hit in the cache, no need to download the distribution
		return cached, nil
	}

	var modules []string
	if distMods {
		// Determine moudles by examining a distribution
		modules, err = GetModules(metadata)
	} else {
		// Determine the modules by installing the package
		modules, err = InstallDiff(metadata)
	}

	if err != nil {
		return PackageInfo{}, err
	}

	// Retrive the stats from the cache since they are no longer available from
	// pypi
	metadata.Info.Downloads = cached.Downloads

	// Copy the modules from the specific dist into the overall metadata
	metadata.Info.Modules = modules
	return metadata.Info, nil
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
	m2pout := flag.String("m2p-out", "module_to_pypi.json", "the destination file for the module to package mapping in json")
	p2mout := flag.String("p2m-out", "pypi_to_module.json", "the destination file for the module to package mapping in json")
	workers := flag.Int("workers", 16, "The number of simultaenous workers to run")
	distMods := flag.Bool("dist", false, "Determine modules by examining dists")
	flag.Parse()

	// Load info from the package cache
	packageCache, err := LoadCache(*cacheFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Skippping cache: %v\n", err)
	}

	var bqCache PackageCache
	if *gcp != "" {
		fmt.Printf("Fetching latest pypi stats from big query. This may take some time...\n")
		bqCache, err = GetPypiStats(*gcp)
		DumpCache(*bigqueryFile, bqCache)
		fmt.Printf("Fetched %v records, saved results to %v", len(bqCache), *bigqueryFile)
	} else {
		fmt.Printf("Loading pypi stats from cache file\n")
		bqCache, err = LoadCache(*bigqueryFile)
		fmt.Printf("Loaded %v stats\n", len(bqCache))
	}

	if err != nil {
		fmt.Fprintf(os.Stderr, "Skippping bq data, downloads will not be available: %v\n", err)
	} else {
		MergeCache(packageCache, bqCache)
	}

	// Scan pypi for all packages
	discoveredPackages := 0
	fmt.Printf("Scanning package index...\n")
	// packages, _ := NewPackageIndex("https://pypi.org/simple/", -1)
	packages := FakePackageIndex("Flask", "boto3", "discord.py")

	// Each package is handled in a seperate goroutine, the total number
	// concurrent is limited by the buffer size of this channel
	resultQueue := make(chan PackageResults, *workers)
	concurrencyLimiter := make(chan struct{}, *workers)
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

			packageInfo, err := ProcessPackage(packageName, cached, *distMods)
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

	errors := 0
	packageCount := 0
	modules := 0
	startTime := time.Now()
	lastStatus := startTime
	cacheBuffer := make([]PackageInfo, 0, *workers)
	fmt.Printf("Scanning package modules...\n")
	for processedPackages := 0; processedPackages < discoveredPackages; processedPackages++ {
		result := <-resultQueue

		if result.err != nil {
			errors++
			fmt.Fprintf(os.Stderr, "{\"package\": \"%v\", \"error\": %v\n", result.info.Name, result.err)
		} else {
			packageCount++
			modules += len(result.info.Modules)

			// Grabbing the cache lock after every message is written to the channel
			// destroys any multithreading benefit. Buffer up cache updates and write
			// them all at once
			cacheBuffer = append(cacheBuffer, result.info)

			// Update the disk cache
			cacheEncoder.Encode(result.info)
		}

		// Print progress updates to stdout
		if time.Since(lastStatus).Seconds() > 1 {
			lastStatus = time.Now()
			percentage := float64(processedPackages) / float64(discoveredPackages)

			elapsed := time.Since(startTime)
			rate := float64(processedPackages) / elapsed.Seconds()
			remaining := float64(discoveredPackages-processedPackages) / rate

			fmt.Printf("%v/%v %.2f%% [%.0fs]\n", processedPackages, discoveredPackages, 100*percentage, remaining)
		}

		// Grab the write lock and update the cache
		packageCacheLock.Lock()
		for _, info := range cacheBuffer {
			packageCache[info.Name] = info
			cacheBuffer = cacheBuffer[:0]
		}
		packageCacheLock.Unlock()
	}

	// After all packages have been processed, close channels
	close(resultQueue)

	fmt.Printf("Found %v modules in %v packages in %.0f seconds. %v packages failed\n", modules, packageCount, time.Since(startTime).Seconds(), errors)

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

	bytes, err := json.MarshalIndent(moduleToPypiPackage, "", "  ")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to serialize mapping to json: %s", err.Error())
	}
	err = ioutil.WriteFile(*m2pout, bytes, 0644)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to write json file: %s", err.Error())
	}

	bytes, err = json.MarshalIndent(pypiPackageToModules, "", "  ")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to serialize mapping to json: %s", err.Error())
	}
	err = ioutil.WriteFile(*p2mout, bytes, 0644)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to write json file: %s", err.Error())
	}
}
