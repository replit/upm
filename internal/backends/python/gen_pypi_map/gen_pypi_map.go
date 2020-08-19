package main

import (
	"encoding/json"
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
func ProcessPackage(packageName string, cache PackageCache) (PackageInfo, error) {
	// Get the package metadata from pypi
	metadata, err := GetPackageMetadata(packageName)

	if err != nil {
		return PackageInfo{}, fmt.Errorf("Encountered error while fetching %v: %v", packageName, err)
	}

	// Determine which dist we want to use to determine modules
	latest := metadata.Releases[metadata.Info.Version]
	if len(latest) == 0 {
		return PackageInfo{}, fmt.Errorf("No package for latest release: %v", packageName)
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
	if latest[0].MD5 != cache[packageName].MD5 {

		// Download the distribution and extract the modules
		modules, err := GetModules(latest[0])
		if err != nil {
			return PackageInfo{}, fmt.Errorf("Encounter error while resolving packages for %v: %v", packageName, err)
		}

		// TODO(@zabot) Pypi stats cannot be fetched from the API and have to be
		// preinserted into the cache
		metadata.Info.Downloads = cache[packageName].Downloads

		// Copy the information from the specific dist into the info
		metadata.Info.Modules = modules
		metadata.Info.MD5 = latest[0].MD5
		return metadata.Info, nil
	}

	// If we hit in the cache, no need to download the distribution
	return cache[packageName], nil
}

func main() {
	// Load info from the package cache
	packageCache, err := LoadCache("cache.json")
	if err != nil {
		fmt.Printf("Skippping cache: %v\n", err)
	}

	bqCache, err := LoadCache("bq.json")
	if err != nil {
		fmt.Printf("Skippping bq data, downloads will not be available: %v\n", err)
	} else {
		MergeCache(packageCache, bqCache)
	}

	// Scan pypi for all packages
	discoveredPackages := 0
	packages, _ := NewPackageIndex("https://pypi.org/simple/")

	// Each package is handled in a seperate goroutine, the total number
	// concurrent is limited by the buffer size of this channel
	infoQueue := make(chan PackageInfo)
	concurrencyLimiter := make(chan struct{}, 10)
	var wg sync.WaitGroup

	for packages.Next() {
		discoveredPackages++
		packageName := packages.Package()

		go func() {
			// Register with the wait group
			wg.Add(1)
			defer wg.Done()

			// Push to the limiter channel and pop when done
			concurrencyLimiter <- struct{}{}
			defer func() { <-concurrencyLimiter }()

			// Get the package info for this package
			packageInfo, err := ProcessPackage(packageName, packageCache)
			if err != nil {
				fmt.Println(err)
				return
			}

			infoQueue <- packageInfo
		}()
	}

	// Open a JSON encoder to stream the package list to a file as it comes in
	cacheWriter, err := os.Create("cache.json")
	if err != nil {
		fmt.Println("Failed to open cache file for writing")
	}
	defer cacheWriter.Close()
	cacheEncoder := json.NewEncoder(cacheWriter)

	processedPackages := 0
	for info := range infoQueue {
		packageCache[info.Name] = info
		cacheEncoder.Encode(info)
		processedPackages++

		if processedPackages%10 == 0 {
			fmt.Println(processedPackages, "/", discoveredPackages)
		}

		// If we've processed everything, close the channel
		if processedPackages == discoveredPackages {
			close(infoQueue)
		}
	}

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

	fmt.Println("var moduleToPypiPackage = map[string]string{")
	for module, pkg := range moduleToPypiPackage {
		fmt.Printf("  \"%v\":  \"%v\",\n", module, pkg)
	}
	fmt.Println("}")

	fmt.Println("var pypiPackageToModules = map[string]string{")
	for pkg, modules := range pypiPackageToModules {
		fmt.Printf("  \"%v\":  \"%v\",\n", pkg, modules)
	}
	fmt.Println("}")
}
