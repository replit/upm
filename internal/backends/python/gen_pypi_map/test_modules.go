package main

import (
	"encoding/json"
	"fmt"
	"net/http"
	"os"
	"regexp"
	"sync"
	"time"
)

func TestModules(packages PackageIndex, bigqueryFile string, cacheDir string, pkgsFile string, distMods bool, workers int, force bool) {
	fmt.Printf("Loading pypi stats from cache file\n")
	bqCache, err := LoadDownloadStats(bigqueryFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to load data from big query file")
		return
	}
	fmt.Printf("Loaded %v stats\n", len(bqCache))

	cache := LoadAllPackageInfo(cacheDir, pkgsFile)

	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to load cache file and directory")
		return
	}

	discoveredPackages := 0
	fmt.Printf("Using %d workers.\n", workers)
	fmt.Printf("Scanning package index...\n")

	// Each package is handled in a seperate goroutine, the total number
	// concurrent is limited by the buffer size of this channel
	resultQueue := make(chan PackageInfo, workers)
	concurrencyLimiter := make(chan struct{}, workers)
	var wg sync.WaitGroup

	for packages.Next() {
		discoveredPackages++
		packageName := packages.Package()

		// Register every goroutine with the wait group before we start it
		wg.Add(1)
		go func() {
			// Notify the wait group when finished
			defer wg.Done()

			// Block until there is room for more goroutines. This way we don't
			// overload the opennumber of open connections. Release our spot when finished
			concurrencyLimiter <- struct{}{}
			defer func() { <-concurrencyLimiter }()

			packageInfo, err := ProcessPackage(packageName, cache, cacheDir, distMods, force)
			packageInfo.Name = packageName
			if err != nil {
				fmt.Fprintf(os.Stderr, "Failed to process package: %s\n", err.Error())
				packageInfo.Error = err.Error()
			}
			resultQueue <- packageInfo
		}()
	}

	fmt.Printf("Discovered %v packages\n", discoveredPackages)

	errors := 0
	packageCount := 0
	modules := 0
	startTime := time.Now()
	lastStatus := startTime
	fmt.Printf("Scanning package modules...\n")
	for processedPackages := 0; processedPackages < discoveredPackages; processedPackages++ {
		result := <-resultQueue

		if result.Error != "" {
			errors++
			// fmt.Fprintf(os.Stderr, "{\"package\": \"%v\", \"error\": %v\n", result.Name, result.Error)
		} else {
			packageCount++
			modules += len(result.Modules)
		}

		// Print progress updates to stdout
		if time.Since(lastStatus).Seconds() > 1 {
			lastStatus = time.Now()
			percentage := float64(processedPackages) / float64(discoveredPackages)

			elapsed := time.Since(startTime)
			rate := float64(processedPackages) / elapsed.Seconds()
			remaining := float64(discoveredPackages-processedPackages) / rate

			fmt.Printf("%v/%v %.2f%% [%.0fs %.0fd]\n", processedPackages, discoveredPackages, 100*percentage, remaining, remaining/60/60/24)
		}
	}

	// After all packages have been processed, close channels
	close(resultQueue)

	fmt.Printf("Found %v modules in %v packages in %.0f seconds. %v packages failed\n", modules, packageCount, time.Since(startTime).Seconds(), errors)

	err = UpdateAllPackageInfo(cacheDir, pkgsFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to update cache: %s\n", err.Error())
	}
}

func GetPackageMetadata(packageName string) (PackageData, error) {
	resp, err := http.Get("https://pypi.org/pypi/" + packageName + "/json")
	if err != nil {
		return PackageData{}, err
	}

	if resp.StatusCode != 200 {
		return PackageData{}, fmt.Errorf("failed to get package info: %d", resp.StatusCode)
	}

	defer resp.Body.Close()

	decoder := json.NewDecoder(resp.Body)

	data := PackageData{}
	decoder.Decode(&data)

	idRegex := regexp.MustCompile("^([a-zA-Z-_0-9.]+)")
	var stripedRequiresDist []string

	for _, dep := range data.Info.RequiresDist {
		match := idRegex.FindStringSubmatch(dep)
		if len(match) > 0 {
			stripedRequiresDist = append(stripedRequiresDist, match[0])
		}
	}
	data.Info.RequiresDist = stripedRequiresDist

	return data, nil
}

// NOTE: cache is read only
func ProcessPackage(packageName string, cache map[string]PackageInfo, cacheDir string, distMods bool, force bool) (PackageInfo, error) {
	// Get the package metadata from pypi
	metadata, err := GetPackageMetadata(packageName)
	if err != nil {
		return PackageInfo{}, PypiError{DownloadFailure, "", err}
	}

	var cached PackageInfo = cache[packageName]

	// Check if cached module is out of date
	if !force && metadata.Info.Version == cached.Version {
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

	var retval PackageInfo
	retval.Version = metadata.Info.Version
	retval.Modules = modules
	retval.Name = metadata.Info.Name
	retval.RequiresDist = metadata.Info.RequiresDist

	if err != nil {
		retval.Error = err.Error()
	}

	err = SavePackageInfo(packageName, cacheDir, &retval)
	if err != nil {
		return PackageInfo{}, err
	}

	return retval, nil
}
