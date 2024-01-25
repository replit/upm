package main

import (
	"encoding/json"
	"fmt"
	"os"
	"regexp"
	"sync"
	"time"

	"github.com/replit/upm/internal/api"
)

func formatSeconds(totalSeconds int) string {
	days := totalSeconds / 60 / 60 / 24
	totalSeconds = totalSeconds % (60 * 60 * 24)
	hours := totalSeconds / 60 / 60
	totalSeconds = totalSeconds % (60 * 60)
	minutes := totalSeconds / 60
	totalSeconds = totalSeconds % 60
	seconds := totalSeconds

	return fmt.Sprintf("%02dd%02dh%02dm%02ds", days, hours, minutes, seconds)
}

func TestModules(packages PackageIndex, cacheDir string, pkgsFile string, distMods bool, workers int, force bool, timeout time.Duration) {

	cache := LoadAllPackageInfo(cacheDir, pkgsFile)

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

			packageInfo, err := ProcessPackage(packageName, cache, cacheDir, distMods, force, timeout)
			packageInfo.Name = packageName
			if err != nil {
				fmt.Fprintf(os.Stderr, "Failed to process package [%v]: %v\n", packageName, err)
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
			remaining := int(float64(discoveredPackages-processedPackages) / rate)

			fmt.Printf("%v/%v %.2f%% [%s]\n", processedPackages, discoveredPackages, 100*percentage, formatSeconds(remaining))
		}
	}

	// After all packages have been processed, close channels
	close(resultQueue)

	fmt.Printf("Found %v modules in %v packages in %s. %v packages failed\n", modules, packageCount, formatSeconds(int(time.Since(startTime).Seconds())), errors)
}

func GetPackageMetadata(packageName string) (PackageData, error) {
	resp, err := api.HttpClient.Get("https://pypi.org/pypi/" + packageName + "/json")
	if err != nil {
		return PackageData{}, err
	}

	if resp.StatusCode != 200 {
		return PackageData{}, fmt.Errorf("failed to get package info: %d", resp.StatusCode)
	}

	defer resp.Body.Close()

	decoder := json.NewDecoder(resp.Body)

	data := PackageData{}
	err = decoder.Decode(&data)
	if err != nil {
		return PackageData{}, err
	}

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
func ProcessPackage(packageName string, cache map[string]PackageInfo, cacheDir string, distMods bool, force bool, timeout time.Duration) (PackageInfo, error) {
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

	// Accumulate everything (including error info!) into retval
	var modules []string
	if distMods {
		// Determine modules by examining a distribution
		modules, err = GetModules(metadata)
	} else {
		// Determine the modules by installing the package
		modules, err = InstallDiff(metadata, timeout)
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
