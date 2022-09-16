package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"sort"
	"strings"
)

func loadCache(cacheDir string, cacheFilePath string) map[string]PackageInfo {
	registry := make(map[string]PackageInfo)

	file, err := os.Open(cacheFilePath)

	if err == nil {
		decoder := json.NewDecoder(file)
		for decoder.More() {
			var info PackageInfo
			decoder.Decode(&info)

			registry[strings.ToLower(info.Name)] = info
		}

		file.Close()
	}

	entries, err := ioutil.ReadDir(cacheDir)

	if err != nil {
		return registry
	}

	for _, entry := range entries {
		var info PackageInfo
		filePath := cacheDir + "/" + entry.Name()
		err := loadPackageInfoFile(filePath, &info)
		if err != nil {
			continue
		}
		registry[strings.ToLower(info.Name)] = info
	}

	return registry
}

/*
Merge files in the cache directory into the cache file
*/
func updateCache(cacheDir string, cacheFilePath string) error {
	registry := loadCache(cacheDir, cacheFilePath)

	keys := make([]string, len(registry))
	i := 0
	for key := range registry {
		keys[i] = key
		i++
	}

	sort.Strings(keys)

	cacheFile, err := os.Create(cacheFilePath)

	if err != nil {
		return err
	}

	encoder := json.NewEncoder(cacheFile)

	for _, key := range keys {
		encoder.Encode(registry[key])
	}

	cacheFile.Close()

	fmt.Printf("Wrote %s\n", cacheFilePath)

	return nil
}
