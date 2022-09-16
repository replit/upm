package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"sort"
	"strings"
)

// func loadPackageInfo(packageName string, cacheDir string, cached *PackageInfo) error {
// 	filePath := cacheDir + "/" + packageName + ".json"
// 	return loadPackageInfoFile(filePath, cached)
// }

func LoadPackageInfoFile(filePath string, cached *PackageInfo) error {
	file, err := os.Open(filePath)
	if err != nil {
		return err
	}
	decoder := json.NewDecoder(file)
	err = decoder.Decode(cached)
	if err != nil {
		return err
	}
	defer file.Close()
	return nil
}

func SavePackageInfo(packageName string, cacheDir string, info *PackageInfo) error {
	err := os.MkdirAll(cacheDir, 0774)

	if err != nil {
		return err
	}
	filePath := cacheDir + "/" + packageName + ".json"
	writer, err := os.Create(filePath)
	if err != nil {
		return err
	}
	defer writer.Close()

	cacheEncoder := json.NewEncoder(writer)
	cacheEncoder.SetIndent("", "  ")
	err = cacheEncoder.Encode(&info)
	fmt.Printf("Wrote %s\n", filePath)
	if err != nil {
		return err
	}
	return nil
}

func LoadAllPackageInfo(cacheDir string, cacheFilePath string) map[string]PackageInfo {
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
		err := LoadPackageInfoFile(filePath, &info)
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
func UpdateAllPackageInfo(cacheDir string, cacheFilePath string) error {
	registry := LoadAllPackageInfo(cacheDir, cacheFilePath)

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
