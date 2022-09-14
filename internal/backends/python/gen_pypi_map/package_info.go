package main

import (
	"encoding/json"
	"fmt"
	"os"
)

func loadPackageInfo(packageName string, cacheDir string, cached *PackageInfo) error {
	filePath := cacheDir + "/" + packageName + ".json"
	return loadPackageInfoFile(filePath, cached)
}

func loadPackageInfoFile(filePath string, cached *PackageInfo) error {
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

func savePackageInfo(packageName string, cacheDir string, info *PackageInfo) error {
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
