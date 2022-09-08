package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strings"
)

func generateSource(pkg string, outputFilePath string, cacheDir string, bqFilePath string) error {
	downloadStats, err := LoadDownloadStats(bqFilePath)
	if err != nil {
		return err
	}
	// Map every module to a list of packages that can provide it
	files, err := ioutil.ReadDir(cacheDir)
	if err != nil {
		return err
	}

	var moduleToPackageList = map[string][]PackageInfo{}
	for _, file := range files {
		var info PackageInfo
		filePath := cacheDir + "/" + file.Name()
		err := loadPackageInfoFile(filePath, &info)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Warning: failed to load %s: %s\n", filePath, err.Error())
			continue
		}
		for _, module := range info.Modules {
			moduleToPackageList[module] = append(moduleToPackageList[module], info)
		}
	}

	fmt.Printf("Loaded %d modules\n", len(moduleToPackageList))

	var moduleToPypiPackage = map[string]string{}
	var pypiPackageToModules = map[string]string{}

	// Guess at every module, add the guess and the package that was guessed to
	// the masp
	for module, packages := range moduleToPackageList {
		if guess, guessable := GuessPackage(module, packages, downloadStats); guessable {
			moduleToPypiPackage[module] = guess.Name
			pypiPackageToModules[guess.Name] = strings.Join(guess.Modules, ",")
		}
	}

	codeWriter, err := os.Create(outputFilePath)

	if err != nil {
		return err
	}

	fmt.Fprintf(codeWriter, "package %v\n\n", pkg)
	DumpMapToGoVar("moduleToPypiPackage", moduleToPypiPackage, codeWriter)
	DumpMapToGoVar("pypiPackageToModules", pypiPackageToModules, codeWriter)

	fmt.Printf("Wrote %s\n", outputFilePath)
	codeWriter.Close()
	return nil

	// bytes, err := json.MarshalIndent(moduleToPypiPackage, "", "  ")
	// if err != nil {
	// 	fmt.Fprintf(os.Stderr, "Failed to serialize mapping to json: %s", err.Error())
	// }
	// err = ioutil.WriteFile(*m2pout, bytes, 0644)
	// if err != nil {
	// 	fmt.Fprintf(os.Stderr, "Failed to write json file: %s", err.Error())
	// }

	// bytes, err = json.MarshalIndent(pypiPackageToModules, "", "  ")
	// if err != nil {
	// 	fmt.Fprintf(os.Stderr, "Failed to serialize mapping to json: %s", err.Error())
	// }
	// err = ioutil.WriteFile(*p2mout, bytes, 0644)
	// if err != nil {
	// 	fmt.Fprintf(os.Stderr, "Failed to write json file: %s", err.Error())
	// }
}
