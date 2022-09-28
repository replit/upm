package main

import (
	"encoding/json"
	"fmt"
	"io"
	"os"
	"strings"
)

type ModuleItem struct {
	Module      string
	PackageList []PackageInfo
}

func GenerateCode(pkg string, outputFilePath string, cache map[string]PackageInfo, bqFilePath string, pkgsLegacyFile string) error {
	downloadStats, err := LoadDownloadStats(bqFilePath)
	if err != nil {
		return err
	}

	legacyPypiPackages := loadLegacyPypyPackages(pkgsLegacyFile)

	packagesProcessed := make(map[string]bool)
	var moduleToPackageList = map[string][]PackageInfo{}

	for _, info := range cache {
		pkgName := strings.ToLower(info.Name)
		if info.Error != "" {
			// fallback to legacy package module info
			legacyInfo, ok := legacyPypiPackages[pkgName]
			if ok {
				info.Modules = legacyInfo.Mods
			}
		}
		packagesProcessed[pkgName] = true
		for _, module := range info.Modules {
			moduleToPackageList[module] = append(moduleToPackageList[module], info)
		}
	}

	// Backfill legacy package info that is missing from our cache
	for pkg, legacyInfo := range legacyPypiPackages {
		_, ok := packagesProcessed[pkg]
		if ok {
			continue
		}
		var info PackageInfo
		info.Name = legacyInfo.Pkg
		info.Modules = legacyInfo.Mods

		for _, module := range info.Modules {
			moduleToPackageList[module] = append(moduleToPackageList[module], info)
		}
	}

	fmt.Printf("Loaded %d modules\n", len(moduleToPackageList))

	// fmt.Printf("Sorted modules by package list size\n")

	var moduleToPypiPackage = map[string]string{}
	var moduleToPypiPackageReason = map[string]string{}
	var pypiPackageToModules = map[string]string{}

	// Guess at every module, add the guess and the package that was guessed to
	// the masp
	for moduleName, candidates := range moduleToPackageList {
		if guess, reason, guessable := GuessPackage(moduleName, candidates, downloadStats); guessable {
			moduleToPypiPackage[moduleName] = guess.Name
			moduleToPypiPackageReason[moduleName] = reason
			pypiPackageToModules[guess.Name] = strings.Join(guess.Modules, ",")
		}
	}

	codeWriter, err := os.Create(outputFilePath)

	if err != nil {
		return err
	}

	fmt.Fprintf(codeWriter, "package %v\n\n", pkg)
	DumpMapToGoVar("moduleToPypiPackage", moduleToPypiPackage, moduleToPypiPackageReason, codeWriter)
	DumpMapToGoVar("pypiPackageToModules", pypiPackageToModules, map[string]string{}, codeWriter)
	DumpIntMapToGoVar("pypiPackageToDownloads", downloadStats, codeWriter)

	fmt.Printf("Wrote %s\n", outputFilePath)
	codeWriter.Close()
	return nil

}

func DumpMapToGoVar(name string, m map[string]string, reasons map[string]string, writer io.Writer) {
	fmt.Fprintf(writer, "var %v= map[string]string{\n", name)

	for key, value := range m {
		reason := reasons[key]
		if reason != "" {
			reason = "// " + reason
		}
		fmt.Fprintf(writer, "\t\"%v\":  \"%v\", %s\n", key, value, reason)
	}
	fmt.Fprintf(writer, "}\n\n")
}

func DumpIntMapToGoVar(name string, m map[string]int, writer io.Writer) {
	fmt.Fprintf(writer, "var %v= map[string]int{\n", name)

	for key, value := range m {
		fmt.Fprintf(writer, "\t\"%v\":  %d,\n", key, value)
	}
	fmt.Fprintf(writer, "}\n\n")
}

func loadLegacyPypyPackages(filePath string) map[string]LegacyPackageInfo {
	injson, err := os.Open(filePath)
	if err != nil {
		return make(map[string]LegacyPackageInfo)
	}
	infoMap := make(map[string]LegacyPackageInfo)

	dec := json.NewDecoder(injson)
	for dec.More() {
		var info LegacyPackageInfo

		err = dec.Decode(&info)
		if err != nil {
			continue
		}
		info.Pkg = strings.ToLower(info.Pkg)
		infoMap[info.Pkg] = info
	}

	return infoMap
}
