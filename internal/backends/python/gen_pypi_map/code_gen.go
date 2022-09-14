package main

import (
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"strings"
)

type ModuleItem struct {
	Module      string
	PackageList []PackageInfo
}

func generateSource(pkg string, outputFilePath string, cacheDir string, bqFilePath string, pypiPackagesPath string) error {
	downloadStats, err := LoadDownloadStats(bqFilePath)
	if err != nil {
		return err
	}

	legacyPypiPackages := loadLegacyPypyPackages(pypiPackagesPath)

	// Map every module to a list of packages that can provide it
	files, err := ioutil.ReadDir(cacheDir)
	if err != nil {
		return err
	}

	packagesProcessed := make(map[string]bool)
	var moduleToPackageList = map[string][]PackageInfo{}

	for _, file := range files {
		var info PackageInfo
		filePath := cacheDir + "/" + file.Name()
		err := loadPackageInfoFile(filePath, &info)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Warning: failed to load %s: %s\n", filePath, err.Error())
			continue
		}
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

	// sort.Slice(moduleList, func(i, j int) bool {
	// 	return len(moduleList[i].PackageList) < len(moduleList[j].PackageList)
	// })

	// fmt.Printf("Sorted modules by package list size\n")

	var moduleToPypiPackage = map[string]string{}
	var moduleToPypiPackageReason = map[string]string{}
	var pypiPackageToModules = map[string]string{}

	// Guess at every module, add the guess and the package that was guessed to
	// the masp
	for moduleName, candidates := range moduleToPackageList {
		// // Filter out packages that have already been matched
		// // We start with ones that have the fewest modules, so it will
		// // match the atomic packages first
		// for _, pkg := range module.PackageList {
		// 	_, ok := pypiPackageToModules[pkg.Name]
		// 	if !ok {
		// 		candidates = append(candidates, pkg)
		// 	}
		// }
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
