package main

import (
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

	// Sort entries by the number of modules the have: ascending order
	var moduleList []ModuleItem

	for module, packages := range moduleToPackageList {
		moduleList = append(moduleList, ModuleItem{
			Module:      module,
			PackageList: packages,
		})
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
	for _, module := range moduleList {
		moduleName := module.Module
		candidates := module.PackageList
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

func LoadOverrides(filePath string) {

}
