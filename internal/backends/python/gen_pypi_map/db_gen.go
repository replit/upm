package main

import (
	"database/sql"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"strings"

	_ "github.com/mattn/go-sqlite3"
)

func GenerateDB(pkg string, outputFilePath string, cache map[string]PackageInfo, bqFilePath string, pkgsLegacyFile string) error {
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

	err = os.Remove(outputFilePath)
	if err != nil && !errors.Is(err, os.ErrNotExist) {
		return err
	}
	db, err := sql.Open("sqlite3", outputFilePath)
	if err != nil {
		return err
	}
	_, err = db.Exec(`
	create table module_to_pypi_package (module_name text primary key, guess text, reason text);
	create table pypi_packages (package_name text primary key, module_list text, downloads int);
	create index downloads_index on pypi_packages (downloads);
	`)
	if err != nil {
		return err
	}

	// Write all data within one transaction for speed
	// https://stackoverflow.com/questions/1711631/improve-insert-per-second-performance-of-sqlite
	_, err = db.Exec(`begin transaction;`)
	if err != nil {
		return err
	}

	// Guess at every module, add the guess and the package that was guessed to
	// the masp
	for moduleName, candidates := range moduleToPackageList {
		if guess, reason, guessable := GuessPackage(moduleName, candidates, downloadStats); guessable {
			stmt, err := db.Prepare("insert into module_to_pypi_package values (?, ?, ?);")
			if err != nil {
				return err
			}
			_, err = stmt.Exec(moduleName, guess.Name, reason)
			if err != nil {
				return err
			}
			stmt.Close()

			stmt, err = db.Prepare(`
			insert into pypi_packages values (?, ?, ?)
			on conflict (package_name)
			do update set
				module_list = excluded.module_list,
				downloads = excluded.downloads;
			`)
			if err != nil {
				return err
			}
			download, ok := downloadStats[normalizePackageName(guess.Name)]
			if !ok {
				download = 0
			}
			_, err = stmt.Exec(guess.Name, strings.Join(guess.Modules, ","), download)
			if err != nil {
				return fmt.Errorf("%s on %s", err.Error(), guess.Name)
			}
			stmt.Close()
		}
	}

	_, err = db.Exec(`end transaction;`)
	if err != nil {
		return err
	}

	err = db.Close()
	if err != nil {
		return err
	}

	// Make it read only
	err = os.Chmod(outputFilePath, 0444)
	if err != nil {
		return err
	}

	fmt.Printf("Wrote %s\n", outputFilePath)
	return nil

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

func normalizePackageName(name string) string {
	nameStr := string(name)
	nameStr = strings.ToLower(nameStr)
	nameStr = strings.Replace(nameStr, "_", "-", -1)
	return nameStr
}
