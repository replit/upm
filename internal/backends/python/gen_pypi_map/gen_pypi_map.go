package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"sort"
	"strings"
)

/*

This CLI program operates with one of 3 commands:

* bq - fetch pypi download stats
* test - test modules on pypi and save the results (1 file per package) in the cache directory
* gen - generate pypi_map.gen.go file which contains 2 mappings used for package guessing by upm
* updatepkgs - update the pkgs.json file
*/

func cmd_bq(args []string) {
	/*
	   Fetch download stats from pypi's public big query table
	   Parameters: gcp, bq
	*/
	bqCommandSet := flag.NewFlagSet("bq-flags", flag.ExitOnError)
	bqGCP := bqCommandSet.String("gcp", "", "A GCP project ID to use to query bigquery directly.")
	bqBQ := bqCommandSet.String("bq", "download_stats.json", "The result of a BigQuery against the pypi downloads dataset.")
	if err := bqCommandSet.Parse(args); err != nil {
		fmt.Fprintf(os.Stderr, "Failed to parse bq flags: %s\n", err)
		return
	}
	if *bqGCP == "" {
		fmt.Fprintln(os.Stderr, "Error: The 'gcp' flag must not be empty.")
		return
	}
	err := FetchBQDownloads(*bqGCP, *bqBQ)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to fetch BQ download stats: %s\n", err.Error())
		return
	}
}

func cmd_test(args []string) {
	/*
		Test packages to find out the list of modules each one provides
		Parameters: cache, index, workers, distMods
	*/

	testCommandSet := flag.NewFlagSet("test-flags", flag.ExitOnError)
	testCache := testCommandSet.String("cache", "cache", "A directory where to store temporary cached information for each module.")
	testIndex := testCommandSet.String("index", "", "A json index file for packages containing an array of strings")
	testWorkers := testCommandSet.Int("workers", 16, "The number of simultaneous workers to run")
	testDistMods := testCommandSet.Bool("distMods", false, "Determine modules by examining dists")
	testBQ := testCommandSet.String("bq", "download_stats.json", "The result of a BigQuery against the pypi downloads dataset.")
	testForce := testCommandSet.Bool("force", false, "Force re-test when cached")
	testPkgsFile := testCommandSet.String("pkgsfile", "pkgs.json", "A file where to store permanent information for each module.")
	if err := testCommandSet.Parse(args); err != nil {
		fmt.Fprintf(os.Stderr, "Failed to parse test flags: %s\n", err)
		return
	}

	var packages PackageIndex
	if *testIndex != "" {
		file, err := os.Open(*testIndex)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Failed to open file %s: %s\n", *testIndex, err.Error())
			return
		}
		var packageList []string
		decoder := json.NewDecoder(file)
		err = decoder.Decode(&packageList)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Failed to decode file %s: %s\n", *testIndex, err.Error())
			return
		}
		defer file.Close()
		packages = FakePackageIndex(packageList...)
	} else {
		packages, _ = NewPackageIndex("https://pypi.org/simple/", -1)
	}
	TestModules(packages, *testBQ, *testCache, *testPkgsFile, *testDistMods, *testWorkers, *testForce)
}

func cmd_gen(args []string) {
	/*
		Generate source file that provides pypi mappings
		Parameters: pkg, out, cachedfr, cachefile, bq, pypipackages
	*/
	genCommandSet := flag.NewFlagSet("gen-flags", flag.ExitOnError)
	genPkg := genCommandSet.String("pkg", "python", "the pkg name for the output source")
	genOut := genCommandSet.String("out", "pypi_map.sqlite", "the destination file for the generated data")
	genCache := genCommandSet.String("cache", "cache", "A directory where to store temporary cached information for each module.")
	genPkgsFile := genCommandSet.String("pkgsfile", "pkgs.json", "A file where to store permanent information for each module.")
	genPkgsLegacyFile := genCommandSet.String("legacypkgsfile", "pypi_packages.json", "Legacy dependencies information for each module - used as a fallback")
	genBQ := genCommandSet.String("bq", "download_stats.json", "The result of a BigQuery against the pypi downloads dataset.")
	if err := genCommandSet.Parse(args); err != nil {
		fmt.Fprintf(os.Stderr, "Failed to parse gen flags: %s\n", err)
		return
	}

	cache := LoadAllPackageInfo(*genCache, *genPkgsFile)
	err := GenerateDB(*genPkg, *genOut, cache, *genBQ, *genPkgsLegacyFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to generate %s: %s\n", *genOut, err.Error())
	}
}

func cmd_updatepkgs(args []string) {
	/*
		Update the pkgs.json file with the latest package information
		Parameters: cache, pkgsfile
	*/
	updateCommandSet := flag.NewFlagSet("update-flags", flag.ExitOnError)
	updateCache := updateCommandSet.String("cache", "cache", "A directory where to store temporary cached information for each module.")
	updatePkgsFile := updateCommandSet.String("pkgsfile", "pkgs.json", "A file where to store permanent information for each module.")
	if err := updateCommandSet.Parse(args); err != nil {
		fmt.Fprintf(os.Stderr, "Failed to parse update flags: %s\n", err)
		return
	}
	err := UpdateAllPackageInfo(*updateCache, *updatePkgsFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed update cache: %s\n", err.Error())
	}
}

func main() {
	command := ""
	if len(os.Args) > 1 {
		command = os.Args[1]
	}
	validCmds := map[string]func([]string){
		"bq":         cmd_bq,
		"test":       cmd_test,
		"gen":        cmd_gen,
		"updatepkgs": cmd_updatepkgs,
	}
	if cmd, ok := validCmds[command]; ok {
		cmd(os.Args[2:])
	} else if command != "" {
		choices := make([]string, 0, len(validCmds))
		for cmd := range validCmds {
			choices = append(choices, cmd)
		}
		sort.Strings(choices)
		fmt.Fprintf(os.Stderr, "Error: Invalid command '%s'.\nValid commands are %s.\n", command, strings.Join(choices, ", "))
	} else {
		choices := make([]string, 0, len(validCmds))
		for cmd := range validCmds {
			choices = append(choices, cmd)
		}
		sort.Strings(choices)
		fmt.Fprintf(os.Stderr, "Error: No command provided.\nValid commands are %s.\n", strings.Join(choices, ", "))
	}
}
