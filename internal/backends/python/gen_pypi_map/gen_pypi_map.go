package main

import (
	"bufio"
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"sort"
	"strings"
	"time"
)

/*

This CLI program provides the following commands, intended to be executed in order:

* bq         - fetch pypi download stats
* test       - test modules on pypi and save the results (1 file per package) in the cache directory
* updatepkgs - read from the cache directory and update the pkgs.json file
* gen        - read pkgs.json and generate pypi_map.sqlite file, containing mappings for package guessing

Additionally,
* test-one - run `test` for a single package
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
	testRemapFile := testCommandSet.String("remapfile", "remap.csv", "A file containing alterations for when a popular package name should be replaced with a newer version")
	testThreshold := testCommandSet.Int("threshold", 5000, "Only process packages with at least this many downloads")
	testTimeout := testCommandSet.Int("timeout", 60, "The maximum number of seconds to wait for a package to install.")
	if err := testCommandSet.Parse(args); err != nil {
		fmt.Fprintf(os.Stderr, "Failed to parse test flags: %s\n", err)
		return
	}

	var packages PackageIndex
	if testThreshold != nil {
		fmt.Printf("Loading pypi stats from cache file\n")
		bqCache, err := LoadDownloadStats(*testBQ)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Failed to load data from big query file %s: %v\n", *testBQ, err)
			return
		}
		fmt.Printf("Loaded %v stats\n", len(bqCache))
		normalizedBqCache := make(map[string]int)

		for name, count := range bqCache {
			normalizedBqCache[normalizePackageName(name)] = count
		}
		bqCache = normalizedBqCache

		packageRemaps := make(map[string]string)
		file, err := os.Open(*testRemapFile)

		if err == nil {
			scanner := bufio.NewScanner(file)
			for scanner.Scan() {
				columns := strings.SplitN(scanner.Text(), ",", 3)
				if len(columns) > 0 {
					old := columns[0]
					new := columns[1]
					// description := columns[2]
					packageRemaps[old] = new
				}
			}

			if err := scanner.Err(); err != nil {
				panic(err)
			}

			file.Close()
		}

		// Deduplicate results
		packageMap := make(map[string]bool)
		for pkgName, count := range bqCache {
			if count < *testThreshold {
				continue
			}
			// Apply package rename
			if newName, ok := packageRemaps[pkgName]; ok {
				pkgName = newName
			}
			packageMap[pkgName] = true
		}

		// Replit dependencies to explicitly include
		packageMap["replit"] = true
		packageMap["replit-ai"] = true
		packageMap["replit-code-exec"] = true
		packageMap["replit-river"] = true
		packageMap["replit.object-storage"] = true

		packageList := []string{}
		for pkgName := range packageMap {
			packageList = append(packageList, pkgName)
		}
		fmt.Printf("Preparing to process %v packages\n", len(packageList))
		packages = FakePackageIndex(packageList...)
	} else if *testIndex != "" {
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
	TestModules(packages, *testCache, *testPkgsFile, *testDistMods, *testWorkers, *testForce, time.Duration(*testTimeout)*time.Second)
}

func cmd_test_one(args []string) {
	/*
		Test a single package to find the list of modules provided
	*/

	testOneCommandSet := flag.NewFlagSet("test-one-flags", flag.ExitOnError)
	testOnePackage := testOneCommandSet.String("package", "", "Which package to test")
	testOneCache := testOneCommandSet.String("cache", "cache", "A directory where to store temporary cached information for each module.")
	testOneDistMods := testOneCommandSet.Bool("distMods", false, "Determine modules by examining dists")
	testOneForce := testOneCommandSet.Bool("force", false, "Force re-test when cached")
	testOnePkgsFile := testOneCommandSet.String("pkgsfile", "pkgs.json", "A file where to store permanent information for each module.")
	testOneTimeout := testOneCommandSet.Int("timeout", 60, "The maximum number of seconds to wait for a package to install.")
	if err := testOneCommandSet.Parse(args); err != nil {
		fmt.Fprintf(os.Stderr, "Failed to parse test flags: %s\n", err)
		return
	}
	if *testOnePackage == "" {
		fmt.Fprintf(os.Stderr, "Missing -package flag, cannot continue\n")
		return
	}

	cache := LoadAllPackageInfo(*testOneCache, *testOnePkgsFile)
	info, err := ProcessPackage(*testOnePackage, cache, *testOneCache, *testOneDistMods, *testOneForce, time.Duration(*testOneTimeout)*time.Second)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error processing package: %v\n", err)
		return
	}

	fmt.Printf("Name: %s\n", info.Name)
	fmt.Printf("Modules: %s\n", strings.Join(info.Modules, ", "))
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
	genBQ := genCommandSet.String("bq", "download_stats.json", "The result of a BigQuery against the pypi downloads dataset.")
	if err := genCommandSet.Parse(args); err != nil {
		fmt.Fprintf(os.Stderr, "Failed to parse gen flags: %s\n", err)
		return
	}

	cache := LoadAllPackageInfo(*genCache, *genPkgsFile)
	err := GenerateDB(*genPkg, *genOut, cache, *genBQ)
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
		"test-one":   cmd_test_one,
		"updatepkgs": cmd_updatepkgs,
		"gen":        cmd_gen,
	}
	if cmd, ok := validCmds[command]; ok {
		cmd(os.Args[2:])
	} else {
		var msg string
		if command != "" {
			msg = fmt.Sprintf("Invalid command '%s'.", command)
		} else {
			msg = "No command provided."
		}
		choices := make([]string, 0, len(validCmds))
		for cmd := range validCmds {
			choices = append(choices, cmd)
		}
		sort.Strings(choices)
		fmt.Fprintf(os.Stderr, "Error: %s\nValid commands are %s.\n", msg, strings.Join(choices, ", "))
	}
}
