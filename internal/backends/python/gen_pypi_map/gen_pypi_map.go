package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"os"
)

func main() {
	command := flag.String("cmd", "", "A command to perform. One of bq, test, and gen.")
	gcp := flag.String("gcp", "", "A GCP project ID to use to query bigquery directly. The result will be written to bq.")
	bq := flag.String("bq", "download_stats.json", "The result of a BigQuery against the pypi downloads dataset.")
	cache := flag.String("cache", "cache", "A directory where to store cached information for each module.")
	index := flag.String("index", "", "An json index file for packages containing an array of strings")
	workers := flag.Int("workers", 16, "The number of simultaenous workers to run")
	distMods := flag.Bool("dist", false, "Determine modules by examining dists")
	force := flag.Bool("force", false, "Force re-test when cached")
	pkg := flag.String("pkg", "python", "the pkg name for the output source")
	out := flag.String("out", "pypi_map.gen.go", "the destination file for the generated code")
	flag.Parse()

	if *command == "bq" {
		/*
			Fetch download stats from pypi's public big query table
			Parameters: gcp, bq
		*/
		err := fetchBQDownloads(*gcp, *bq)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Failed to fetch BQ download stats: %s", err.Error())
			return
		}
	} else if *command == "test" {
		/*
			Test packages to find out the list of modules each one provides
			Parameters: cache, index, workers, distMods
		*/
		var packages PackageIndex
		if *index != "" {
			file, err := os.Open(*index)
			if err != nil {
				fmt.Fprintf(os.Stderr, "Failed to open file %s: %s\n", *index, err.Error())
				return
			}
			var packageList []string
			decoder := json.NewDecoder(file)
			err = decoder.Decode(&packageList)
			if err != nil {
				fmt.Fprintf(os.Stderr, "Failed to decode file %s: %s\n", *index, err.Error())
				return
			}
			defer file.Close()
			packages = FakePackageIndex(packageList...)
		} else {
			packages, _ = NewPackageIndex("https://pypi.org/simple/", -1)
		}
		testModules(packages, *bq, *cache, *distMods, *workers, *force)
	} else if *command == "gen" {
		/*
			Generate source file that provides pypi mappings
			Parameters: pkg, out, cache, bq
		*/
		err := generateSource(*pkg, *out, *cache, *bq)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Failed to generate %s: %s", *out, err.Error())
		}
	} else {
		fmt.Fprintf(os.Stderr, "Please provide a cmd parameter")
	}
}
