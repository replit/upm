package main

import (
	"bufio"
	"fmt"
	"io"
	"net/http"
	"path"
	"sort"
)

// These modules are always ignored
var ignoredModules = map[string]bool{
	"test":          true,
	"tests":         true,
	"testing":       true,
	"doc":           true,
	"docs":          true,
	"documentation": true,
	"_private":      true,
	"setup":         true,
}

func parseTopLevel(reader io.Reader) []string {
	modules := make([]string, 0)

	scanner := bufio.NewScanner(reader)
	scanner.Split(bufio.ScanLines)
	for scanner.Scan() {
		modules = append(modules, scanner.Text())
	}

	return modules
}

func GetModules(metadata PackageData) ([]string, error) {
	fmt.Println("GetModules")
	latest := metadata.Releases[metadata.Info.Version]
	if len(latest) == 0 {
		return nil, PypiError{NoDistributions, metadata.Info.Version, nil}
	}

	// Sort the releases by priority we want to parse
	distPriorities := map[string]int{
		"bdist_wheel": 1,
		"sdist":       2,
	}

	sort.Slice(latest, func(a, b int) bool {
		return distPriorities[latest[a].PackageType] < distPriorities[latest[b].PackageType]
	})

	pkg := latest[0]

	// Download the distribution
	resp, err := http.Get(pkg.URL)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	// Get the right archive reader
	var reader ArchiveReader
	if path.Ext(pkg.Filename) == ".zip" ||
		path.Ext(pkg.Filename) == ".whl" ||
		path.Ext(pkg.Filename) == ".egg" {
		reader, err = MakeZipReader(resp.Body, pkg.Size)
		if err != nil {
			return nil, err
		}
	} else if path.Ext(pkg.Filename) == ".gz" {
		reader, err = MakeTarballReader(resp.Body)
		if err != nil {
			return nil, err
		}
	} else {
		return nil, PypiError{UnknownArchive, path.Ext(pkg.Filename), nil}
	}
	defer reader.Close()

	var modules []string
	switch pkg.PackageType {
	case "bdist_wheel":
		modules, err = ExtractBdist(reader)
	case "bdist_egg":
		modules, err = ExtractBdist(reader)
	case "bdist_dumb":
		modules, err = ExtractBdist(reader)
	case "sdist":
		modules, err = ExtractSdist(reader)
	default:
		return nil, PypiError{UnknownDist, pkg.PackageType, nil}
	}

	if err != nil {
		return nil, err
	}

	// Filter out modules that aren't useful
	ret := make([]string, 0, len(modules))
	for _, m := range modules {
		// Only add top level modules, sub modules aren't useful for guessing
		if path.Dir(m) != "." {
			continue
		}

		// Skip modules that are common in many packages
		if _, hit := ignoredModules[path.Base(m)]; hit {
			continue
		}

		ret = append(ret, m)
	}

	return ret, nil
}
