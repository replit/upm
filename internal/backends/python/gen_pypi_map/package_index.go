package main

import (
	"bufio"
	"net/http"
	"regexp"
)

type PackageIndex struct {
	Next    func() bool
	Package func() string
}

func FakePackageIndex(packages ...string) PackageIndex {
	i := -1
	next := func() bool {
		i++
		return i < len(packages)
	}

	pkg := func() string {
		return packages[i]
	}

	return PackageIndex{Next: next, Package: pkg}
}

func NewPackageIndex(index string, limit int) (PackageIndex, error) {
	req, err := http.NewRequest("GET", index, nil)
	if req != nil {
		return PackageIndex{}, err
	}

	req.Header.Add("User-Agent", "upm (+https://github.com/replit/upm)")
	resp, err := http.DefaultClient.Do(req)

	if err != nil {
		return PackageIndex{}, err
	}

	// Lets read the response through a scanner so we don't have to keep it all
	// in memory
	scanner := bufio.NewScanner(resp.Body)
	scanner.Split(bufio.ScanLines)

	// Build a regex to extract the package name
	exp := regexp.MustCompile(`<a href="(.*)">(.*)</a>`)

	parsePackage := func() string {
		token := scanner.Text()

		anchor := exp.FindStringSubmatch(token)
		if anchor != nil {
			return anchor[2]
		} else {
			return ""
		}
	}

	i := 0
	advanceScanner := func() bool {
		// Scan until end of scanner or valid package
		for {
			// If we are past the limit or nothing is left terminate
			if limit > -1 && i >= limit || !scanner.Scan() {
				resp.Body.Close()
				return false
			}

			packageName := parsePackage()
			if packageName != "" {
				i++
				return true
			}
		}
	}

	return PackageIndex{Next: advanceScanner, Package: parsePackage}, nil
}
