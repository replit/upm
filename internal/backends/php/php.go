package php

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"net/http"
	"net/url"
	"strings"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

/*
 Used for composer.lock Parsing
*/
type composerLock struct {
	Packages []composerPackageLock `json:"packages"`
}

type composerPackageLock struct {
	Name    string `json:"name"`
	Version string `json:"version"`
}

/*
 Used for Package Search
*/
type packagistSearchResults struct {
	Packages []packagistSearchResult `json:"results"`
}

type packagistSearchResult struct {
	Name        string `json:"name"`
	Description string `json:"description"`
	Repository  string `json:"repository"`
}

/*
  Used for Package Info
*/
type packagistInfoSearchResult struct {
	PackageInfo map[string][]packageDetail `json:"packages"`
}

type packageDetail struct {
	Name          string            `json:"name"`
	Description   string            `json:"description"`
	LatestVersion string            `json:"version"`
	Licenses      []string          `json:"license"`
	Homepage      string            `json:"homepage"`
	Support       map[string]string `json:"support"`
	Authors       []authors         `json:"authors"`
}

type authors struct {
	Name  string `json:"name"`
	Email string `json:"email"`
}

func search(query string) []api.PkgInfo {
	endpoint := "https://packagist.org/search.json?q=" + url.QueryEscape(query)
	resp, err := http.Get(endpoint)

	if err != nil {
		util.Die("packagist search err: %s", err)
	}

	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		util.Die("packagist err: %s", err)
	}

	pkgInfo, err := parseSearch(body)

	if err != nil {
		util.Die("Error: %s", err)
	}

	return pkgInfo
}

func parseSearch(arr []byte) ([]api.PkgInfo, error) {
	var packagistResults packagistSearchResults

	if err := json.Unmarshal(arr, &packagistResults); err != nil {
		return nil, errors.New("Packagist error. Your query may be malformed")
	}

	pkgInfoArr := []api.PkgInfo{}
	for _, result := range packagistResults.Packages {
		pkgInfoArr = append(pkgInfoArr, api.PkgInfo{
			Name:          result.Name,
			Description:   result.Description,
			SourceCodeURL: result.Repository,
		})
	}

	return pkgInfoArr, nil
}

// This API can only accept strings in the [vendor]/[packageName] format
func info(name api.PkgName) api.PkgInfo {
	endpoint := fmt.Sprintf("https://repo.packagist.org/p2/%s.json", string(name))
	resp, err := http.Get(endpoint)

	if err != nil {
		util.Die("packagist err: %s", err)
	}

	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		util.Die("packagist err: %s", err)
	}

	pkgInfo, err := parseInfo(body, name)

	if err != nil {
		util.Die("Error: %s", err)
	}

	return pkgInfo

}

func parseInfo(body []byte, packageName api.PkgName) (api.PkgInfo, error) {
	var packagistInfo packagistInfoSearchResult
	if err := json.Unmarshal(body, &packagistInfo); err != nil {
		return api.PkgInfo{}, errors.New("Malformed Packagist response. Package may not exist")
	}

	if val, ok := packagistInfo.PackageInfo[string(packageName)]; ok {
		if len(val) == 0 {
			return api.PkgInfo{}, errors.New("Empty message from packagist")
		}
	} else {
		return api.PkgInfo{}, errors.New("Requested package was not in packagist response")
	}

	// latest version is always first
	latestVersionResult := packagistInfo.PackageInfo[string(packageName)][0]

	authors := []string{}
	for _, author := range latestVersionResult.Authors {
		authors = append(authors, author.Name)
	}

	bugTracker, _ := latestVersionResult.Support["issues"]

	return api.PkgInfo{
		Name:          latestVersionResult.Name,
		Description:   latestVersionResult.Description,
		Version:       latestVersionResult.LatestVersion,
		HomepageURL:   latestVersionResult.Homepage,
		License:       strings.Join(latestVersionResult.Licenses, ", "),
		Author:        strings.Join(authors, ", "),
		BugTrackerURL: bugTracker,
	}, nil
}

func listSpecfile() map[api.PkgName]api.PkgSpec {
	contents, err := ioutil.ReadFile("composer.json")

	if err != nil {
		util.Die("composer.json: %s", err)
	}

	return listSpecfileWithContents(contents)
}

func listSpecfileWithContents(contents []byte) map[api.PkgName]api.PkgSpec {
	var specfile struct {
		RequireDependencies    map[string]string `json:"require"`
		RequireDevDependencies map[string]string `json:"require-dev"`
	}

	if err := json.Unmarshal(contents, &specfile); err != nil {
		util.Die("composer.json: %s", err)
	}

	packages := make(map[api.PkgName]api.PkgSpec)

	for name, dependency := range specfile.RequireDependencies {
		packages[api.PkgName(name)] = api.PkgSpec(dependency)
	}

	for name, dependency := range specfile.RequireDevDependencies {
		packages[api.PkgName(name)] = api.PkgSpec(dependency)
	}

	return packages
}

func listLockfile() map[api.PkgName]api.PkgVersion {
	contents, err := ioutil.ReadFile("composer.lock")
	if err != nil {
		util.Die("composer.lock failure: %s", err)
	}
	return listLockfileWithContents(contents)
}

func listLockfileWithContents(contents []byte) map[api.PkgName]api.PkgVersion {
	var composerLock composerLock
	if err := json.Unmarshal(contents, &composerLock); err != nil {
		util.Die("composer.lock err: %s", err)
	}

	packages := make(map[api.PkgName]api.PkgVersion)

	for _, pkg := range composerLock.Packages {
		packages[api.PkgName(pkg.Name)] = api.PkgVersion(pkg.Version)
	}

	return packages
}

var PhpComposerBackend = api.LanguageBackend{
	Name:             "php-composer",
	Specfile:         "composer.json",
	Lockfile:         "composer.lock",
	FilenamePatterns: []string{"*.php"},
	Quirks:           api.QuirksAddRemoveAlsoLocks | api.QuirksAddRemoveAlsoInstalls,
	GetPackageDir: func() string {
		return "vendor"
	},
	Search: search,
	Info:   info,
	Add: func(pkgs map[api.PkgName]api.PkgSpec, projectVendorName string) {
		cmd := []string{"composer", "require"}

		for name, spec := range pkgs {
			arg := string(name)
			if spec != "" {
				arg += ":" + string(spec)
			}
			cmd = append(cmd, arg)
		}
		util.RunCmd(cmd)
	},
	Remove: func(pkgs map[api.PkgName]bool) {
		cmd := []string{"composer", "remove"}
		for name := range pkgs {
			cmd = append(cmd, string(name))
		}
		util.RunCmd(cmd)
	},
	Lock: func() {
		util.RunCmd([]string{"composer", "update"})
	},
	Install: func() {
		util.RunCmd([]string{"composer", "install"})
	},
	ListSpecfile: listSpecfile,
	ListLockfile: listLockfile,
	Guess: func() (map[api.PkgName]bool, bool) {
		util.NotImplemented()
		return nil, false
	},
}
