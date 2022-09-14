// Package rust provides a backend for Rust using Cargo.
package rust

import (
	"encoding/json"
	"io/ioutil"
	"net/http"
	"net/url"

	"github.com/BurntSushi/toml"
	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

type cargoToml struct {
	Dependencies map[string]interface{} `toml:"dependencies"`
}

type cargoLock struct {
	Packages []cargoPackage `toml:"package"`
}

type cargoPackage struct {
	Name    string `toml:"name"`
	Version string `toml:"version"`
}

type crateSearchResults struct {
	Crates []crate `json:"crates"`
}

type crateInfoResult struct {
	Crate    crate     `json:"crate"`
	Versions []version `json:"versions"`
}

type crate struct {
	Name          string `json:"name"`
	Description   string `json:"description"`
	Homepage      string `json:"homepage"`
	Documentation string `json:"documentation"`
	Repository    string `json:"repository"`
	NewestVersion string `json:"newest_version"`
	Versions      []int  `json:"versions"`
}

type version struct {
	Num         string `json:"num"`
	PublishedBy user   `json:"published_by"`
	License     string `json:"license"`
}

type user struct {
	Name string `json:"name"`
}

func (c *crateInfoResult) toPkgInfo() api.PkgInfo {
	var author string
	var license string

	for _, version := range c.Versions {
		if version.Num == c.Crate.NewestVersion {
			author = version.PublishedBy.Name
			license = version.License
			break
		}
	}

	return api.PkgInfo{
		Name:             c.Crate.Name,
		Description:      c.Crate.Description,
		Version:          c.Crate.NewestVersion,
		HomepageURL:      c.Crate.Homepage,
		DocumentationURL: c.Crate.Documentation,
		SourceCodeURL:    c.Crate.Repository,
		Author:           author,
		License:          license,
	}
}

func search(query string) []api.PkgInfo {
	endpoint := "https://crates.io/api/v1/crates"
	path := "?q=" + url.QueryEscape(query)

	resp, err := http.Get(endpoint + path)
	if err != nil {
		util.Die("crates.io: %s", err)
	}
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		util.Die("crates.io: %s", err)
	}

	var crateResults crateSearchResults
	if err := json.Unmarshal(body, &crateResults); err != nil {
		util.Die("crates.io: %s", err)
	}

	var pkgs []api.PkgInfo
	for _, crate := range crateResults.Crates {
		crateInfo := crateInfoResult{
			Crate: crate,
		}
		pkgs = append(pkgs, crateInfo.toPkgInfo())
	}

	return pkgs
}

func info(name api.PkgName) api.PkgInfo {
	endpoint := "https://crates.io/api/v1/crates"
	path := "/" + url.PathEscape(string(name))

	resp, err := http.Get(endpoint + path)
	if err != nil {
		util.Die("crates.io: %s", err)
	}
	defer resp.Body.Close()

	switch resp.StatusCode {
	case 200:
		break
	case 404:
		return api.PkgInfo{}
	default:
		util.Die("crates.io: HTTP status %d", resp.StatusCode)
	}

	body, err := ioutil.ReadAll(resp.Body)
	var crateInfo crateInfoResult
	if err := json.Unmarshal(body, &crateInfo); err != nil {
		util.Die("crates.io: %s", err)
	}

	return crateInfo.toPkgInfo()
}

func listSpecfile() map[api.PkgName]api.PkgSpec {
	contents, err := ioutil.ReadFile("Cargo.toml")
	if err != nil {
		util.Die("Cargo.toml: %s", err)
	}

	return listSpecfileWithContents(contents)
}

func listSpecfileWithContents(contents []byte) map[api.PkgName]api.PkgSpec {
	var specfile cargoToml
	err := toml.Unmarshal(contents, &specfile)
	if err != nil {
		util.Die("Cargo.toml: %s", err)
	}

	packages := make(map[api.PkgName]api.PkgSpec)
	for name, dependency := range specfile.Dependencies {
		var spec api.PkgSpec
		switch value := dependency.(type) {
		case string:
			spec = api.PkgSpec(value)

		case map[string]interface{}:
			found := false
			for _, key := range []string{"version", "git", "path"} {
				specStr, ok := value[key].(string)
				if !ok {
					continue
				}

				spec = api.PkgSpec(specStr)
				found = true
				break
			}

			if !found {
				util.Die("Cargo.toml: could not determine spec for dependecy %q", name)
			}

		default:
			util.Die("Cargo.toml: unexpected dependency format %q", name)
		}

		packages[api.PkgName(name)] = spec

	}

	return packages
}

func listLockfile() map[api.PkgName]api.PkgVersion {
	contents, err := ioutil.ReadFile("Cargo.lock")
	if err != nil {
		util.Die("Cargo.lock: %s", err)
	}

	return listLockfileWithContents(contents)
}

func listLockfileWithContents(contents []byte) map[api.PkgName]api.PkgVersion {
	var lockfile cargoLock
	err := toml.Unmarshal(contents, &lockfile)
	if err != nil {
		util.Die("Cargo.lock: %s", err)
	}

	packages := make(map[api.PkgName]api.PkgVersion)
	for _, pkg := range lockfile.Packages {
		packages[api.PkgName(pkg.Name)] = api.PkgVersion(pkg.Version)
	}

	return packages
}

// RustBackend is a UPM backend for Rust that uses Cargo.
var RustBackend = api.LanguageBackend{
	Name:             "rust",
	Specfile:         "Cargo.toml",
	Lockfile:         "Cargo.lock",
	FilenamePatterns: []string{"*.rs"},
	GetPackageDir: func() string {
		return "target"
	},
	Search: search,
	Info:   info,
	Add: func(pkgs map[api.PkgName]api.PkgSpec, projectName string) {
		if !util.Exists("Cargo.toml") {
			util.RunCmd([]string{"cargo", "init", "."})
		}
		cmd := []string{"cargo", "add"}
		for name, spec := range pkgs {
			arg := string(name)
			if spec != "" {
				arg += "@" + string(spec)
			}
			cmd = append(cmd, arg)
		}
		util.RunCmd(cmd)
	},
	Remove: func(pkgs map[api.PkgName]bool) {
		cmd := []string{"cargo", "rm"}
		for name := range pkgs {
			cmd = append(cmd, string(name))
		}
		util.RunCmd(cmd)
	},
	Lock: func() {
		// Lock file is updated at build time
	},
	Install: func() {
		// Dependencies are installed at build time
	},
	ListSpecfile: listSpecfile,
	ListLockfile: listLockfile,
	Guess: func() (map[api.PkgName]bool, bool) {
		util.NotImplemented()
		return nil, false
	},
}
