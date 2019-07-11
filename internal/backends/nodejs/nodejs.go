package nodejs

import (
	"encoding/json"
	"io/ioutil"
	"net/http"
	"net/url"
	"regexp"
	"strings"

	"github.com/hashicorp/go-version"
	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

// https://github.com/npm/registry/blob/5db1bb329f554454467531a3e1bae5e97da160df/docs/REGISTRY-API.md
type npmSearchResults struct {
	Objects []struct {
		Package struct {
			Name        string `json:"name"`
			Version     string `json:"version"`
			Description string `json:"description"`
			Links       struct {
				Homepage   string `json:"homepage"`
				Repository string `json:"repository"`
				Bugs       string `json:"bugs"`
			} `json:"links"`
			Author struct {
				Username string `json:"username"`
				Email    string `json:"email"`
			} `json:"author"`
		} `json:"package"`
	} `json:"objects"`
}

// https://github.com/npm/registry/blob/5db1bb329f554454467531a3e1bae5e97da160df/docs/responses/package-metadata.md
type npmInfoResult struct {
	Name     string                 `json:"name"`
	Versions map[string]interface{} `json:"versions"`
	Author   struct {
		Name  string `json:"name"`
		Email string `json:"email"`
		URL   string `json:"url"`
	} `json:"author"`
	Bugs struct {
		URL string `json:"url"`
	} `json:"bugs"`
	Description string `json:"description"`
	Homepage    string `json:"homepage"`
	License     string `json:"license"`
	Repository  struct {
		Type string `json:"type"`
		URL  string `json:"url"`
	} `json:"repository"`
}

type packageJSON struct {
	Dependencies    map[string]string `json:"dependencies"`
	DevDependencies map[string]string `json:"devDependencies"`
}

var nodejsPatterns = []string{"*.js", "*.ts", "*.jsx", "*.tsx"}

var NodejsBackend = api.LanguageBackend{
	Name:             "nodejs-yarn",
	Specfile:         "package.json",
	Lockfile:         "yarn.lock",
	FilenamePatterns: nodejsPatterns,
	Quirks: api.QuirksAddRemoveAlsoInstalls |
		api.QuirksLockAlsoInstalls,
	Search: func(query string) []api.PkgInfo {
		endpoint := "https://registry.npmjs.org/-/v1/search"
		query = "?text=" + url.QueryEscape(query)

		resp, err := http.Get(endpoint + query)
		if err != nil {
			util.Die("NPM registry: %s", err)
		}
		defer resp.Body.Close()

		body, err := ioutil.ReadAll(resp.Body)
		var npmResults npmSearchResults
		if err := json.Unmarshal(body, &npmResults); err != nil {
			util.Die("NPM registry: %s", err)
		}

		results := make([]api.PkgInfo, len(npmResults.Objects))
		for i := range npmResults.Objects {
			p := npmResults.Objects[i].Package
			results[i] = api.PkgInfo{
				Name:          p.Name,
				Description:   p.Description,
				Version:       p.Version,
				HomepageURL:   p.Links.Homepage,
				SourceCodeURL: p.Links.Repository,
				BugTrackerURL: p.Links.Bugs,
				Author: util.AuthorInfo{
					Name:  p.Author.Username,
					Email: p.Author.Email,
				}.String(),
			}
		}
		return results
	},
	Info: func(name api.PkgName) api.PkgInfo {
		endpoint := "https://registry.npmjs.org"
		path := "/" + url.QueryEscape(string(name))

		resp, err := http.Get(endpoint + path)
		if err != nil {
			util.Die("NPM registry: %s", err)
		}
		defer resp.Body.Close()

		body, err := ioutil.ReadAll(resp.Body)
		var npmInfo npmInfoResult
		if err := json.Unmarshal(body, &npmInfo); err != nil {
			util.Die("NPM registry: %s", err)
		}

		lastVersionStr := ""
		if len(npmInfo.Versions) > 0 {
			var lastVersion *version.Version = nil
			for versionStr := range npmInfo.Versions {
				version, err := version.NewVersion(versionStr)
				if err != nil {
					continue
				}

				if version.Prerelease() != "" {
					continue
				}

				if lastVersion == nil || version.GreaterThan(lastVersion) {
					lastVersion = version
				}
			}
			if lastVersion != nil {
				lastVersionStr = lastVersion.String()
			}
		}

		return api.PkgInfo{
			Name:          npmInfo.Name,
			Description:   npmInfo.Description,
			Version:       lastVersionStr,
			HomepageURL:   npmInfo.Homepage,
			SourceCodeURL: npmInfo.Repository.URL,
			BugTrackerURL: npmInfo.Bugs.URL,
			Author: util.AuthorInfo{
				Name:  npmInfo.Author.Name,
				Email: npmInfo.Author.Email,
				URL:   npmInfo.Author.URL,
			}.String(),
			License: npmInfo.License,
		}
	},
	Add: func(pkgs map[api.PkgName]api.PkgSpec) {
		cmd := []string{"yarn", "add"}
		for name, spec := range pkgs {
			cmd = append(cmd, string(name)+"@"+string(spec))
		}
		util.RunCmd(cmd)
	},
	Remove: func(pkgs map[api.PkgName]bool) {
		cmd := []string{"yarn", "remove"}
		for name, _ := range pkgs {
			cmd = append(cmd, string(name))
		}
		util.RunCmd(cmd)
	},
	Lock: func() {
		util.RunCmd([]string{"yarn", "upgrade"})
	},
	Install: func() {
		util.RunCmd([]string{"yarn", "install"})
	},
	ListSpecfile: func() map[api.PkgName]api.PkgSpec {
		contentsB, err := ioutil.ReadFile("package.json")
		if err != nil {
			util.Die("package.json: %s", err)
		}
		var cfg packageJSON
		if err := json.Unmarshal(contentsB, &cfg); err != nil {
			util.Die("package.json: %s", err)
		}
		pkgs := map[api.PkgName]api.PkgSpec{}
		for nameStr, specStr := range cfg.Dependencies {
			pkgs[api.PkgName(nameStr)] = api.PkgSpec(specStr)
		}
		for nameStr, specStr := range cfg.DevDependencies {
			pkgs[api.PkgName(nameStr)] = api.PkgSpec(specStr)
		}
		return pkgs
	},
	ListLockfile: func() map[api.PkgName]api.PkgVersion {
		contentsB, err := ioutil.ReadFile("yarn.lock")
		if err != nil {
			util.Die("yarn.lock: %s", err)
		}
		contents := string(contentsB)
		r := regexp.MustCompile(`(?m)^"?([^@ \n]+).+:\n  version "(.+)"$`)
		pkgs := map[api.PkgName]api.PkgVersion{}
		for _, match := range r.FindAllStringSubmatch(contents, -1) {
			name := api.PkgName(match[1])
			version := api.PkgVersion(match[2])
			pkgs[name] = version
		}
		return pkgs
	},
	Guess: func() map[api.PkgName]bool {
		// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/import
		r := regexp.MustCompile(strings.Join([]string{
			// import defaultExport from "module-name";
			// import * as name from "module-name";
			// import { export } from "module-name";
			`(?m)from\s*['"]([^'"]+)['"]\s*;?\s*$`,
			// import "module-name";
			`(?m)import\s*['"]([^'"]+)['"]\s*;?\s*$`,
			// const mod = import("module-name")
			// const mod = require("module-name")
			`(?m)(?:require|import)\s*\(\s*['"]([^'"{}]+)['"]\s*\)`,
		}, "|"))
		pkgs := map[api.PkgName]bool{}
		for _, match := range util.SearchRecursive(r, nodejsPatterns) {
			// Only one group should match, so this join
			// is really picking out whichever string is
			// non-empty in the slice.
			module := strings.Join(match[1:], "")
			// Skip local modules.
			if strings.Contains(module, "!") {
				parts := strings.Split(module, "!")
				module = parts[len(parts)-1]
			}
			if strings.HasPrefix(module, ".") {
				continue
			}
			if strings.Contains(module, "/") {
				parts := strings.Split(module, "/")
				if strings.HasPrefix(module, "@") {
					module = strings.Join(parts[:2], "/")
				} else {
					module = parts[0]
				}
			}
			pkgs[api.PkgName(module)] = true
		}

		return pkgs
	},
}
