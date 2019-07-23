// Package ruby provides a backend for Ruby using Bundler.
package ruby

import (
	"encoding/json"
	"io/ioutil"
	"net/http"
	"net/url"
	"strings"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

// rubygemsInfo represents the information we get from Gems.search (a
// list of maps) or Gems.info (just one map) in JSON format.
type rubygemsInfo struct {
	Authors       string `json:"authors"`
	BugTrackerURI string `json:"bug_tracker_uri"`
	Dependencies  map[string][]struct {
		Name         string `json:"name"`
		Requirements string `json:"requirements"`
	} `json:"dependencies"`
	DocumentationURI string   `json:"documentation_uri"`
	HomepageURI      string   `json:"homepage_uri"`
	Info             string   `json:"info"`
	Licenses         []string `json:"licenses"`
	Name             string   `json:"name"`
	SourceCodeURI    string   `json:"source_code_uri"`
	Version          string   `json:"version"`
}

// RubyBackend is a UPM language backend for Ruby using Bundler.
var RubyBackend = api.LanguageBackend{
	Name:             "ruby-bundler",
	Specfile:         "Gemfile",
	Lockfile:         "Gemfile.lock",
	FilenamePatterns: []string{"*.rb"},
	Quirks:           api.QuirksAddRemoveAlsoInstalls,
	Search: func(query string) []api.PkgInfo {
		endpoint := "https://rubygems.org/api/v1/search.json"
		queryParams := "?query=" + url.QueryEscape(query)

		resp, err := http.Get(endpoint + queryParams)
		if err != nil {
			util.Die("RubyGems: %s", err)
		}
		defer resp.Body.Close()

		body, err := ioutil.ReadAll(resp.Body)
		if err != nil {
			util.Die("RubyGems: %s", err)
		}

		var outputStructs []rubygemsInfo
		if err := json.Unmarshal(body, &outputStructs); err != nil {
			util.Die("RubyGems response: %s", err)
		}

		results := []api.PkgInfo{}
		for _, s := range outputStructs {
			deps := []string{}
			for _, group := range s.Dependencies {
				for _, dep := range group {
					deps = append(deps, dep.Name)
				}
			}
			results = append(results, api.PkgInfo{
				Name:             s.Name,
				Description:      s.Info,
				Version:          s.Version,
				HomepageURL:      s.HomepageURI,
				DocumentationURL: s.DocumentationURI,
				SourceCodeURL:    s.SourceCodeURI,
				BugTrackerURL:    s.BugTrackerURI,
				Author:           s.Authors,
				License:          strings.Join(s.Licenses, ", "),
				Dependencies:     deps,
			})
		}
		return results
	},
	Info: func(name api.PkgName) api.PkgInfo {
		endpoint := "https://rubygems.org/api/v1/gems/"
		path := url.QueryEscape(string(name)) + ".json"

		resp, err := http.Get(endpoint + path)
		if err != nil {
			util.Die("RubyGems: %s", err)
		}
		defer resp.Body.Close()

		switch resp.StatusCode {
		case 200:
			break
		case 404:
			return api.PkgInfo{}
		default:
			util.Die("RubyGems: HTTP status %d", resp.StatusCode)
		}

		body, err := ioutil.ReadAll(resp.Body)
		if err != nil {
			util.Die("RubyGems: %s", err)
		}

		var s rubygemsInfo
		if err := json.Unmarshal(body, &s); err != nil {
			util.Die("RubyGems response: %s", err)
		}
		deps := []string{}
		for _, group := range s.Dependencies {
			for _, dep := range group {
				deps = append(deps, dep.Name)
			}
		}
		return api.PkgInfo{
			Name:             s.Name,
			Description:      s.Info,
			Version:          s.Version,
			HomepageURL:      s.HomepageURI,
			DocumentationURL: s.DocumentationURI,
			SourceCodeURL:    s.SourceCodeURI,
			BugTrackerURL:    s.BugTrackerURI,
			Author:           s.Authors,
			License:          strings.Join(s.Licenses, ", "),
			Dependencies:     deps,
		}
	},
	Add: func(pkgs map[api.PkgName]api.PkgSpec) {
		if !util.Exists(".bundle/config") {
			util.RunCmd([]string{"bundle", "config", "path", "vendor/bundle"})
		}
		if !util.Exists("Gemfile") {
			util.RunCmd([]string{"bundle", "init"})
		}
		args := []string{}
		for name, spec := range pkgs {
			if spec == "" {
				args = append(args, string(name))
			}
		}
		if len(args) > 0 {
			util.RunCmd(append([]string{"bundle", "add"}, args...))
		}
		for name, spec := range pkgs {
			if spec != "" {
				nameArg := string(name)
				versionArg := "--version=" + string(spec)
				util.RunCmd([]string{"bundle", "add", nameArg, versionArg})
			}
		}
	},
	Remove: func(pkgs map[api.PkgName]bool) {
		if !util.Exists(".bundle/config") {
			util.RunCmd([]string{"bundle", "config", "path", "vendor/bundle"})
		}
		cmd := []string{"bundle", "remove", "--install"}
		for name, _ := range pkgs {
			cmd = append(cmd, string(name))
		}
		util.RunCmd(cmd)
	},
	Lock: func() {
		if !util.Exists(".bundle/config") {
			util.RunCmd([]string{"bundle", "config", "path", "vendor/bundle"})
		}
		util.RunCmd([]string{"bundle", "lock"})
	},
	Install: func() {
		// Don't install gems system-globally, unless the user
		// has already created a config file and presumably
		// knows what they are doing.
		if !util.Exists(".bundle/config") {
			util.RunCmd([]string{"bundle", "config", "path", "vendor/bundle"})
		}
		// We need --clean to handle uninstalls.
		util.RunCmd([]string{"bundle", "install", "--clean"})
	},
	ListSpecfile: func() map[api.PkgName]api.PkgSpec {
		outputB := util.GetCmdOutput([]string{
			"ruby", "-e", util.GetResource("/ruby/list-specfile.rb"),
		})
		results := map[api.PkgName]api.PkgSpec{}
		if err := json.Unmarshal(outputB, &results); err != nil {
			util.Die("ruby: %s", err)
		}
		return results
	},
	ListLockfile: func() map[api.PkgName]api.PkgVersion {
		outputB := util.GetCmdOutput([]string{
			"ruby", "-e", util.GetResource("/ruby/list-lockfile.rb"),
		})
		results := map[api.PkgName]api.PkgVersion{}
		if err := json.Unmarshal(outputB, &results); err != nil {
			util.Die("ruby: %s", err)
		}
		return results
	},
	Guess: func() map[api.PkgName]bool {
		util.NotImplemented()
		return nil
	},
}
