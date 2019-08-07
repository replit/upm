// Package ruby provides a backend for Ruby using Bundler.
package ruby

import (
	"encoding/json"
	"io/ioutil"
	"net/http"
	"net/url"
	"os"
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

// getPath returns the appropriate --path for 'bundle install'. This
// will normally be '.bundle' (in the current directory), but may
// instead be the empty string, indicating that no --path argument
// should be passed. (This is for the case where the user has
// explicitly configured a different path.)
func getPath() string {
	tempdir := util.TempDir()
	defer os.RemoveAll(tempdir)

	// The --parseable option is completely undocumented outside
	// of the source code, thanks Bundler.
	outputB := util.GetCmdOutput([]string{
		"bundle", "config", "--parseable", "path"})

	if len(outputB) == 0 {
		// Nothing configured, use our default.
		return ".bundle"
	} else {
		// If on the other hand there *is* something
		// configured, we'll return the empty string to
		// indicate that a --path argument should *not* be
		// used, which will allow Bundler to use the default
		// we just looked up. (In fact, we *must* refrain from
		// passing --path in this case; otherwise a
		// superfluous .bundle/config file might get created.)
		return ""
	}
}

// RubyBackend is a UPM language backend for Ruby using Bundler.
var RubyBackend = api.LanguageBackend{
	Name:             "ruby-bundler",
	Specfile:         "Gemfile",
	Lockfile:         "Gemfile.lock",
	FilenamePatterns: []string{"*.rb"},
	Quirks:           api.QuirksAddRemoveAlsoLocks,
	GetPackageDir: func() string {
		path := string(util.GetCmdOutput([]string{
			"bundle", "config", "--parseable", "path"}))
		path = strings.TrimSuffix(path, "\n")
		path = strings.TrimPrefix(path, "path=")
		if path == "" {
			return ".bundle"
		} else {
			return path
		}
	},
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
			// We need to --skip-install here and run that
			// separately, because there's no way to get
			// Bundler to --clean when installing via add.
			util.RunCmd(append([]string{
				"bundle", "add", "--skip-install"}, args...))
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
		cmd := []string{"bundle", "remove", "--skip-install"}
		for name, _ := range pkgs {
			cmd = append(cmd, string(name))
		}
		util.RunCmd(cmd)
	},
	Lock: func() {
		util.RunCmd([]string{"bundle", "lock"})
	},
	Install: func() {
		// We need --clean to handle uninstalls.
		args := []string{"bundle", "install", "--clean"}
		if path := getPath(); path != "" {
			args = append(args, "--path", path)
		}
		util.RunCmd(args)
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
	Guess: func() (map[api.PkgName]bool, bool) {
		util.NotImplemented()
		panic("unreachable code")
	},
}
