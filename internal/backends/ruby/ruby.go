package ruby

import (
	"encoding/json"
	"strings"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

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

const rubySearchCode = `
require 'gems'
require 'json'

puts Gems.search(ARGV[0]).to_json
`

const rubyInfoCode = `
require 'gems'
require 'json'

puts Gems.info(ARGV[0]).to_json
`

const rubyListSpecfileCode = `
require 'bundler'
require 'json'

dsl = Bundler::Dsl.new

result = {}
dsl.eval_gemfile("Gemfile").each do |dep|
  result[dep.name] = dep.requirement.requirements.map{ |req| req.join(" ") }.join(", ")
end

puts result.to_json
`

const rubyListLockfileCode = `
require 'bundler'
require 'json'

# https://stackoverflow.com/a/40098825/3538165
lockfile = Bundler::LockfileParser.new(Bundler.read_file(Bundler.default_lockfile))

result = {}
lockfile.specs.each do |spec|
  result[spec.name] = spec.version
end

puts result.to_json
`

var RubyBackend = api.LanguageBackend{
	Name:             "ruby-bundler",
	Specfile:         "Gemfile",
	Lockfile:         "Gemfile.lock",
	FilenamePatterns: []string{"*.rb"},
	Quirks:           api.QuirksAddRemoveAlsoInstalls,
	Search: func(query string) []api.PkgInfo {
		outputB := util.GetCmdOutput([]string{
			"ruby", "-e", rubySearchCode, query,
		})
		var outputStructs []rubygemsInfo
		if err := json.Unmarshal(outputB, &outputStructs); err != nil {
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
		outputB := util.GetCmdOutput([]string{
			"ruby", "-e", rubyInfoCode, string(name),
		})
		var s rubygemsInfo
		if err := json.Unmarshal(outputB, &s); err != nil {
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
		if !util.FileExists("Gemfile") {
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
		cmd := []string{"bundle", "remove", "--install"}
		for name, _ := range pkgs {
			cmd = append(cmd, string(name))
		}
		util.RunCmd(cmd)
	},
	Lock: func() {
		util.RunCmd([]string{"bundle", "lock"})
	},
	Install: func() {
		// Don't install gems system-globally, unless the user
		// has already created a config file and presumably
		// knows what they are doing.
		if !util.FileExists(".bundle/config") {
			util.RunCmd([]string{"bundle", "config", "path", "vendor/bundle"})
		}
		// We need --clean to handle uninstalls.
		util.RunCmd([]string{"bundle", "install", "--clean"})
	},
	ListSpecfile: func() map[api.PkgName]api.PkgSpec {
		outputB := util.GetCmdOutput([]string{
			"ruby", "-e", rubyListSpecfileCode,
		})
		results := map[api.PkgName]api.PkgSpec{}
		if err := json.Unmarshal(outputB, &results); err != nil {
			util.Die("ruby: %s", err)
		}
		return results
	},
	ListLockfile: func() map[api.PkgName]api.PkgVersion {
		outputB := util.GetCmdOutput([]string{
			"ruby", "-e", rubyListLockfileCode,
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
