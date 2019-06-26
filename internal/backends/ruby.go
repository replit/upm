package backends

import (
	"encoding/json"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

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

var rubyBackend = api.LanguageBackend{
	Name:             "ruby-bundler",
	Specfile:         "Gemfile",
	Lockfile:         "Gemfile.lock",
	FilenamePatterns: []string{"*.rb"},
	Quirks:           api.QuirksNone,
	Search: func(query string) []api.PkgInfo {
		util.NotImplemented()
		return nil
	},
	Info: func(name api.PkgName) *api.PkgInfo {
		util.NotImplemented()
		return nil
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
			cmd := append([]string{"bundle", "add"}, args...)
			// This argument *has* to come after the gem
			// name, otherwise it will be silently ignored
			// (thanks Bundler).
			cmd = append(cmd, "--skip-install")
			util.RunCmd(cmd)
		}
		for name, spec := range pkgs {
			if spec != "" {
				nameArg := string(name)
				versionArg := "--version=" + string(spec)
				util.RunCmd([]string{"bundle", "add", nameArg, versionArg, "--skip-install"})
			}
		}
	},
	Remove: func(pkgs map[api.PkgName]bool) {
		cmd := []string{"bundle", "remove"}
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
