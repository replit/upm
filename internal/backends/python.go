package backends

import (
	"encoding/json"
	"regexp"
	"strings"

	"github.com/BurntSushi/toml"
	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

type pypiXMLRPCEntry struct {
	Name    string `json:"name"`
	Summary string `json:"summary"`
	Version string `json:"version"`
}

type pypiXMLRPCInfo struct {
	Author       string   `json:"author"`
	AuthorEmail  string   `json:"author_email"`
	HomePage     string   `json:"home_page"`
	License      string   `json:"license"`
	Name         string   `json:"name"`
	ProjectURL   []string `json:"project_url"`
	RequiresDist []string `json:"requires_dist"`
	Summary      string   `json:"summary"`
	Version      string   `json:"version"`
}

type pyprojectTOML struct {
	Tool struct {
		Poetry struct {
			Dependencies    map[string]string `json:"dependencies"`
			DevDependencies map[string]string `json:"dev-dependencies"`
		} `json:"poetry"`
	} `json:"tool"`
}

type poetryLock struct {
	Package []struct {
		Name    string `json:"name"`
		Version string `json:"version"`
	} `json:"package"`
}

const pythonSearchCode = `
from __future__ import print_function
import json
import sys
try:
    from xmlrpc import client as xmlrpc
except ImportError:
    import xmlrpclib as xmlrpc

query = sys.argv[1]
pypi = xmlrpc.ServerProxy("https://pypi.org/pypi")
results = pypi.search({"name": query})
json.dump(results, sys.stdout, indent=2)
print()
`

const pythonInfoCode = `
from __future__ import print_function
import json
import sys
try:
    from xmlrpc import client as xmlrpc
except ImportError:
    import xmlrpclib as xmlrpc

package = sys.argv[1]
pypi = xmlrpc.ServerProxy("https://pypi.org/pypi")
releases = pypi.package_releases(package)
if not releases:
    print("{}")
    sys.exit(0)
release, = releases
info = pypi.release_data(package, release)
json.dump(info, sys.stdout, indent=2)
print()
`

const pythonGuessCode = `
from __future__ import print_function
import json
import pipreqs.pipreqs as pipreqs
import sys

imports = pipreqs.get_all_imports(".")
packages = pipreqs.get_pkg_names(imports)
json.dump(packages, sys.stdout, indent=2)
print()
`

func pythonMakeBackend(python string) api.LanguageBackend {
	return api.LanguageBackend{
		Name:             "python-" + python + "-poetry",
		Specfile:         "pyproject.toml",
		Lockfile:         "poetry.lock",
		FilenamePatterns: []string{"*.py"},
		Quirks:           api.QuirksNone,
		Search: func(query string) []api.PkgInfo {
			outputB := util.GetCmdOutput([]string{
				python, "-c", pythonSearchCode, query,
			})
			var outputJSON []pypiXMLRPCEntry
			if err := json.Unmarshal(outputB, &outputJSON); err != nil {
				util.Die("PyPI response: %s", err)
			}
			results := []api.PkgInfo{}
			for i := range outputJSON {
				results = append(results, api.PkgInfo{
					Name:        outputJSON[i].Name,
					Description: outputJSON[i].Summary,
					Version:     outputJSON[i].Version,
				})
			}
			return results
		},
		Info: func(name api.PkgName) api.PkgInfo {
			outputB := util.GetCmdOutput([]string{
				python, "-c", pythonInfoCode, string(name),
			})
			var output pypiXMLRPCInfo
			if err := json.Unmarshal(outputB, &output); err != nil {
				util.Die("PyPI response: %s", err)
			}
			info := api.PkgInfo{
				Name:        output.Name,
				Description: output.Summary,
				Version:     output.Version,
				HomepageURL: output.HomePage,
				Author: util.AuthorInfo{
					Name:  output.Author,
					Email: output.AuthorEmail,
				}.String(),
				License: output.License,
			}
			for _, line := range output.ProjectURL {
				fields := strings.SplitN(line, ", ", 2)
				if len(fields) != 2 {
					continue
				}

				name := fields[0]
				url := fields[1]

				matched, err := regexp.MatchString(`(?i)doc`, name)
				if err != nil {
					panic(err)
				}
				if matched {
					info.DocumentationURL = url
					continue
				}

				matched, err = regexp.MatchString(`(?i)code`, name)
				if err != nil {
					panic(err)
				}
				if matched {
					info.SourceCodeURL = url
					continue
				}

				matched, err = regexp.MatchString(`(?i)track`, name)
				if err != nil {
					panic(err)
				}
				if matched {
					info.BugTrackerURL = url
					continue
				}
			}

			deps := []string{}
			for _, line := range output.RequiresDist {
				if strings.Contains(line, "extra ==") {
					continue
				}

				deps = append(deps, strings.Fields(line)[0])
			}
			info.Dependencies = deps

			return info
		},
		Add: func(pkgs map[api.PkgName]api.PkgSpec) {
			if !util.FileExists("pyproject.toml") {
				util.RunCmd([]string{python, "-m", "poetry", "init", "--no-interaction"})
			}
			cmd := []string{python, "-m", "poetry", "add"}
			for name, spec := range pkgs {
				cmd = append(cmd, string(name)+string(spec))
			}
			util.RunCmd(cmd)
		},
		Remove: func(pkgs map[api.PkgName]bool) {
			cmd := []string{python, "-m", "poetry", "remove"}
			for name, _ := range pkgs {
				cmd = append(cmd, string(name))
			}
			util.RunCmd(cmd)
		},
		Lock: func() {
			util.RunCmd([]string{python, "-m", "poetry", "lock"})
		},
		Install: func() {
			// Unfortunately, this doesn't necessarily uninstall
			// packages that have been removed from the lockfile,
			// which happens for example if 'poetry remove' is
			// interrupted. See
			// <https://github.com/sdispater/poetry/issues/648>.
			util.RunCmd([]string{python, "-m", "poetry", "install"})
		},
		ListSpecfile: func() map[api.PkgName]api.PkgSpec {
			var cfg pyprojectTOML
			if _, err := toml.DecodeFile("pyproject.toml", &cfg); err != nil {
				util.Die("%s", err.Error())
			}
			pkgs := map[api.PkgName]api.PkgSpec{}
			for nameStr, specStr := range cfg.Tool.Poetry.Dependencies {
				if nameStr == "python" {
					continue
				}

				pkgs[api.PkgName(nameStr)] = api.PkgSpec(specStr)
			}
			for nameStr, specStr := range cfg.Tool.Poetry.DevDependencies {
				if nameStr == "python" {
					continue
				}

				pkgs[api.PkgName(nameStr)] = api.PkgSpec(specStr)
			}
			return pkgs
		},
		ListLockfile: func() map[api.PkgName]api.PkgVersion {
			var cfg poetryLock
			if _, err := toml.DecodeFile("poetry.lock", &cfg); err != nil {
				util.Die("%s", err.Error())
			}
			pkgs := map[api.PkgName]api.PkgVersion{}
			for _, pkgObj := range cfg.Package {
				name := api.PkgName(pkgObj.Name)
				version := api.PkgVersion(pkgObj.Version)
				pkgs[name] = version
			}
			return pkgs
		},
		Guess: func() map[api.PkgName]bool {
			outputB := util.GetCmdOutput([]string{
				python, "-c", pythonGuessCode,
			})
			var output []string
			if err := json.Unmarshal(outputB, &output); err != nil {
				util.Die("pipreqs: %s", err)
			}
			pkgs := map[api.PkgName]bool{}
			for i := range output {
				name := api.PkgName(output[i])
				pkgs[name] = true
			}
			return pkgs
		},
	}
}

var python2Backend = pythonMakeBackend("python2")
var python3Backend = pythonMakeBackend("python3")
