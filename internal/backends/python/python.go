// Package python provides backends for Python 2 and 3 using Poetry.
package python

import (
	"encoding/json"
	"os"
	"regexp"
	"strings"

	"github.com/BurntSushi/toml"
	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

// pypiXMLRPCEntry represents one element of the response we get from
// the PyPI XMLRPC API on doing a search.
type pypiXMLRPCEntry struct {
	Name    string `json:"name"`
	Summary string `json:"summary"`
	Version string `json:"version"`
}

// pypiXMLRPCInfo represents the response we get from the PyPI XMLRPC
// API on doing a single-package lookup.
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

// pyprojectTOML represents the relevant parts of a pyproject.toml
// file.
type pyprojectTOML struct {
	Tool struct {
		Poetry struct {
			Dependencies    map[string]string `json:"dependencies"`
			DevDependencies map[string]string `json:"dev-dependencies"`
		} `json:"poetry"`
	} `json:"tool"`
}

// poetryLock represents the relevant parts of a poetry.lock file, in
// TOML format.
type poetryLock struct {
	Package []struct {
		Name    string `json:"name"`
		Version string `json:"version"`
	} `json:"package"`
}

// pythonMakeBackend returns a language backend for a given version of
// Python. name is either "python2" or "python3", and python is the
// name of an executable (either a full path or just a name like
// "python3") to use when invoking Python. (This is used to implement
// UPM_PYTHON2 and UPM_PYTHON3.)
func pythonMakeBackend(name string, python string) api.LanguageBackend {
	return api.LanguageBackend{
		Name:             "python-" + name + "-poetry",
		Specfile:         "pyproject.toml",
		Lockfile:         "poetry.lock",
		FilenamePatterns: []string{"*.py"},
		Quirks: api.QuirksAddRemoveAlsoLocks |
			api.QuirksAddRemoveAlsoInstalls,
		Search: func(query string) []api.PkgInfo {
			outputB := util.GetCmdOutput([]string{
				python, "-c",
				util.GetResource("/python/pypi-search.py"),
				query,
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
				python, "-c",
				util.GetResource("/python/pypi-info.py"),
				string(name),
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
			if !util.Exists("pyproject.toml") {
				util.RunCmd([]string{python, "-m", "poetry", "init", "--no-interaction"})
			}
			cmd := []string{python, "-m", "poetry", "add", "--allow-prereleases"}
			for name, spec := range pkgs {
				name := string(name)
				spec := string(spec)

				// NB: this doesn't work if spec has
				// spaces in it, because of a bug in
				// Poetry that can't be worked around.
				// It looks like that bug might be
				// fixed in the 1.0 release though :/
				if spec != "" {
					cmd = append(cmd, name+" "+spec)
				} else {
					cmd = append(cmd, name)
				}
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
		GuessRegexps: util.Regexps([]string{
			// The (?:.|\\\n) subexpression allows us to
			// match match multiple lines if
			// backslash-escapes are used on the newlines.
			`from (?:.|\\\n) import`,
			`import ((?:.|\\\n)*) as`,
			`import ((?:.|\\\n)*)`,
		}),
		Guess: func() map[api.PkgName]bool {
			tempdir := util.TempDir()
			defer os.RemoveAll(tempdir)

			util.WriteResource("/python/stdlib", tempdir)
			util.WriteResource("/python/mapping", tempdir)
			util.WriteResource("/python/pipreqs.py", tempdir)
			script := util.WriteResource("/python/bare-imports.py", tempdir)

			outputB := util.GetCmdOutput([]string{
				python, script, strings.Join(util.IgnoredPaths, " "),
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

// getPython2 returns either "python2" or the value of the UPM_PYTHON2
// environment variable.
func getPython2() string {
	python2 := os.Getenv("UPM_PYTHON2")
	if python2 != "" {
		return python2
	} else {
		return "python2"
	}
}

// getPython3 returns either "python3" or the value of the UPM_PYTHON3
// environment variable.
func getPython3() string {
	python3 := os.Getenv("UPM_PYTHON3")
	if python3 != "" {
		return python3
	} else {
		return "python3"
	}
}

// Python2Backend is a UPM backend for Python 2 that uses Poetry.
var Python2Backend = pythonMakeBackend("python2", getPython2())

// Python3Backend is a UPM backend for Python 3 that uses Poetry.
var Python3Backend = pythonMakeBackend("python3", getPython3())
