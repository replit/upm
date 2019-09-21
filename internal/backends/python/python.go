// Package python provides backends for Python 2 and 3 using Poetry.
package python

import (
	"encoding/json"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/BurntSushi/toml"
	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

// this generates a mapping of pypi packages <-> modules
// moduleToPypiPackage pypiPackageToModules are provided
//go:generate go run ./gen_pypi_map -from pypi_packages.json -pkg python -out pypi_map.gen.go

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
			Name string `json:"name"`
			// interface{} because they can be either
			// strings or maps (why?? good lord).
			Dependencies    map[string]interface{} `json:"dependencies"`
			DevDependencies map[string]interface{} `json:"dev-dependencies"`
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

// normalizeSpec returns the version string from a Poetry spec, or the
// empty string. The Poetry spec may be either a string or a
// map[string]interface{} with a "version" key that is a string. If
// neither, then the empty string is returned.
func normalizeSpec(spec interface{}) string {
	switch spec := spec.(type) {
	case string:
		return spec
	case map[string]interface{}:
		switch spec := spec["version"].(type) {
		case string:
			return spec
		}
	}
	return ""
}

// normalizePackageName implements NormalizePackageName for the Python
// backends.
func normalizePackageName(name api.PkgName) api.PkgName {
	nameStr := string(name)
	nameStr = strings.ToLower(nameStr)
	nameStr = strings.Replace(nameStr, "_", "-", -1)
	return api.PkgName(nameStr)
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
		NormalizePackageName: normalizePackageName,
		GetPackageDir: func() string {
			// Check if we're already inside an activated
			// virtualenv. If so, just use it.
			if venv := os.Getenv("VIRTUAL_ENV"); venv != "" {
				return venv
			}

			// Ideally Poetry would provide some way of
			// actually checking where the virtualenv will
			// go. But it doesn't. So we have to
			// reimplement the logic ourselves, which is
			// totally fragile and disgusting. (No, we
			// can't use 'poetry run which python' because
			// that will *create* a virtualenv if one
			// doesn't exist, and there's no workaround
			// for that without mutating the global config
			// file.)
			//
			// Note, we don't yet support Poetry's
			// settings.virtualenvs.in-project. That would
			// be a pretty easy fix, though. (Why is this
			// so complicated??)

			outputB := util.GetCmdOutput([]string{
				python, "-m", "poetry",
				"config", "settings.virtualenvs.path",
			})
			var path string
			if err := json.Unmarshal(outputB, &path); err != nil {
				util.Die("parsing output from Poetry: %s", err)
			}

			base := ""
			if util.Exists("pyproject.toml") {
				var cfg pyprojectTOML
				if _, err := toml.DecodeFile("pyproject.toml", &cfg); err != nil {
					util.Die("%s", err.Error())
				}
				base = cfg.Tool.Poetry.Name
			}

			if base == "" {
				cwd, err := os.Getwd()
				if err != nil {
					util.Die("%s", err)
				}
				base = strings.ToLower(filepath.Base(cwd))
			}

			version := strings.TrimSpace(string(util.GetCmdOutput([]string{
				python, "-c",
				`import sys; print(".".join(map(str, sys.version_info[:2])))`,
			})))

			return filepath.Join(path, base+"-py"+version)
		},
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
			cmd := []string{python, "-m", "poetry", "add"}
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
			pkgs, err := listSpecfile()
			if err != nil {
				util.Die("%s", err.Error())
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
		Guess: func() (map[api.PkgName]bool, bool) { return guess(python) },
	}
}

func listSpecfile() (map[api.PkgName]api.PkgSpec, error) {
	var cfg pyprojectTOML
	if _, err := toml.DecodeFile("pyproject.toml", &cfg); err != nil {
		return nil, err
	}
	pkgs := map[api.PkgName]api.PkgSpec{}
	for nameStr, spec := range cfg.Tool.Poetry.Dependencies {
		if nameStr == "python" {
			continue
		}

		specStr := normalizeSpec(spec)
		if specStr == "" {
			continue
		}
		pkgs[api.PkgName(nameStr)] = api.PkgSpec(specStr)
	}
	for nameStr, spec := range cfg.Tool.Poetry.DevDependencies {
		if nameStr == "python" {
			continue
		}

		specStr := normalizeSpec(spec)
		if specStr == "" {
			continue
		}
		pkgs[api.PkgName(nameStr)] = api.PkgSpec(specStr)
	}

	return pkgs, nil
}

func guess(python string) (map[api.PkgName]bool, bool) {
	tempdir := util.TempDir()
	defer os.RemoveAll(tempdir)

	util.WriteResource("/python/pipreqs.py", tempdir)
	script := util.WriteResource("/python/bare-imports.py", tempdir)

	outputB := util.GetCmdOutput([]string{
		python, script, strings.Join(util.IgnoredPaths, " "),
	})

	var output struct {
		Imports []string `json:"imports"`
		Success bool     `json:"success"`
	}

	if err := json.Unmarshal(outputB, &output); err != nil {
		util.Die("pipreqs: %s", err)
	}

	availMods := map[string]bool{}

	if knownPkgs, err := listSpecfile(); err == nil {
		for pkgName := range knownPkgs {
			mods, ok := pypiPackageToModules[string(pkgName)]
			if ok {
				for _, mod := range strings.Split(mods, ",") {
					availMods[mod] = true
				}
			}
		}
	}

	pkgs := map[api.PkgName]bool{}

	for _, modname := range output.Imports {
		// provided by an existing package or perhaps by the system
		if availMods[modname] {
			continue
		}

		pkg, ok := moduleToPypiPackage[modname]
		if ok {
			name := api.PkgName(pkg)
			pkgs[normalizePackageName(name)] = true
		}
	}

	return pkgs, output.Success
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
