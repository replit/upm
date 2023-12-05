// Package python provides backends for Python 2 and 3 using Poetry.
package python

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/BurntSushi/toml"
	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/nix"
	"github.com/replit/upm/internal/util"
	"gopkg.in/DataDog/dd-trace-go.v1/ddtrace/tracer"
)

var normalizationPattern = regexp.MustCompile(`[-_.]+`)

// this generates a mapping of pypi packages <-> modules
// moduleToPypiPackage pypiPackageToModules are provided
// pypiEntryInfoResponse is a wrapper around pypiEntryInfo
// that matches the format of the REST API
type pypiEntryInfoResponse struct {
	Info pypiEntryInfo `json:"info"`
}

// pypiEntryInfo represents the response we get from the
// PyPI API on doing a single-package lookup.
type pypiEntryInfo struct {
	Author        string   `json:"author"`
	AuthorEmail   string   `json:"author_email"`
	HomePage      string   `json:"home_page"`
	License       string   `json:"license"`
	Name          string   `json:"name"`
	ProjectURL    string   `json:"project_url"`
	PackageURL    string   `json:"package_url"`
	BugTrackerURL string   `json:"bugtrack_url"`
	DocsURL       string   `json:"docs_url"`
	RequiresDist  []string `json:"requires_dist"`
	Summary       string   `json:"summary"`
	Version       string   `json:"version"`
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
// See https://packaging.python.org/en/latest/specifications/name-normalization/
func normalizePackageName(name api.PkgName) api.PkgName {
	nameStr := string(name)
	nameStr = strings.ToLower(nameStr)
	nameStr = normalizationPattern.ReplaceAllString(nameStr, "-")
	return api.PkgName(nameStr)
}

func info(name api.PkgName) api.PkgInfo {
	res, err := api.HttpClient.Get(fmt.Sprintf("https://pypi.org/pypi/%s/json", string(name)))

	if err != nil {
		util.Die("HTTP Request failed with error: %s", err)
	}

	defer res.Body.Close()

	if res.StatusCode == 404 {
		return api.PkgInfo{}
	}

	if res.StatusCode != 200 {
		util.Die("Received status code: %d", res.StatusCode)
	}

	body, err := io.ReadAll(res.Body)
	if err != nil {
		util.Die("Res body read failed with error: %s", err)
	}

	var output pypiEntryInfoResponse
	if err := json.Unmarshal(body, &output); err != nil {
		util.Die("PyPI response: %s", err)
	}

	info := api.PkgInfo{
		Name:             output.Info.Name,
		Description:      output.Info.Summary,
		Version:          output.Info.Version,
		HomepageURL:      output.Info.HomePage,
		DocumentationURL: output.Info.DocsURL,
		BugTrackerURL:    output.Info.BugTrackerURL,
		Author: util.AuthorInfo{
			Name:  output.Info.Author,
			Email: output.Info.AuthorEmail,
		}.String(),
		License: output.Info.License,
	}

	deps := []string{}
	for _, line := range output.Info.RequiresDist {
		if strings.Contains(line, "extra ==") {
			continue
		}

		deps = append(deps, strings.Fields(line)[0])
	}
	info.Dependencies = deps

	return info
}

func add(ctx context.Context, pkgs map[api.PkgName]api.PkgSpec, projectName string) {
	//nolint:ineffassign,wastedassign,staticcheck
	span, ctx := tracer.StartSpanFromContext(ctx, "poetry (init) add")
	defer span.Finish()
	// Initalize the specfile if it doesnt exist
	if !util.Exists("pyproject.toml") {
		cmd := []string{"poetry", "init", "--no-interaction"}

		if projectName != "" {
			cmd = append(cmd, "--name", projectName)
		}

		util.RunCmd(cmd)
	}

	cmd := []string{"poetry", "add"}
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
}

// makePythonPoetryBackend returns a backend for invoking poetry, given an arg0 for invoking Python
// (either a full path or just a name like "python3") to use when invoking Python.
func makePythonPoetryBackend(python string) api.LanguageBackend {
	return api.LanguageBackend{
		Name:             "python3-poetry",
		Alias:            "python-python3-poetry",
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

			outputB, err := util.GetCmdOutputFallible([]string{
				"poetry",
				"config", "settings.virtualenvs.path",
			})
			if err != nil {
				// there's no virtualenv configured, so no package directory
				return ""
			}

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
			results, err := SearchPypi(query)
			if err != nil {
				util.Die("failed to search pypi: %s", err.Error())
			}
			return results
		},
		Info: info,
		Add:  add,
		Remove: func(ctx context.Context, pkgs map[api.PkgName]bool) {
			//nolint:ineffassign,wastedassign,staticcheck
			span, ctx := tracer.StartSpanFromContext(ctx, "poetry remove")
			defer span.Finish()
			cmd := []string{"poetry", "remove"}
			for name := range pkgs {
				cmd = append(cmd, string(name))
			}
			util.RunCmd(cmd)
		},
		Lock: func(ctx context.Context) {
			//nolint:ineffassign,wastedassign,staticcheck
			span, ctx := tracer.StartSpanFromContext(ctx, "poetry lock")
			defer span.Finish()
			util.RunCmd([]string{"poetry", "lock", "--no-update"})
		},
		Install: func(ctx context.Context) {
			//nolint:ineffassign,wastedassign,staticcheck
			span, ctx := tracer.StartSpanFromContext(ctx, "poetry install")
			defer span.Finish()
			// Unfortunately, this doesn't necessarily uninstall
			// packages that have been removed from the lockfile,
			// which happens for example if 'poetry remove' is
			// interrupted. See
			// <https://github.com/sdispater/poetry/issues/648>.
			util.RunCmd([]string{"poetry", "install"})
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
		Guess: func(ctx context.Context) (map[api.PkgName]bool, bool) { return guess(ctx, python) },
		InstallReplitNixSystemDependencies: func(ctx context.Context, pkgs []api.PkgName) {
			//nolint:ineffassign,wastedassign,staticcheck
			span, ctx := tracer.StartSpanFromContext(ctx, "python.InstallReplitNixSystemDependencies")
			defer span.Finish()
			ops := []nix.NixEditorOp{}
			for _, pkg := range pkgs {
				deps := nix.PythonNixDeps(string(pkg))
				ops = append(ops, nix.ReplitNixAddToNixEditorOps(deps)...)
			}

			// Ignore the error here, because if we can't read the specfile,
			// we still want to add the deps from above at least.
			specfilePkgs, _ := listSpecfile()
			for pkg := range specfilePkgs {
				deps := nix.PythonNixDeps(string(pkg))
				ops = append(ops, nix.ReplitNixAddToNixEditorOps(deps)...)
			}
			nix.RunNixEditorOps(ops)
		},
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

func getTopLevelModuleName(fullModname string) string {
	return strings.Split(fullModname, ".")[0]
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

// Python3Backend is a UPM backend for Python 3 that uses Poetry.
var Python3Backend = makePythonPoetryBackend(getPython3())
