// poetry.go, functions and bindings for Poetry
package python

import (
	"context"
	"os"
	"os/exec"
	"strings"

	"github.com/BurntSushi/toml"
	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/nix"
	"github.com/replit/upm/internal/pkg"
	"github.com/replit/upm/internal/util"
	"gopkg.in/DataDog/dd-trace-go.v1/ddtrace/tracer"
)

// poetryLock represents the relevant parts of a poetry.lock file, in
// TOML format.
type poetryLock struct {
	Package []struct {
		Name    string `json:"name"`
		Version string `json:"version"`
	} `json:"package"`
}

func poetryIsAvailable() bool {
	_, err := exec.LookPath("poetry")
	return err == nil
}

func verifyPoetrySpecfile(path string) (bool, error) {
	cfg, err := readPyproject()
	if err != nil {
		return false, err
	}

	return cfg.Tool.Poetry != nil, nil
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

func listPoetrySpecfile(mergeAllGroups bool) (map[api.PkgName]api.PkgSpec, error) {
	cfg, err := readPyproject()
	if err != nil {
		return nil, err
	}
	pkgs := map[api.PkgName]api.PkgSpec{}
	if cfg.Tool.Poetry == nil {
		return pkgs, nil
	}
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
	if mergeAllGroups && cfg.Tool.Poetry.Group != nil {
		for _, group := range cfg.Tool.Poetry.Group {
			for nameStr, spec := range group.Dependencies {
				specStr := normalizeSpec(spec)
				if specStr == "" {
					continue
				}
				pkgs[api.PkgName(nameStr)] = api.PkgSpec(specStr)
			}
		}
	}

	return pkgs, nil
}

func poetryAdd(ctx context.Context, pkgs map[api.PkgName]api.PkgSpec, projectName string) {
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
		if found, ok := moduleToPypiPackageAliases[name]; ok {
			delete(pkgs, api.PkgName(name))
			name = found
			pkgs[api.PkgName(name)] = api.PkgSpec(spec)
		}
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
		Name:                 "python3-poetry",
		Alias:                "python-python3-poetry",
		Specfile:             "pyproject.toml",
		IsSpecfileCompatible: verifyPoetrySpecfile,
		Lockfile:             "poetry.lock",
		IsAvailable:          poetryIsAvailable,
		FilenamePatterns:     []string{"*.py"},
		Quirks: api.QuirksAddRemoveAlsoLocks |
			api.QuirksAddRemoveAlsoInstalls,
		NormalizePackageArgs: normalizePackageArgs,
		NormalizePackageName: normalizePackageName,
		GetPackageDir: func() string {
			// Check if we're already inside an activated
			// virtualenv. If so, just use it.
			if venv := os.Getenv("VIRTUAL_ENV"); venv != "" {
				return venv
			}

			// Take PYTHONUSERBASE into consideration, if set
			if userbase := os.Getenv("PYTHONUSERBASE"); userbase != "" {
				return userbase
			}

			// Terminate early if we're running inside a repl.
			// This will suppress the following poetry commands
			// from showing up in the Packager pane.
			if os.Getenv("REPL_HOME") != "" {
				return ""
			}

			outputB, err := util.GetCmdOutputFallible([]string{
				"poetry", "env", "list", "--full-path",
			})
			if err != nil {
				// there's no virtualenv configured, so no package directory
				return ""
			}

			var path string
			for _, line := range strings.Split(strings.TrimSpace(string(outputB)), "\n") {
				var isActive bool
				path, isActive = strings.CutSuffix(line, " (Activated)")
				if isActive {
					break
				}
			}

			return path
		},
		SortPackages: pkg.SortPrefixSuffix(normalizePackageName),

		Search: searchPypi,
		Info:   info,
		Add:    poetryAdd,
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
		ListSpecfile: func(mergeAllGroups bool) map[api.PkgName]api.PkgSpec {
			pkgs, err := listPoetrySpecfile(mergeAllGroups)
			if err != nil {
				util.DieIO("%s", err.Error())
			}

			return pkgs
		},
		ListLockfile: func() map[api.PkgName]api.PkgVersion {
			var cfg poetryLock
			if _, err := toml.DecodeFile("poetry.lock", &cfg); err != nil {
				util.DieProtocol("%s", err.Error())
			}
			pkgs := map[api.PkgName]api.PkgVersion{}
			for _, pkgObj := range cfg.Package {
				name := api.PkgName(pkgObj.Name)
				version := api.PkgVersion(pkgObj.Version)
				pkgs[name] = version
			}
			return pkgs
		},
		GuessRegexps: pythonGuessRegexps,
		Guess:        guess,
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
			specfilePkgs, _ := listPoetrySpecfile(true)
			for pkg := range specfilePkgs {
				deps := nix.PythonNixDeps(string(pkg))
				ops = append(ops, nix.ReplitNixAddToNixEditorOps(deps)...)
			}
			nix.RunNixEditorOps(ops)
		},
	}
}
