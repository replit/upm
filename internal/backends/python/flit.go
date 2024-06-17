// flit.go, functions and bindings for flit
package python

import (
	"context"
	"os/exec"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/nix"
	"github.com/replit/upm/internal/pkg"
	"github.com/replit/upm/internal/util"
	"gopkg.in/DataDog/dd-trace-go.v1/ddtrace/tracer"
)

func flitIsAvailable() bool {
	_, err := exec.LookPath("flit")
	return err == nil
}

func flitGetPackageDir() string {
	if path := getCommonPackageDir(); path != "" {
		return path
	}

	if outputB, err := util.GetCmdOutputFallible([]string{
		"python",
		"-c", "import site; print(site.USER_BASE)",
	}); err == nil {
		return string(outputB)
	}
	return ""
}

func flitListSpecfile(mergeAllGroups bool) (pkgs map[api.PkgName]api.PkgSpec) {
	cfg, err := readPyproject()
	if err != nil {
		util.DieIO("%s", err.Error())
	}

	pkgs = map[api.PkgName]api.PkgSpec{}

	if cfg.Project == nil {
		return pkgs
	}

	for _, pkg := range cfg.Project.Dependencies {
		if name, spec, found := findPackage(pkg); found {
			pkgs[*name] = *spec
		}
	}

	return pkgs
}

// makePythonFlitBackend returns a backend for invoking poetry, given an arg0 for invoking Python
// (either a full path or just a name like "python3") to use when invoking Python.
func makePythonFlitBackend(python string) api.LanguageBackend {
	b := api.LanguageBackend{
		Name:                 "python3-flit",
		Specfile:             "pyproject.toml",
		IsAvailable:          flitIsAvailable,
		Alias:                "python-python3-flit",
		FilenamePatterns:     []string{"*.py"},
		Quirks:               api.QuirksNotReproducible | api.QuirksCannotAddRemove,
		NormalizePackageArgs: normalizePackageArgs,
		NormalizePackageName: normalizePackageName,
		GetPackageDir:        flitGetPackageDir,
		SortPackages:         pkg.SortPrefixSuffix(normalizePackageName),

		Search: searchPypi,
		Info:   info,
		Install: func(ctx context.Context) {
			//nolint:ineffassign,wastedassign,staticcheck
			span, ctx := tracer.StartSpanFromContext(ctx, "flit install")
			defer span.Finish()

			util.RunCmd([]string{"flit", "install"})
		},
		ListSpecfile: flitListSpecfile,
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

			for pkg := range flitListSpecfile(true) {
				deps := nix.PythonNixDeps(string(pkg))
				ops = append(ops, nix.ReplitNixAddToNixEditorOps(deps)...)
			}
			nix.RunNixEditorOps(ops)
		},
	}

	return b
}
