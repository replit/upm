// pip.go, functions and bindings for pip
package python

import (
	"context"
	"os"
	"os/exec"
	"strings"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/nix"
	"github.com/replit/upm/internal/pkg"
	"github.com/replit/upm/internal/util"
	"gopkg.in/DataDog/dd-trace-go.v1/ddtrace/tracer"
)

func pipIsAvailable() bool {
	_, err := exec.LookPath("pip")
	return err == nil
}

func pipGetPackageDir() string {
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

func pipAdd(pipFlags []PipFlag) func(context.Context, map[api.PkgName]api.PkgSpec, string) {
	return func(ctx context.Context, pkgs map[api.PkgName]api.PkgSpec, projectName string) {
		//nolint:ineffassign,wastedassign,staticcheck
		span, ctx := tracer.StartSpanFromContext(ctx, "pip install")
		defer span.Finish()

		cmd := []string{"pip", "install"}
		for _, flag := range pipFlags {
			cmd = append(cmd, string(flag))
		}
		for name, spec := range pkgs {
			name := string(name)
			spec := string(spec)
			if found, ok := moduleToPypiPackageAliases[name]; ok {
				delete(pkgs, api.PkgName(name))
				name = found
				pkgs[api.PkgName(name)] = api.PkgSpec(spec)
			}

			cmd = append(cmd, name+" "+spec)
		}
		// Run install
		util.RunCmd(cmd)
		// Determine what was actually installed
		outputB, err := util.GetCmdOutputFallible([]string{
			"pip", "freeze",
		})
		if err != nil {
			util.DieSubprocess("failed to run freeze: %s", err.Error())
		}

		// As we walk through the output of pip freeze,
		// compare the package metadata name to the normalized
		// pkgs that we are trying to install, to see which we
		// want to track in `requirements.txt`.
		normalizedPkgs := make(map[api.PkgName]api.PkgName)
		for name := range pkgs {
			normalizedPkgs[normalizePackageName(name)] = name
		}

		var toAppend []string
		for _, canonicalSpec := range strings.Split(string(outputB), "\n") {
			var name api.PkgName
			matches := matchPackageAndSpec.FindSubmatch(([]byte)(canonicalSpec))
			if len(matches) > 0 {
				name = normalizePackageName(api.PkgName(string(matches[1])))
				if rawName, ok := normalizedPkgs[name]; ok {
					// We've meticulously maintained the pkgspec from the CLI args, if specified,
					// so we don't clobber it with pip freeze's output of "==="
					name := string(matches[1])
					userArgSpec := string(pkgs[rawName])
					toAppend = append(toAppend, name+userArgSpec)
				}
			}
		}

		handle, err := os.OpenFile("requirements.txt", os.O_APPEND|os.O_CREATE|os.O_RDWR, 0o644)
		if err != nil {
			util.DieIO("Unable to open requirements.txt for writing: %s", err)
		}
		defer handle.Close()

		// Probe handle to determine if the last character is a newline
		var hasTrailingNewline bool
		fileInfo, err := handle.Stat()
		if err != nil {
			util.DieIO("Error getting file info: %s", err)
		}
		if fileInfo.Size() > 0 {
			var lastChar = make([]byte, 1)
			_, err := handle.ReadAt(lastChar, fileInfo.Size()-1)
			if err != nil {
				util.DieIO("Error reading last character: %s", err)
			}
			hasTrailingNewline = (lastChar[0] == '\n')
		}
		// Maintain existing formatting style.
		// If the user has a trailing newline, keep it.
		// If the user has no trailing newline, don't add one.
		var leadingNewline, trailingNewline string
		if hasTrailingNewline {
			leadingNewline = ""
			trailingNewline = "\n"
		} else {
			leadingNewline = "\n"
			trailingNewline = ""
		}
		for _, line := range toAppend {
			if _, err := handle.WriteString(leadingNewline + line + trailingNewline); err != nil {
				util.DieIO("Error writing to requirements.txt: %s", err)
			}
		}
	}
}

// makePythonPipBackend returns a backend for invoking poetry, given an arg0 for invoking Python
// (either a full path or just a name like "python3") to use when invoking Python.
func makePythonPipBackend(python string) api.LanguageBackend {
	var pipFlags []PipFlag

	b := api.LanguageBackend{
		Name:                 "python3-pip",
		Specfile:             "requirements.txt",
		IsAvailable:          pipIsAvailable,
		Alias:                "python-python3-pip",
		FilenamePatterns:     []string{"*.py"},
		Quirks:               api.QuirksAddRemoveAlsoInstalls | api.QuirksNotReproducible,
		NormalizePackageArgs: normalizePackageArgs,
		NormalizePackageName: normalizePackageName,
		GetPackageDir:        pipGetPackageDir,
		SortPackages:         pkg.SortPrefixSuffix(normalizePackageName),

		Search: searchPypi,
		Info:   info,
		Add:    pipAdd(pipFlags),
		Remove: func(ctx context.Context, pkgs map[api.PkgName]bool) {
			//nolint:ineffassign,wastedassign,staticcheck
			span, ctx := tracer.StartSpanFromContext(ctx, "pip uninstall")
			defer span.Finish()

			cmd := []string{"pip", "uninstall", "--yes"}
			for name := range pkgs {
				cmd = append(cmd, string(name))
			}
			util.RunCmd(cmd)
			err := RemoveFromRequirementsTxt("requirements.txt", pkgs)
			if err != nil {
				util.DieIO("%s", err.Error())
			}
		},
		Install: func(ctx context.Context) {
			//nolint:ineffassign,wastedassign,staticcheck
			span, ctx := tracer.StartSpanFromContext(ctx, "pip install")
			defer span.Finish()

			util.RunCmd([]string{"pip", "install", "-r", "requirements.txt"})
		},
		ListSpecfile: func(mergeAllGroups bool) map[api.PkgName]api.PkgSpec {
			flags, pkgs, err := ListRequirementsTxt("requirements.txt")
			if err != nil {
				util.DieIO("%s", err.Error())
			}

			// Stash the seen flags into a module global.
			// This isn't great, but the expectation is that ListSpecfile
			// is called before we run `Add`.
			pipFlags = flags

			// NB: We rely on requirements.txt being populated with the
			// Python package _metadata_ name, not the PEP-503/PEP-508
			// normalized version.
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
			_, specfilePkgs, _ := ListRequirementsTxt("requirements.txt")
			for pkg := range specfilePkgs {
				deps := nix.PythonNixDeps(string(pkg))
				ops = append(ops, nix.ReplitNixAddToNixEditorOps(deps)...)
			}
			nix.RunNixEditorOps(ops)
		},
	}

	return b
}
