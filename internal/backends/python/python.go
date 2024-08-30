// Package python provides backends for Python 2 and 3 using Poetry and pip.
package python

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"os/exec"
	"regexp"
	"strings"

	"github.com/BurntSushi/toml"
	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/nix"
	"github.com/replit/upm/internal/pkg"
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

type pyprojectPackageCfg struct {
	Include string `json:"include"`
	From    string `json:"from"`
}

type pyprojectTOMLGroup struct {
	Dependencies map[string]interface{} `json:"dependencies"`
}

// pyprojectTOML represents the relevant parts of a pyproject.toml
// file.
type pyprojectTOML struct {
	BuildSystem *struct {
		Requires     []string `toml:"requires"`
		BuildBackend string   `toml:"build-backend"`
	} `toml:"build-system"`
	Project *struct {
		Dependencies []string `toml:"dependencies"`
	} `toml:"project"`
	Tool struct {
		Poetry *struct {
			Name string `toml:"name"`
			// interface{} because they can be either
			// strings or maps (why?? good lord).
			Dependencies    map[string]interface{}        `toml:"dependencies"`
			DevDependencies map[string]interface{}        `toml:"dev-dependencies"`
			Packages        []pyprojectPackageCfg         `toml:"packages"`
			Group           map[string]pyprojectTOMLGroup `toml:"group"`
		} `toml:"poetry"`
		Uv *struct {
			Sources map[string]interface{} `toml:"sources"`
		} `toml:"uv"`
	} `toml:"tool"`
}

// poetryLock represents the relevant parts of a poetry.lock file, in
// TOML format.
type poetryLock struct {
	Package []struct {
		Name    string `json:"name"`
		Version string `json:"version"`
	} `json:"package"`
}

type uvLock struct {
	Version        int    `toml:"version"`
	RequiresPython string `toml:"requires-python"`
	Packages       []struct {
		Name    string `toml:"name"`
		Version string `toml:"version"`
		Source  struct {
			Registry string `toml:"registry"`
		} `toml:"source"`
		Sdist struct {
			URL  string `toml:"url"`
			Hash string `toml:"hash"`
			Size int    `toml:"size"`
		} `toml:"sdist"`
		Wheels []struct {
			URL  string `toml:"url"`
			Hash string `toml:"hash"`
			Size int    `toml:"size"`
		} `toml:"wheels"`
	} `toml:"package"`
}

func pep440Join(name api.PkgName, spec api.PkgSpec) string {
	if spec == "" {
		return string(name)
	} else if matchSpecOnly.Match([]byte(spec)) {
		return string(name) + string(spec)
	}
	// We did not match the version range separator in the spec, so we got
	// something like "foo 1.2.3", we need to return "foo==1.2.3"
	return string(name) + "==" + string(spec)
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

func normalizePackageArgs(args []string) map[api.PkgName]api.PkgCoordinates {
	pkgs := make(map[api.PkgName]api.PkgCoordinates)
	versionComponent := regexp.MustCompile(pep440VersionComponent)
	for _, arg := range args {
		var rawName string
		var name api.PkgName
		var spec api.PkgSpec
		if found := matchPackageAndSpec.FindSubmatch([]byte(arg)); len(found) > 0 {
			rawName = string(found[1])
			name = api.PkgName(rawName)
			spec = api.PkgSpec(string(found[2]))
		} else {
			split := strings.SplitN(arg, " ", 2)
			rawName = split[0]
			name = api.PkgName(rawName)
			if len(split) > 1 {
				specStr := strings.TrimSpace(split[1])

				if specStr != "" {
					if offset := versionComponent.FindIndex([]byte(spec)); len(offset) == 0 {
						spec = api.PkgSpec("==" + specStr)
					} else {
						spec = api.PkgSpec(specStr)
					}
				} else {
					spec = api.PkgSpec(specStr)
				}
			}
		}
		pkgs[normalizePackageName(name)] = api.PkgCoordinates{
			Name: rawName,
			Spec: spec,
		}
	}
	return pkgs
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
		util.DieNetwork("HTTP Request failed with error: %s", err)
	}

	defer res.Body.Close()

	if res.StatusCode == 404 {
		return api.PkgInfo{}
	}

	if res.StatusCode != 200 {
		util.DieNetwork("Received status code: %d", res.StatusCode)
	}

	body, err := io.ReadAll(res.Body)
	if err != nil {
		util.DieProtocol("Res body read failed with error: %s", err)
	}

	var output pypiEntryInfoResponse
	if err := json.Unmarshal(body, &output); err != nil {
		util.DieProtocol("PyPI response: %s", err)
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

func searchPypi(query string) []api.PkgInfo {
	// Normalize query before looking it up in the overide map
	query = string(normalizePackageName(api.PkgName(query)))
	if renamed, found := moduleToPypiPackageOverride[query]; found {
		query = renamed[0]
	}
	results, err := SearchPypi(query)
	if err != nil {
		util.DieNetwork("failed to search pypi: %s", err.Error())
	}
	// Elide package override from results
	filtered := []api.PkgInfo{}
	for _, info := range results {
		lowered := string(normalizePackageName(api.PkgName(info.Name)))
		if rename, ok := moduleToPypiPackageOverride[lowered]; ok {
			// If rename[0] == lowered, we are noconflicting a package
			// If they are different, we are overriding that package,
			// so the query'd package should not be included in the results
			// set.
			if rename[0] != lowered {
				continue
			}
		}
		filtered = append(filtered, info)
	}
	return filtered
}

func commonInstallNixDeps(ctx context.Context, pkgs []api.PkgName, specfilePkgs map[api.PkgName]api.PkgSpec) {
	//nolint:ineffassign,wastedassign,staticcheck
	span, ctx := tracer.StartSpanFromContext(ctx, "python.InstallReplitNixSystemDependencies")
	defer span.Finish()
	ops := []nix.NixEditorOp{}
	for _, pkg := range pkgs {
		deps := nix.PythonNixDeps(string(pkg))
		ops = append(ops, nix.ReplitNixAddToNixEditorOps(deps)...)
	}

	for pkg := range specfilePkgs {
		deps := nix.PythonNixDeps(string(pkg))
		ops = append(ops, nix.ReplitNixAddToNixEditorOps(deps)...)
	}
	nix.RunNixEditorOps(ops)
}

func commonGuessPackageDir() string {
	// Check if we're already inside an activated
	// virtualenv. If so, just use it.
	if venv := os.Getenv("VIRTUAL_ENV"); venv != "" {
		return venv
	}

	// Take PYTHONUSERBASE into consideration, if set
	if userbase := os.Getenv("PYTHONUSERBASE"); userbase != "" {
		return userbase
	}

	return ""
}

var pythonGuessRegexps = util.Regexps([]string{
	// The (?:.|\\\n) subexpression allows us to
	// match match multiple lines if
	// backslash-escapes are used on the newlines.
	`from (?:.|\\\n) import`,
	`import ((?:.|\\\n)*) as`,
	`import ((?:.|\\\n)*)`,
})

func readPyproject() (*pyprojectTOML, error) {
	var cfg pyprojectTOML
	if _, err := toml.DecodeFile("pyproject.toml", &cfg); err != nil {
		return nil, err
	}
	return &cfg, nil
}

// makePythonPoetryBackend returns a backend for invoking poetry
func makePythonPoetryBackend() api.LanguageBackend {
	listPoetrySpecfile := func(mergeAllGroups bool) (map[api.PkgName]api.PkgSpec, error) {
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

	return api.LanguageBackend{
		Name:     "python3-poetry",
		Alias:    "python-python3-poetry",
		Specfile: "pyproject.toml",
		IsSpecfileCompatible: func(path string) (bool, error) {
			cfg, err := readPyproject()
			if err != nil {
				return false, err
			}

			return cfg.Tool.Poetry != nil, nil
		},
		Lockfile: "poetry.lock",
		IsAvailable: func() bool {
			_, err := exec.LookPath("poetry")
			return err == nil
		},
		FilenamePatterns: []string{"*.py"},
		Quirks: api.QuirksAddRemoveAlsoLocks |
			api.QuirksAddRemoveAlsoInstalls,
		NormalizePackageArgs: normalizePackageArgs,
		NormalizePackageName: normalizePackageName,
		GetPackageDir: func() string {
			pkgdir := commonGuessPackageDir()
			if pkgdir != "" {
				return pkgdir
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
		Add: func(ctx context.Context, pkgs map[api.PkgName]api.PkgSpec, projectName string) {
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
				if found, ok := moduleToPypiPackageAliases[string(name)]; ok {
					delete(pkgs, api.PkgName(name))
					name = api.PkgName(found)
					pkgs[name] = api.PkgSpec(spec)
				}

				// NB: this doesn't work if spec has
				// spaces in it, because of a bug in
				// Poetry that can't be worked around.
				// It looks like that bug might be
				// fixed in the 1.0 release though :/
				cmd = append(cmd, pep440Join(name, spec))
			}
			util.RunCmd(cmd)
		},
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
			// Ignore the error here, because if we can't read the specfile,
			// we still want to add the deps from above at least.
			specfilePkgs, _ := listPoetrySpecfile(true)
			commonInstallNixDeps(ctx, pkgs, specfilePkgs)
		},
	}
}

// makePythonPipBackend returns a backend for invoking pip.
func makePythonPipBackend() api.LanguageBackend {
	var pipFlags []PipFlag

	b := api.LanguageBackend{
		Name:     "python3-pip",
		Specfile: "requirements.txt",
		IsSpecfileCompatible: func(path string) (bool, error) {
			cfg, err := readPyproject()
			if err != nil {
				return true, err
			}

			return cfg.Tool.Poetry == nil, nil
		},
		IsAvailable: func() bool {
			_, err := exec.LookPath("pip")
			return err == nil
		},
		Alias:                "python-python3-pip",
		FilenamePatterns:     []string{"*.py"},
		Quirks:               api.QuirksAddRemoveAlsoInstalls | api.QuirksNotReproducible,
		NormalizePackageArgs: normalizePackageArgs,
		NormalizePackageName: normalizePackageName,
		GetPackageDir: func() string {
			pkgdir := commonGuessPackageDir()
			if pkgdir != "" {
				return pkgdir
			}

			if outputB, err := util.GetCmdOutputFallible([]string{
				"python",
				"-c", "import site; print(site.USER_BASE)",
			}); err == nil {
				return string(outputB)
			}

			return ""
		},
		SortPackages: pkg.SortPrefixSuffix(normalizePackageName),

		Search: searchPypi,
		Info:   info,
		Add: func(ctx context.Context, pkgs map[api.PkgName]api.PkgSpec, projectName string) {
			//nolint:ineffassign,wastedassign,staticcheck
			span, ctx := tracer.StartSpanFromContext(ctx, "pip install")
			defer span.Finish()

			cmd := []string{"pip", "install"}
			for _, flag := range pipFlags {
				cmd = append(cmd, string(flag))
			}
			for name, spec := range pkgs {
				if found, ok := moduleToPypiPackageAliases[string(name)]; ok {
					delete(pkgs, name)
					name = api.PkgName(found)
					pkgs[name] = spec
				}

				cmd = append(cmd, pep440Join(name, spec))
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
						toAppend = append(toAppend, pep440Join(name, pkgs[rawName]))
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
		},
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
			// Ignore the error here, because if we can't read the specfile,
			// we still want to add the deps from above at least.
			_, specfilePkgs, _ := ListRequirementsTxt("requirements.txt")
			commonInstallNixDeps(ctx, pkgs, specfilePkgs)
		},
	}

	return b
}

// makePythonUvBackend returns a backend for invoking uv.
func makePythonUvBackend() api.LanguageBackend {
	listUvSpecfile := func() map[api.PkgName]api.PkgSpec {
		cfg, err := readPyproject()
		if err != nil {
			return nil
		}

		pkgs := map[api.PkgName]api.PkgSpec{}

		for _, dep := range cfg.Project.Dependencies {
			var name *api.PkgName
			var spec *api.PkgSpec

			matches := matchPackageAndSpec.FindSubmatch([]byte(dep))
			if len(matches) > 1 {
				_name := api.PkgName(string(matches[1]))
				name = &_name
			}
			if len(matches) > 2 {
				_spec := api.PkgSpec(string(matches[2]))
				spec = &_spec
			} else {
				_spec := api.PkgSpec("")
				spec = &_spec
			}
			pkgs[*name] = *spec
		}

		return pkgs
	}
	b := api.LanguageBackend{
		Name:     "python3-uv",
		Specfile: "pyproject.toml",
		IsSpecfileCompatible: func(path string) (bool, error) {
			cfg, err := readPyproject()
			if err != nil {
				return false, err
			}

			return cfg.Tool.Poetry == nil && cfg.BuildSystem.BuildBackend == "hatchling.build", nil
		},
		Lockfile: "uv.lock",
		IsAvailable: func() bool {
			_, err := exec.LookPath("uv")
			return err == nil
		},
		Alias:                "python-python3-uv",
		FilenamePatterns:     []string{"*.py"},
		Quirks:               api.QuirksAddRemoveAlsoInstalls | api.QuirksAddRemoveAlsoLocks,
		NormalizePackageArgs: normalizePackageArgs,
		NormalizePackageName: normalizePackageName,
		GetPackageDir: func() string {
			pkgdir := commonGuessPackageDir()
			if pkgdir != "" {
				return pkgdir
			}

			return ""
		},
		SortPackages: pkg.SortPrefixSuffix(normalizePackageName),

		Search: searchPypi,
		Info:   info,
		Add: func(ctx context.Context, pkgs map[api.PkgName]api.PkgSpec, projectName string) {
			//nolint:ineffassign,wastedassign,staticcheck
			span, ctx := tracer.StartSpanFromContext(ctx, "uv (init) add")
			defer span.Finish()
			// Initalize the specfile if it doesnt exist
			if !util.Exists("pyproject.toml") {
				cmd := []string{"uv", "init", "--no-progress"}

				if projectName != "" {
					cmd = append(cmd, "--name", projectName)
				}

				util.RunCmd(cmd)
			}

			cmd := []string{"uv", "add"}
			for name, spec := range pkgs {
				if found, ok := moduleToPypiPackageAliases[string(name)]; ok {
					delete(pkgs, name)
					name = api.PkgName(found)
					pkgs[api.PkgName(name)] = api.PkgSpec(spec)
				}

				cmd = append(cmd, pep440Join(name, spec))
			}
			util.RunCmd(cmd)
		},
		Lock: func(ctx context.Context) {
			//nolint:ineffassign,wastedassign,staticcheck
			span, ctx := tracer.StartSpanFromContext(ctx, "poetry lock")
			defer span.Finish()
			util.RunCmd([]string{"uv", "lock"})
		},
		Remove: func(ctx context.Context, pkgs map[api.PkgName]bool) {
			//nolint:ineffassign,wastedassign,staticcheck
			span, ctx := tracer.StartSpanFromContext(ctx, "uv uninstall")
			defer span.Finish()

			cmd := []string{"uv", "remove"}
			for name := range pkgs {
				cmd = append(cmd, string(name))
			}
			util.RunCmd(cmd)
		},
		Install: func(ctx context.Context) {
			//nolint:ineffassign,wastedassign,staticcheck
			span, ctx := tracer.StartSpanFromContext(ctx, "uv install")
			defer span.Finish()

			util.RunCmd([]string{"uv", "sync"})
		},
		ListSpecfile: func(mergeAllGroups bool) map[api.PkgName]api.PkgSpec {
			pkgs := listUvSpecfile()
			return pkgs
		},
		ListLockfile: func() map[api.PkgName]api.PkgVersion {
			var cfg uvLock
			if _, err := toml.DecodeFile("uv.lock", &cfg); err != nil {
				util.DieProtocol("%s", err.Error())

			}
			pkgs := map[api.PkgName]api.PkgVersion{}
			for _, pkgObj := range cfg.Packages {
				pkgs[api.PkgName(pkgObj.Name)] = api.PkgVersion(pkgObj.Version)
			}
			return pkgs
		},
		GuessRegexps: pythonGuessRegexps,
		Guess:        guess,
		InstallReplitNixSystemDependencies: func(ctx context.Context, pkgs []api.PkgName) {
			// Ignore the error here, because if we can't read the specfile,
			// we still want to add the deps from above at least.
			specfilePkgs := listUvSpecfile()
			commonInstallNixDeps(ctx, pkgs, specfilePkgs)
		},
	}

	return b
}

// PythonPoetryBackend is a UPM backend for Python 3 that uses Poetry.
var PythonPoetryBackend = makePythonPoetryBackend()
var PythonPipBackend = makePythonPipBackend()
var PythonUvBackend = makePythonUvBackend()
