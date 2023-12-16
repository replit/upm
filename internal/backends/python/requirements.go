package python

import (
	"bufio"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

// requirements.txt parser
//
// Specification reference:
//
//   https://pip.pypa.io/en/stable/reference/requirements-file-format/#example

var pep345Name = `(?:[A-Z0-9]|[A-Z0-9][A-Z0-9._-]*[A-Z0-9])`
var pep440VersionComponent = `(?:(?:~=|!=|===|==|>=|<=|>|<)\s*[^, ]+)`
var pep440VersionSpec = pep440VersionComponent + `(?:\s*,\s*` + pep440VersionComponent + `)*`
var extrasSpec = `\[(` + pep345Name + `(?:\s*,\s*` + pep345Name + `)*)\]`
var matchPackageAndSpec = regexp.MustCompile(`(?i)^\s*(` + pep345Name + `)\s*` + `((?:` + extrasSpec + `)?\s*(?:` + pep440VersionSpec + `)?)?\s*$`)
var matchEggComponent = regexp.MustCompile(`(?i)\begg=(` + pep345Name + `)(?:$|[^A-Z0-9])`)

// Global options:
//
//	https://pip.pypa.io/en/stable/reference/requirements-file-format/#global-options
//
// NB: We explicitly ignore -r/--requirement and -c/--constraint here, since we handle them separately
var knownFlags = map[string]bool{
	`-i`:                true,
	`--index-url`:       true,
	`--extra-index-url`: true,
	`--no-index`:        true,
	`-e`:                true,
	`--editable`:        true,
	`-f`:                true,
	`--find-links`:      true,
	`--no-binary`:       true,
	`--only-binary`:     true,
	`--prefer-binary`:   true,
	`--require-hashes`:  true,
	`--pre`:             true,
	`--trusted-host`:    true,
	`--use-feature`:     true,
}

type PipFlag string
type Constraints map[api.PkgName]api.PkgSpec

func findPackage(line string) (*api.PkgName, *api.PkgSpec, bool) {
	var name *api.PkgName
	var spec *api.PkgSpec

	var found bool

	matches := matchPackageAndSpec.FindSubmatch([]byte(line))
	if len(matches) > 1 {
		_name := api.PkgName(string(matches[1]))
		name = &_name
		found = true
	}
	if len(matches) > 2 {
		_spec := api.PkgSpec(string(matches[2]))
		spec = &_spec
	}
	return name, spec, found
}

func recurseRequirementsTxt(depth int, path string, sofar map[api.PkgName]api.PkgSpec, constraints Constraints) ([]PipFlag, map[api.PkgName]api.PkgSpec, Constraints) {
	// Perhaps this can be lifted, but sensibly attempt to protect ourselves
	if depth > 10 {
		util.Die("Too many -r redirects in %s", path)
	}

	var flags []PipFlag

	handle, err := os.Open(path)
	if err != nil {
		util.Die("%s", err.Error())
	}
	defer handle.Close()

	scanner := bufio.NewScanner(handle)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())

		// Separate out comments
		segments := strings.SplitN(line, "#", 1)
		if len(segments) == 2 {
			line = strings.TrimSpace(segments[0])
			// comment = strings.TrimSpace(segments[1])
		}

		if line == "" {
			// Skip blank lines
		} else if name, spec, found := findPackage(line); found {
			// Found a package!
			sofar[*name] = *spec
		} else if nextfile, found := strings.CutPrefix(line, "-r "); found {
			// # It is possible to refer to other requirement files...
			// -r other-requirements.txt
			nextfile = filepath.Join(filepath.Dir(path), strings.TrimSpace(nextfile))
			var newFlags []PipFlag
			newFlags, sofar, constraints = recurseRequirementsTxt(depth+1, nextfile, sofar, constraints)
			flags = append(flags, newFlags...)
		} else if _, found := strings.CutPrefix(line, "-c "); found {
			// ... or constraints files.
			// -c constraints.txt
			//
			// TODO: Accumulate constraints to pass to underlyling pip
		} else if parts := strings.SplitN(line, " ", 2); len(parts) > 1 && knownFlags[parts[0]] {
			flags = append(flags, PipFlag(strings.Join(parts, " ")))
			// If we find an editable package, try to extract the package name out of the URI
			// This both shows that package in the `upm list` output, as well as prevents upm
			// from inserting it into requirements.txt, which would then cause a conflict
			// on the next run.
			if parts[0] == "-e" || parts[0] == "--editable" {
				matches := matchEggComponent.FindSubmatch([]byte(line))
				if len(matches) > 1 {
					sofar[api.PkgName(string(matches[1]))] = ""
				}
			}
		}
	}

	return flags, sofar, constraints
}

func ListRequirementsTxt(path string) ([]PipFlag, map[api.PkgName]api.PkgSpec) {
	flags, result, _ := recurseRequirementsTxt(0, path, make(map[api.PkgName]api.PkgSpec), make(Constraints))
	return flags, result
}

func recurseRemoveFromRequirementsTxt(depth int, path string, pkgs map[api.PkgName]bool) {
	if depth > 10 {
		util.Die("Too many -r redirects in %s", path)
	}

	var lines []string

	handle, err := os.OpenFile(path, os.O_RDWR, 0644)
	if err != nil {
		util.Die("%s", err.Error())
	}
	defer handle.Close()

	{
		scanner := bufio.NewScanner(handle)
		for scanner.Scan() {
			line := strings.TrimSpace(scanner.Text())

			if name, _, found := findPackage(line); found && pkgs[normalizePackageName(*name)] {
				continue
			} else if nextfile, found := strings.CutPrefix(line, "-r "); found {
				recurseRemoveFromRequirementsTxt(depth+1, nextfile, pkgs)
				lines = append(lines, line)
			} else {
				lines = append(lines, line)
			}
		}
	}

	err = handle.Truncate(0)
	if err != nil {
		util.Die("%s", err.Error())
	}

	// Truncate does not reset the cursor position
	_, err = handle.Seek(0, 0)
	if err != nil {
		util.Die("%s", err.Error())
	}

	for _, line := range lines {
		_, err := handle.WriteString(line + "\n")
		if err != nil {
			util.Die("%s", err.Error())
		}
	}
}

func RemoveFromRequirementsTxt(path string, pkgs map[api.PkgName]bool) {
	recurseRemoveFromRequirementsTxt(0, path, pkgs)
}
