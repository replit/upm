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
var pep440VersionSpec = `(?:(?:~=|!=|===|==|>=|<=|>|<)\s*[^, ]+)+`
var matchPackageAndSpec = regexp.MustCompile(`(?i)^\s*(` + pep345Name + `)\s*(` + pep440VersionSpec + `)?\s*$`)

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
			sofar, constraints = recurseRequirementsTxt(depth+1, nextfile, sofar, constraints)
		} else if _, found := strings.CutPrefix(line, "-c "); found {
			// ... or constraints files.
			// -c constraints.txt
			//
			// TODO: Accumulate constraints to pass to underlyling pip
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
