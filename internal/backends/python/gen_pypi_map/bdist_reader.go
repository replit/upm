package main

import (
	"path"
	"strings"
)

func ValidatePackage(name string, packageMap map[string]bool) bool {
	// If we've already validated this package, don't do it again
	if packageMap[name] {
		return true
	}

	dir := path.Dir(name)

	// Top level packages and modules are always valid
	if dir == "." {
		packageMap[name] = true
		return true
	}

	// Nested packages are only valid if they are in valid packages
	packageMap[name] = ValidatePackage(dir, packageMap)
	return packageMap[name]
}

// Bdists are packaged such that they can be directly unzipped onto the python
// path to install them. As a consequence, the directory structure of a bdist
// is identical to the exposed modules
func ExtractBdist(reader ArchiveReader) ([]string, error) {
	// Make a map of all possible modules in the archive (.py files or directories
	// containing an __init__.py). This map will have false positives because
	// sometimes a python file isn't actually a module (details below).
	modules := make(map[string]bool, 0)
	for reader.Next() {
		name := reader.File()

		if path.Ext(name) == ".py" {
			dir := path.Dir(name)

			if path.Base(name) == "__init__.py" {
				// If this is a package, add it
				modules[dir] = false
			} else {
				// If this is a top level module, add it
				module := strings.TrimSuffix(name, ".py")
				modules[module] = false
			}
		}
	}

	// Move modules into return array. A python file that is in a directory that
	// does not contain an __init__ module is not an acessible module, unless
	// that module is in the top level of the dist.
	ret := make([]string, 0, len(modules))
	for pkg, _ := range modules {
		valid := ValidatePackage(pkg, modules)
		if valid {
			ret = append(ret, pkg)
		}
	}
	return ret, nil
}
