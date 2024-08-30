package python

import (
	"path"
	"strings"
)

type moduleState struct {
	fileExists   func(path string) bool
	moduleRoots  []string
	packageRoots map[string]string
}

// Test to see if `name` is a python module, relative to `directory`
func (state *moduleState) isModuleComponent(directory string, name string) bool {
	result := state.fileExists(path.Join(directory, name+".py"))
	if !result {
		result = state.fileExists(path.Join(directory, name, "__init__.py"))
	}
	return result
}

// Test to see if a dotted package, `pkg` (eg: foo.bar.baz) is a python module, relative to `root`
// Convention suggests we should only count modules as such if they have __init__.py on
// every level, but in practice (and based on the implementation) this isn't relevant.
// Importantly, AWS codegen'd python modules do not emit intermediate __init__.py files,
// and "extension" libraries that install themselves into the subdirectory of a dependency
// (common in Flask-land, likely others as well) should still be discovered.
func (state *moduleState) isModuleLocalToRoot(pkg string, root string) bool {
	components := strings.Split(pkg, ".")
	last := components[len(components)-1]
	components = components[:len(components)-1]

	return state.isModuleComponent(path.Join(append([]string{root}, components...)...), last)
}

// Test to see if a dotted package, `pkg`, is a python module, relative to any project root
func (state *moduleState) IsLocalModule(pkg string) bool {
	found := false
	if state.packageRoots != nil {
		for localPkg := range state.packageRoots {
			basePkg := strings.SplitN(pkg, ".", 2)[0]
			found = localPkg == basePkg
			if found {
				return found
			}
		}
	}
	if state.moduleRoots != nil {
		for _, root := range state.moduleRoots {
			found = state.isModuleLocalToRoot(pkg, root)
			if found {
				return found
			}
		}
	}
	return found
}
