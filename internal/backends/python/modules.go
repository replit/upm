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
func (state *moduleState) isModuleLocalToRoot(pkg string, root string) bool {
	components := strings.Split(pkg, ".")
	dir := root
	found := true
	for _, nextComponent := range components {
		found = state.isModuleComponent(dir, nextComponent)
		if !found {
			break
		}
		dir = path.Join(dir, nextComponent)
	}
	return found
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
