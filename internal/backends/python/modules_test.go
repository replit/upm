package python

import (
	"testing"
)

// Presumed executed as python -m app
func TestBareModules(t *testing.T) {
	localFiles := map[string]bool{
		"app.py":              true, // Contains `import foo.bar`
		"foo/__init__.py":     true,
		"foo/bar/__init__.py": true,
	}

	testState := moduleState{
		fileExists:  func(path string) bool { return localFiles[path] },
		moduleRoots: []string{"src", "."},
	}

	for _, pkg := range []string{"foo", "foo.bar"} {
		if !testState.IsLocalModule(pkg) {
			t.Errorf("%s should be a module", pkg)
		}
	}
}
