package util

import (
	"context"
	"os"
	"path"
	"strings"
	"testing"

	"github.com/smacker/go-tree-sitter/python"
)

var importsQuery = `
(module
  [(import_statement
     name: [(dotted_name) @import
            (aliased_import
              name: (dotted_name) @import)])

   (import_from_statement
     module_name: (dotted_name) @import)]

  .

  (comment)? @pragma)
`

func writeFile(dir, name string, contents []byte) error {
	err := os.MkdirAll(dir, 0o755)
	if err != nil {
		return err
	}
	err = os.WriteFile(path.Join(dir, name), contents, 0644)
	if err != nil {
		return err
	}
	return nil
}

func TestTreeSitter(t *testing.T) {
	testDir := t.TempDir()

	expected := map[string]bool{
		"from_root":  true,
		"from_inner": true,
	}

	innerDir := path.Join(testDir, "src", "my_module", "inner")
	venvDir := path.Join(testDir, "venv", "ignored_module")

	var err error
	err = writeFile(testDir, "root.py", []byte("import from_root"))
	if err != nil {
		t.Error(err)
	}
	err = writeFile(innerDir, "inner.py", []byte("import from_inner"))
	if err != nil {
		t.Error(err)
	}
	err = writeFile(venvDir, "ignored.py", []byte("import from_venv"))
	if err != nil {
		t.Error(err)
	}

	pathSegmentPatterns := []string{"*.py"}
	ignorePathSegments := map[string]bool{
		"venv": true,
	}

	ctx := context.Background()
	py := python.GetLanguage()
	foundMap := map[string]bool{}
	{
		var found []string
		found, err = GuessWithTreeSitter(ctx, testDir, py, importsQuery, pathSegmentPatterns, ignorePathSegments)
		if err != nil {
			t.Error(err)
		}

		for _, pkg := range found {
			foundMap[pkg] = true
		}
	}

	for pkg := range foundMap {
		if expected[pkg] {
			delete(expected, pkg)
			delete(foundMap, pkg)
		} else {
			t.Error("Missing match: ", pkg)
		}
	}

	if len(expected) > 0 {
		formatted := []string{}
		for pkg := range expected {
			formatted = append(formatted, pkg)
		}
		t.Error("Not all expected checks were passed. Missing:", strings.Join(formatted, ", "))
	}

	if len(foundMap) > 0 {
		formatted := []string{}
		for pkg := range foundMap {
			formatted = append(formatted, pkg)
		}
		t.Error("Not all expected checks were passed. Extra:", strings.Join(formatted, ", "))
	}
}
