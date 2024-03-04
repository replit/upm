package python

import (
	"bytes"
	"context"
	"fmt"
	"os"
	"os/exec"
	"path"
	"strings"
	"testing"
)

func TestParseFile(t *testing.T) {
	expected := map[string]bool{
		"ast":       true,
		"sys":       true,
		"Django":    true,
		"Flask":     true,
		"replit":    true,
		"replit.ai": true,
		"bar":       true,
		"baz":       true,
	}

	content := `
import ast
from sys import argv
from Django import forms
from Flask import Flask
import replit
import replit.ai
import foo #upm package(bar)

if True:
    import baz
`

	testDir := t.TempDir()
	testFile := testDir + "/index.py"
	err := os.WriteFile(testFile, []byte(content), 0o644)
	if err != nil {
		t.Fatal("failed to write test file", err)
	}

	found, err := findImports(context.Background(), testDir)
	if err != nil {
		t.Fatal("Parse failed", err)
	}

	if len(found) != len(expected) {
		t.Errorf("Expected %d imports, got %d", len(expected), len(found))
	}

	for path := range found {
		if _, ok := expected[path]; ok {
			delete(expected, path)
		} else {
			t.Errorf("Unexpected import %s", path)
		}
	}

	if len(expected) > 0 {
		t.Errorf("Missing imports %v", expected)
	}
}

/* TestLocalModules
 *
 * Create a real poetry project in testDir,
 * execute poetry to lock/install that project,
 * then execute Python in the project to see the expected string.
 *
 * Once that is verified, run doGuess inside the project and confirm
 * that we only get expected guesses.
 *
 * Principally, we do _not_ want to see local packages showing up
 * in the output!
 */
func TestLocalModules(t *testing.T) {
	expected := "Hello, world!"

	// Every file and its contents for our project.
	files := map[string]string{
		"README.md": "stub",
		"pyproject.toml": `
[tool.poetry]
name = "grab-test"
version = "0.1.0"
description = ""
authors = []
readme = "README.md"
packages = [{include="foo", from="src"}]

[tool.poetry.dependencies]
python = "^3.11"


[build-system]
requires = ["poetry-core"]
build-backend = "poetry.core.masonry.api"
`,
		"src/foo/__main__.py": `
import foo.bar.baz

print(foo.bar.baz.blix())
`,
		"src/foo/__init__.py":     "",
		"src/foo/bar/__init__.py": "",
		"src/foo/bar/baz.py": `
def blix():
	return "` + expected + `"
`,
		"app.py": ` # TODO(dstewart) Remove this once we re-enable recursion
import foo.bar.baz
import flask
`,
	}

	// Create the project in a temporary directory
	testDir := t.TempDir()
	for fpath, contents := range files {
		testFile := path.Join(testDir, fpath)
		err := os.MkdirAll(path.Dir(testFile), 0o755)
		if err != nil {
			t.Fatal("unable to create directory", err)
		}
		err = os.WriteFile(testFile, []byte(contents), 0o644)
		if err != nil {
			t.Fatal("failed to write test file", err)
		}
	}

	// Running both of the following tests require we
	// are in the root of the project directory.
	lastDir, err := os.Getwd()
	if err != nil {
		t.Fatal("unable to get cwd", err)
	}
	err = os.Chdir(testDir)
	if err != nil {
		t.Fatal("unable to chdir", err)
	}
	defer (func() {
		err := os.Chdir(lastDir)
		if err != nil {
			t.Fatal("unable to chdir", err)
		}
	})()

	// Sanity test, actually run Python in the environment.
	cmd := exec.Command("bash", "-c", "poetry lock -n; poetry install; poetry run python -m foo")

	var stderr bytes.Buffer
	cmd.Stderr = &stderr
	stdout, err := cmd.Output()
	if err != nil {

		fmt.Println("stderr:", stderr.String())
		t.Fatal("failed to execute python", err)
	}
	lines := strings.Split(strings.TrimSpace(string(stdout)), "\n")
	last := lines[len(lines)-1]
	if last != expected {
		t.Errorf("Expected %s to equal %s", last, expected)
	}

	testPypiMap := func(pkg string) (string, bool) {
		return strings.Split(pkg, ".")[0], true
	}
	// Run guess in the directory.
	pkgs, _ := doGuess(context.Background(), testPypiMap)

	// Flask is the sentinel, if we do not see it, something
	// else is wrong.
	expectedPackages := map[string]bool{"flask": true}
	for test := range expectedPackages {
		if _, ok := pkgs[test]; ok {
			delete(expectedPackages, test)
		}
		delete(pkgs, test)
	}

	if len(expectedPackages) > 0 {
		expectedPkgs := []string{}
		for extra := range expectedPackages {
			expectedPkgs = append(expectedPkgs, extra)
		}

		t.Fatal("did not guess expected packages: ", strings.Join(expectedPkgs, ", "))
	}

	if len(pkgs) > 0 {
		unexpectedPkgs := []string{}
		for extra := range pkgs {
			unexpectedPkgs = append(unexpectedPkgs, extra)
		}

		t.Fatal("guessed extra unexpected packages: ", strings.Join(unexpectedPkgs, ", "))
	}
}
