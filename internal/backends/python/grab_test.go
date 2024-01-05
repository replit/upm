package python

import (
	"context"
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
	}

	content := `
import ast
from sys import argv
from Django import forms
from Flask import Flask
import replit
import replit.ai
import foo #upm package(bar)
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

func TestLocalModules(t *testing.T) {
	expected := "Hello, world!"

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

	lastDir, err := os.Getwd()
	if err != nil {
		t.Fatal("unable to get cwd", err)
	}
	err = os.Chdir(testDir)
	if err != nil {
		t.Fatal("unable to get cwd", err)
	}
	defer os.Chdir(lastDir)

	// Sanity test, actually run Python in the environment.
	cmd := exec.Command("bash", "-c", "poetry lock -n; poetry install; poetry run python -m foo")
	stdout, err := cmd.Output()
	if err != nil {
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

	expectedPackages := []string{"flask"}
	for _, test := range expectedPackages {
		if _, ok := pkgs[test]; ok {
			delete(pkgs, test)
		}
	}

	unexpectedPkgs := []string{}
	for extra := range pkgs {
		unexpectedPkgs = append(unexpectedPkgs, extra)
	}

	if len(pkgs) > 0 {
		t.Fatal("guessed extra unexpected packages: ", strings.Join(unexpectedPkgs, ", "))
	}
}
