package python

import (
	"context"
	"os"
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
		if _, ok := expected[string(path)]; ok {
			delete(expected, string(path))
		} else {
			t.Errorf("Unexpected import %s", path)
		}
	}

	if len(expected) > 0 {
		t.Errorf("Missing imports %v", expected)
	}
}
