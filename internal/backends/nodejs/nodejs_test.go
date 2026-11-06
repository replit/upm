package nodejs

import (
	"context"
	"os"
	"path/filepath"
	"slices"
	"testing"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/store"
)

type TestCase struct {
	scenario    string
	backend     api.LanguageBackend
	fileContent string
	expected    map[string]bool
}

func TestNodejsYarnBackend_Guess(t *testing.T) {
	tcs := []TestCase{
		{
			scenario: "Returns the imports when given a JavaScript file with normal imports",
			backend:  NodejsNPMBackend,
			fileContent: `
			import React from 'react';

			export const App = () => null;
		`,
			expected: map[string]bool{
				"react": true,
			},
		},
		{
			scenario: "Returns normal requires",
			backend:  NodejsNPMBackend,
			fileContent: `
			const request = require('request');
		`,
			expected: map[string]bool{
				"request": true,
			},
		},
		{
			scenario: "Ignore internal imports",
			backend:  NodejsNPMBackend,
			fileContent: `
			const http = require('http');
		`,
			expected: map[string]bool{},
		},
		{
			scenario: "Ignore internal submodules imports",
			backend:  NodejsNPMBackend,
			fileContent: `
			const dns = require('dns/promises');
		`,
			expected: map[string]bool{},
		},
		{
			scenario: "Returns both requires and imports in mixed file",
			backend:  NodejsNPMBackend,
			fileContent: `
			const request = require('request');
			import yargs from 'yargs';
		`,
			expected: map[string]bool{
				"request": true,
				"yargs":   true,
			},
		},
		{
			scenario: "Ignore local file imports",
			backend:  NodejsNPMBackend,
			fileContent: `
			import React from 'react';
			import SomeComponent from './SomeComponent';

			export const App = () => React.createElement(SomeComponent, {});
		`,
			expected: map[string]bool{
				"react": true,
			},
		},
		{
			scenario: "Ignore https file imports",
			backend:  NodejsNPMBackend,
			fileContent: `
			import React from "https://cdn.skypack.dev/react";
			import SomeComponent from './SomeComponent';

			export const App = () => React.createElement(SomeComponent, {});
		`,
			expected: map[string]bool{},
		},
		{
			scenario: "Will process packages in namespace",
			backend:  NodejsNPMBackend,
			fileContent: `
			import React from 'react';
			import { Button } from '@material-ui/core';

			export const App = () => React.createElement(Button, { color: "primary" }, "Hello, World!");
		`,
			expected: map[string]bool{
				"react":             true,
				"@material-ui/core": true,
			},
		},
		{
			scenario: "Conditional require",
			backend:  NodejsNPMBackend,
			fileContent: `
			if (process.env.NODE_ENV === "production") {
				require("node-fetch");
			}
		`,
			expected: map[string]bool{
				"node-fetch": true,
			},
		},
		{
			scenario: "types then conditional require",
			backend:  NodejsNPMBackend,
			fileContent: `
			type Field<T> = { field: string; };

			if (process.env.NODE_ENV === "production") {
				require("node-fetch");
			}
		`,
			expected: map[string]bool{},
		},
		{
			scenario: "dynamic import",
			backend:  NodejsNPMBackend,
			fileContent: `

			if (process.env.NODE_ENV === "production") {
				import("node-fetch");
			}
		`,
			expected: map[string]bool{
				"node-fetch": true,
			},
		},
		{
			scenario: "typings then dynamic import",
			backend:  NodejsNPMBackend,
			fileContent: `
			type Field<T> = { field: string; };

			if (process.env.NODE_ENV === "production") {
				import("node-fetch");
			}
		`,
			expected: map[string]bool{},
		},
	}

	for _, tc := range tcs {
		tc := tc
		t.Run(tc.scenario+"in js", func(t *testing.T) {
			verify(t, tc, "js")

		})

		t.Run(tc.scenario+" in ts", func(t *testing.T) {
			verify(t, tc, "ts")

		})
	}
}

func verify(t *testing.T, tc TestCase, extension string) {
	dir, err := os.MkdirTemp(".", "temp")
	if err != nil {
		t.Error(err)
	}
	defer os.RemoveAll(dir)

	file, err := os.CreateTemp(dir, "*."+extension)
	if err != nil {
		t.Error(err)
	}

	_, err = file.WriteString(tc.fileContent)
	if err != nil {
		t.Error(err)
	}

	result, ok := tc.backend.Guess(context.Background())
	if !ok {
		t.Errorf("Guess return a non true value")
	}

	if len(tc.expected) != len(result) {
		t.Errorf("Expected length of result to match length of expected")
	}

	for key := range tc.expected {
		if _, ok := result[key]; !ok {
			t.Errorf("Key %s not found in result map", key)
		}
	}
}

// TestNodejsGuessRegexps checks that nodejsGuessRegexps recognizes every way
// a dependency can be spelled. These regexps decide whether the bare imports
// search runs at all, so a dependency that they miss is a dependency that
// UPM never sees.
func TestNodejsGuessRegexps(t *testing.T) {
	tcs := []struct {
		scenario string
		content  string
		expected []string
	}{
		{
			scenario: "require with a string literal",
			content:  `const request = require('request');`,
			expected: []string{"request"},
		},
		{
			scenario: "require with a template literal",
			content:  "const request = require(`request`);",
			expected: []string{"request"},
		},
		{
			scenario: "dynamic import with a string literal",
			content:  `const request = import('request');`,
			expected: []string{"request"},
		},
		{
			scenario: "dynamic import with a template literal",
			content:  "const request = import(`request`);",
			expected: []string{"request"},
		},
		{
			scenario: "require with a scoped package and a template literal",
			content:  "const request = require(`@scope/request`);",
			expected: []string{"@scope/request"},
		},
		{
			scenario: "import with a string literal",
			content:  `import React from 'react';`,
			expected: []string{"react"},
		},
		{
			scenario: "side effect import with a string literal",
			content:  `import 'react';`,
			expected: []string{"react"},
		},
		{
			scenario: "template literal with a substitution is not a dependency",
			content:  "const request = require(`request-${version}`);",
			expected: []string{},
		},
		{
			scenario: "template literal on its own is not a dependency",
			content:  "const request = `request`;",
			expected: []string{},
		},
	}

	for _, tc := range tcs {
		t.Run(tc.scenario, func(t *testing.T) {
			matches := []string{}
			for _, r := range nodejsGuessRegexps {
				for _, match := range r.FindAllStringSubmatch(tc.content, -1) {
					matches = append(matches, match[1:]...)
				}
			}
			slices.Sort(matches)

			if !slices.Equal(matches, tc.expected) {
				t.Errorf("Expected matches to be %v, got %v", tc.expected, matches)
			}
		})
	}
}

// TestNodejsGuessBacktickImports checks that dependencies pulled in through a
// template literal are found by the bare imports search.
func TestNodejsGuessBacktickImports(t *testing.T) {
	file, err := os.CreateTemp(".", "backtick-*.js")
	if err != nil {
		t.Fatal(err)
	}
	defer func() {
		if err := os.Remove(file.Name()); err != nil {
			t.Error(err)
		}
	}()

	if _, err := file.WriteString("const request = require(`backtick-request`);\n"); err != nil {
		t.Fatal(err)
	}
	if err := file.Close(); err != nil {
		t.Fatal(err)
	}

	result, ok := NodejsNPMBackend.Guess(context.Background())
	if !ok {
		t.Fatal("Guess returned a non true value")
	}

	if _, ok := result["backtick-request"]; !ok {
		t.Errorf("Key backtick-request not found in result map: %v", result)
	}
}

// TestNodejsGuessWithCacheBacktickImports checks that a project whose only
// dependency is imported through a template literal is still detected. The
// guess regexps decide whether the bare imports search runs at all, so a
// dependency that they miss is a dependency that UPM never sees.
func TestNodejsGuessWithCacheBacktickImports(t *testing.T) {
	projectDir := t.TempDir()
	t.Setenv("UPM_STORE", filepath.Join(t.TempDir(), "store.json"))

	if err := os.WriteFile(
		filepath.Join(projectDir, "index.js"),
		[]byte("const request = require(`backtick-request`);\n"),
		0o644,
	); err != nil {
		t.Fatal(err)
	}

	cwd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	if err := os.Chdir(projectDir); err != nil {
		t.Fatal(err)
	}
	defer func() {
		if err := os.Chdir(cwd); err != nil {
			t.Error(err)
		}
	}()

	guessed := store.GuessWithCache(context.Background(), NodejsNPMBackend, false)
	if _, ok := guessed["backtick-request"]; !ok {
		t.Errorf("Key backtick-request not found in result map: %v", guessed)
	}
}
