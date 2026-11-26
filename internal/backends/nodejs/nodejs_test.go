package nodejs

import (
	"context"
	"os"
	"testing"

	"github.com/replit/upm/internal/api"
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
			expected: map[string]bool{
				"node-fetch": true,
			},
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
			expected: map[string]bool{
				"node-fetch": true,
			},
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

func TestNodejsGuessRegexpsMatchBacktickImports(t *testing.T) {
	content := "const request = require(`request`);\nconst fetch = import(`node-fetch`);\n"
	expected := map[string]bool{
		"request":    true,
		"node-fetch": true,
	}

	for _, re := range nodejsGuessRegexps {
		for _, match := range re.FindAllStringSubmatch(content, -1) {
			for _, part := range match[1:] {
				delete(expected, part)
			}
		}
	}

	if len(expected) > 0 {
		t.Errorf("Missing imports: %v", expected)
	}
}

func verify(t *testing.T, tc TestCase, extension string) {
	dir := t.TempDir()

	file, err := os.CreateTemp(dir, "*."+extension)
	if err != nil {
		t.Error(err)
	}

	_, err = file.WriteString(tc.fileContent)
	if err != nil {
		t.Error(err)
	}
	if err := file.Close(); err != nil {
		t.Error(err)
	}

	cwd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	if err := os.Chdir(dir); err != nil {
		t.Fatal(err)
	}
	defer func() {
		if err := os.Chdir(cwd); err != nil {
			t.Fatal(err)
		}
	}()

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
