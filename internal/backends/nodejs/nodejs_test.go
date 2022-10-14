package nodejs

import (
	"io/ioutil"
	"os"
	"testing"

	"github.com/replit/upm/internal/api"
)

type TestCase struct {
	scenario    string
	backend     api.LanguageBackend
	fileContent string
	expected    map[api.PkgName]bool
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
			expected: map[api.PkgName]bool{
				"react": true,
			},
		},
		{
			scenario: "Returns normal requires",
			backend:  NodejsNPMBackend,
			fileContent: `
			const request = require('request');
		`,
			expected: map[api.PkgName]bool{
				"request": true,
			},
		},
		{
			scenario: "Ignore internal imports",
			backend:  NodejsNPMBackend,
			fileContent: `
			const http = require('http');
		`,
			expected: map[api.PkgName]bool{},
		},
		{
			scenario: "Returns both requires and imports in mixed file",
			backend:  NodejsNPMBackend,
			fileContent: `
			const request = require('request');
			import yargs from 'yargs';
		`,
			expected: map[api.PkgName]bool{
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
			expected: map[api.PkgName]bool{
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
			expected: map[api.PkgName]bool{},
		},
		{
			scenario: "Will process packages in namespace",
			backend:  NodejsNPMBackend,
			fileContent: `
			import React from 'react';
			import { Button } from '@material-ui/core';
			
			export const App = () => React.createElement(Button, { color: "primary" }, "Hello, World!");
		`,
			expected: map[api.PkgName]bool{
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
			expected: map[api.PkgName]bool{
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
			expected: map[api.PkgName]bool{},
		},
		{
			scenario: "dynamic import",
			backend:  NodejsNPMBackend,
			fileContent: `

			if (process.env.NODE_ENV === "production") { 
				import("node-fetch");
			}
		`,
			expected: map[api.PkgName]bool{
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
			expected: map[api.PkgName]bool{},
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
	dir, err := ioutil.TempDir(".", "temp")
	if err != nil {
		t.Error(err)
	}
	defer os.RemoveAll(dir)

	file, err := ioutil.TempFile(dir, "*."+extension)
	if err != nil {
		t.Error(err)
	}

	_, err = file.WriteString(tc.fileContent)
	if err != nil {
		t.Error(err)
	}

	result, ok := tc.backend.Guess()
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
