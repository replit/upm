package nodejs

import (
	"fmt"
	"os"
	"testing"
)

func TestParseFile(t *testing.T) {
	expected := map[string]bool{
		"'fs'": true,
		"'assert'": true,
		"'path'": true,
		"\"nan\"": true,
		"'buffer'": true,
		"\"console\"": true,
		"'dns'": true,
		"`child_process`": true,
	}

	content := `
import * as fs from 'fs';
import 'assert';
import { isAbsolute } from 'path';
import nan from "nan";
const foo = require('buffer');
const foo2 = require("console");
require("cluster", "crypto");
require('async_hooks', 'constants');
require('dns',)
require(` + "`child_process`" + `)

foo('bar');

fs.doThing();

const foo = ({
	// this is the reason we switched to tree-sitter
	foo = () => {},
}) => void 0;

process.exit(1);
`

	testDir := t.TempDir()
	testFile := testDir + "/index.js"
	err := os.WriteFile(testFile, []byte(content), 0644)
	fmt.Println("wrote to", testFile)
	if err != nil {
		t.Fatal("failed to write test file", err)
	}

	found, err := findImports(testDir)

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
		t.Errorf("Missing imports: %v", expected)
	}
}
