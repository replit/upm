package nodejs

import "testing"

func TestParseFile(t *testing.T) {
	expected := []string{
		"fs",
		"assert",
		"path",
		"nan",
		"buffer",
		"console",
		"dns",
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

foo('bar');

fs.doThing();

const foo = ({
	// this is the reason we switched to tree-sitter
	foo = () => {},
}) => void 0;

process.exit(1);
`

	results := make(chan parseResult)
	go parseFile([]byte(content), results)
	result := <-results

	if !result.ok {
		t.Errorf("Parse failed")
	}

	if len(result.importPaths) != len(expected) {
		t.Errorf("Expected %d imports, got %d", len(expected), len(result.importPaths))
	}

	for i, path := range result.importPaths {
		if i >= len(expected) {
			t.Errorf("Unexpected import %s", path)
		} else if path != expected[i] {
			t.Errorf("Expected %s, got %s", expected[i], path)
		}
	}
}
