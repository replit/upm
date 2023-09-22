package bun

import (
	testUtils "github.com/replit/upm/test-suite/utils"
)

func TestWhichLanguage(t testUtils.BackendT) {
	t.Subtest("bun is default package.json backend", func(t testUtils.BackendT) {
		t.AddTestFile("/javascript/no-deps/package.json", "package.json")
		t.UpmWhichLanguage("bun")
	})

	t.Subtest("bun detected with bun.lockb", func(t testUtils.BackendT) {
		t.AddTestFile("/javascript/no-deps/package.json", "package.json")
		// bun deletes the lockfile if there's no deps
		t.AddTestFile("/javascript/one-dep/bun.lockb", "bun.lockb")
		t.UpmWhichLanguage("bun")
	})
}
