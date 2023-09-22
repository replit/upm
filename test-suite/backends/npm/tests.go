package npm

import (
	testUtils "github.com/replit/upm/test-suite/utils"
)

func TestAdd(t testUtils.BackendT) {
	t.Subtest("unlocked", func(bt testUtils.BackendT) {
		bt.Subtest(genAddTest(false, "no-deps"))
		bt.Subtest(genAddTest(false, "one-dep"))
		bt.Subtest(genAddTest(false, "many-deps"))
	})

	t.Subtest("locked", func(bt testUtils.BackendT) {
		// don't test locked with no-deps
		// TODO: `upm lock` with 0 deps should be a no-op
		bt.Subtest(genAddTest(true, "one-dep"))
		bt.Subtest(genAddTest(true, "many-deps"))
	})
}

func TestLock(t testUtils.BackendT) {
	t.Subtest(genLockTest("no-deps"))
	t.Subtest(genLockTest("one-dep"))
	t.Subtest(genLockTest("many-deps"))
}

func TestRemove(t testUtils.BackendT) {
	t.Subtest("unlocked", func(bt testUtils.BackendT) {
		bt.Subtest(genRemoveTest(false, "one-dep", "express"))
		bt.Subtest(genRemoveTest(false, "many-deps", "express", "eslint", "svelte"))
	})

	t.Subtest("locked", func(bt testUtils.BackendT) {
		bt.Subtest(genRemoveTest(true, "one-dep", "express"))
		bt.Subtest(genRemoveTest(true, "many-deps", "express", "eslint", "svelte"))
	})
}

func TestWhichLanguage(t testUtils.BackendT) {
	t.AddTestFile("/javascript/no-deps/package.json", "package.json")
	t.AddTestFile("/javascript/no-deps/package-lock.json", "package-lock.json")
	t.UpmWhichLanguage("nodejs-npm")
}
