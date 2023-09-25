package npm

import (
	testUtils "github.com/replit/upm/test-suite/utils"
)

func TestGuess(t testUtils.BackendT) {
	for _, lang := range []string{"js", "jsx"} {
		t.Subtest(lang, func(bt testUtils.BackendT) {
			bt.Subtest(genGuessTest("basic", lang, "express", "prosemirror-commands", "prosemirror-model", "@tailwindcss/forms", "@sveltejs/adapter-auto"))
			bt.Subtest(genGuessTest("dedup", lang, "express"))
			bt.Subtest(genGuessTest("nested", lang, "@sveltejs/kit", "ws"))
		})
	}

	for _, lang := range []string{"ts", "tsx"} {
		t.Subtest(lang, func(bt testUtils.BackendT) {
			bt.Subtest(genGuessTest("basic", lang, "express", "prosemirror-commands", "prosemirror-model", "@tailwindcss/forms", "@sveltejs/adapter-auto"))
			bt.Subtest(genGuessTest("dedup", lang, "express"))
			bt.Subtest(genGuessTest("nested", lang, "@sveltejs/kit", "ws"))
			bt.Subtest(genGuessTest("typeImports", lang, "express", "prosemirror-model", "@skeletonlabs/tw-plugin"))
		})
	}
}

func TestInstall(t testUtils.BackendT) {
	t.Subtest(genInstallTest("one-dep"))
	t.Subtest(genInstallTest("many-deps"))
}

func TestList(t testUtils.BackendT) {
	t.Subtest("unlocked", func(bt testUtils.BackendT) {
		t.Subtest(genListTest(false, "no-deps"))
		t.Subtest(genListTest(false, "one-dep", "express"))
		t.Subtest(genListTest(false, "many-deps", "express", "eslint", "svelte", "vite", "enquirer"))
	})
}

func TestLock(t testUtils.BackendT) {
	t.Subtest(genLockTest("no-deps"))
	t.Subtest(genLockTest("one-dep"))
	t.Subtest(genLockTest("many-deps"))
}

func TestRemove(t testUtils.BackendT) {
	t.Subtest("unlocked", genRemoveTest(false, "many-deps", "express", "eslint", "svelte"))
	t.Subtest("locked", genRemoveTest(true, "many-deps", "express", "eslint", "svelte"))
}
