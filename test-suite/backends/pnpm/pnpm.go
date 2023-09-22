package pnpm

import testUtils "github.com/replit/upm/test-suite/utils"

func TestWhichLanguage(t testUtils.BackendT) {
	t.AddTestFile("/javascript/no-deps/package.json", "package.json")
	t.AddTestFile("/javascript/no-deps/pnpm-lock.yaml", "pnpm-lock.yaml")
	t.UpmWhichLanguage("nodejs-pnpm")
}
