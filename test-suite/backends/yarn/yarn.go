package yarn

import testUtils "github.com/replit/upm/test-suite/utils"

func TestWhichLanguage(t testUtils.BackendT) {
	t.AddTestFile("/javascript/no-deps/package.json", "package.json")
	t.AddTestFile("/javascript/no-deps/yarn.lock", "yarn.lock")
	t.UpmWhichLanguage("nodejs-yarn")
}
