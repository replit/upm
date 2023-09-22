package npm

import (
	testUtils "github.com/replit/upm/test-suite/utils"
)

func TestWhichLanguage(t testUtils.BackendT) {
	t.AddTestFile("/javascript/no-deps/package.json", "package.json")
	t.AddTestFile("/javascript/no-deps/package-lock.json", "package-lock.json")

	detected, err := t.UpmWhichLanguage()
	if err != nil {
		t.T.Fatalf("failed to detect language: %v", err)
	}

	if detected != "nodejs-npm" {
		t.T.Fatalf("expected nodejs-npm, got %s", detected)
	}
}
