package testSuite

import (
	"testing"

	testUtils "github.com/replit/upm/test-suite/utils"
)

var testWhichLanguage = map[string]bool{
	"bun":            true,
	"nodejs-npm":     true,
	"nodejs-pnpm":    true,
	"nodejs-yarn":    true,
	"python3-poetry": true,
	"python3-pip":    true,
	"python3-uv":     true,
}

func TestWhichLanguage(t *testing.T) {
	defaults := map[string]bool{
		"bun": true,
	}

	for _, bt := range languageBackends {
		bt.Start(t)

		if !testWhichLanguage[bt.Backend.Name] {
			t.Run(bt.Backend.Name, func(t *testing.T) {
				t.Skip("no test")
			})
			continue
		}

		bt.Subtest(bt.Backend.Name, func(bt testUtils.BackendT) {
			if bt.Backend.QuirksIsReproducible() {
				bt.Subtest("locked", func(bt testUtils.BackendT) {
					bt.AddTestDir("one-dep")
					bt.UpmWhichLanguage()
				})
			} else {
				bt.Subtest("no lockfile", func(bt testUtils.BackendT) {
					bt.AddTestDir("one-dep")
					bt.UpmWhichLanguage()
				})
			}

			if defaults[bt.Backend.Name] {
				bt.Subtest("default", func(bt testUtils.BackendT) {
					bt.AddTestDir("one-dep")
					bt.RemoveTestFile(bt.Backend.Lockfile)
					bt.UpmWhichLanguage()
				})
			}
		})
	}
}
