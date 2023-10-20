package testSuite

import (
	"testing"

	testUtils "github.com/replit/upm/test-suite/utils"
)

func TestWhichLanguage(t *testing.T) {
	defaults := map[string]bool{
		"bun": true,
	}

	for _, bt := range languageBackends {
		bt.Start(t)

		if bt.Backend.Name != "bun" && bt.Backend.Name != "nodejs-pnpm" && bt.Backend.Name != "nodejs-yarn" {
			t.Run(bt.Backend.Name, func(t *testing.T) {
				t.Skip("no test")
			})
			continue
		}

		template := bt.Backend.Name + "/one-dep/"

		bt.Subtest(bt.Backend.Name, func(bt testUtils.BackendT) {
			bt.Subtest("locked", func(bt testUtils.BackendT) {
				bt.AddTestFile(template+bt.Backend.Specfile, bt.Backend.Specfile)
				bt.AddTestFile(template+bt.Backend.Lockfile, bt.Backend.Lockfile)
				bt.UpmWhichLanguage()
			})

			if defaults[bt.Backend.Name] {
				bt.Subtest("default", func(bt testUtils.BackendT) {
					bt.AddTestFile(template+bt.Backend.Specfile, bt.Backend.Specfile)
					bt.UpmWhichLanguage()
				})
			}
		})
	}
}
