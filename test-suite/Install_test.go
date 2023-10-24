package testSuite

import (
	"os"
	"path"
	"testing"

	testUtils "github.com/replit/upm/test-suite/utils"
)

func TestInstall(t *testing.T) {
	for _, bt := range languageBackends {
		bt.Start(t)

		if bt.Backend.Name != "bun" && bt.Backend.Name != "nodejs-npm" && bt.Backend.Name != "nodejs-pnpm" && bt.Backend.Name != "nodejs-yarn" {
			t.Run(bt.Backend.Name, func(t *testing.T) {
				t.Skip("no test")
			})
			continue
		}

		bt.Subtest(bt.Backend.Name, doInstall)
	}
}

func doInstall(bt testUtils.BackendT) {
	for _, tmpl := range standardTemplates {
		if tmpl == "no-deps" {
			continue
		}

		template := bt.Backend.Name + "/" + tmpl + "/"
		bt.Subtest(tmpl, func(bt testUtils.BackendT) {
			bt.AddTestFile(template+bt.Backend.Specfile, bt.Backend.Specfile)
			bt.AddTestFile(template+bt.Backend.Lockfile, bt.Backend.Lockfile)

			bt.UpmInstall()

			packageDirPath := bt.UpmPackageDir()

			_, err := os.Stat(path.Join(bt.TestDir(), packageDirPath))
			if err != nil {
				bt.Fail("expected package dir to exist: %v", err)
			}
		})
	}
}
