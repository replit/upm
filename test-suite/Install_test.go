package testSuite

import (
	"os"
	"path"
	"testing"

	testUtils "github.com/replit/upm/test-suite/utils"
)

var testInstalls = map[string]bool{
	"bun":            true,
	"nodejs-npm":     true,
	"nodejs-pnpm":    true,
	"nodejs-yarn":    true,
	"python3-poetry": true,
	"python3-pip":    true,
	"python3-flit":   true,
}

func TestInstall(t *testing.T) {
	for _, bt := range languageBackends {
		bt.Start(t)

		if !testInstalls[bt.Backend.Name] {
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
			if bt.Backend.QuirksIsReproducible() {
				bt.AddTestFile(template+bt.Backend.Lockfile, bt.Backend.Lockfile)
			}

			bt.UpmInstall()

			packageDirPath := bt.UpmPackageDir()

			if !path.IsAbs(packageDirPath) {
				packageDirPath = path.Join(bt.TestDir(), packageDirPath)
			}
			_, err := os.Stat(packageDirPath)
			if err != nil {
				bt.Fail("expected package dir to exist: %v", err)
			}
		})
	}
}
