package testSuite

import (
	"testing"

	testUtils "github.com/replit/upm/test-suite/utils"
)

var testLocks = map[string]bool{
	"bun":            true,
	"nodejs-npm":     true,
	"nodejs-pnpm":    true,
	"nodejs-yarn":    true,
	"python3-poetry": true,
}

func TestLock(t *testing.T) {
	for _, bt := range languageBackends {
		bt.Start(t)

		if !testLocks[bt.Backend.Name] {
			t.Run(bt.Backend.Name, func(t *testing.T) {
				t.Skip("no test")
			})
			continue
		}

		bt.Subtest(bt.Backend.Name, doLock)
	}
}

func doLock(bt testUtils.BackendT) {
	for _, tmpl := range standardTemplates {
		template := bt.Backend.Name + "/" + tmpl + "/"

		bt.Subtest(tmpl, func(bt testUtils.BackendT) {
			bt.AddTestFile(template+bt.Backend.Specfile, bt.Backend.Specfile)

			specDeps := bt.UpmListSpecFile()

			bt.UpmLock()

			lockDeps := bt.UpmListLockFile()

			for _, specDep := range specDeps {
				found := false
				for _, lockDep := range lockDeps {
					if specDep.Name == lockDep.Name {
						found = true
						break
					}
				}

				if !found {
					bt.Fail("expected %s in lock file", specDep.Name)
				}
			}
		})
	}
}
