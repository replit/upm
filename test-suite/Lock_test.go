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

		bt.Subtest(bt.Backend.Name, func(bt testUtils.BackendT) {
			// test absence of lock file
			for _, tmpl := range standardTemplates {
				bt.Subtest(tmpl, func(bt testUtils.BackendT) {
					bt.AddTestDir(tmpl)
					bt.RemoveTestFile(bt.Backend.Lockfile)

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

			// test absence of package dir
			tmpl := "no-package-dir"
			bt.Subtest(bt.Backend.Name+"/"+tmpl, func(bt testUtils.BackendT) {
				bt.AddTestDir(tmpl)
				bt.RemoveTestFile(bt.Backend.Lockfile)

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
		})
	}
}
