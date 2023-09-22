package npm

import (
	"os"
	"path"

	testUtils "github.com/replit/upm/test-suite/utils"
)

func genAddTest(locked bool, templateName string) (string, func(t testUtils.BackendT)) {
	template := "/javascript/" + templateName + "/"

	return templateName, func(t testUtils.BackendT) {
		t.Subtest("each", func(t testUtils.BackendT) {
			t.AddTestFile(template+"package.json", "package.json")

			if locked {
				t.AddTestFile(template+t.Backend.Lockfile, t.Backend.Lockfile)
			}

			t.UpmAdd("lodash")
			t.UpmAdd("react")
			t.UpmAdd("@replit/protocol")
		})

		t.Subtest("all", func(t testUtils.BackendT) {
			t.AddTestFile(template+"package.json", "package.json")

			if locked {
				t.AddTestFile(template+t.Backend.Lockfile, t.Backend.Lockfile)
			}

			t.UpmAdd("lodash", "react", "@replit/protocol")
		})
	}
}

func genInstallTest(templateName string) (string, func(t testUtils.BackendT)) {
	template := "/javascript/" + templateName + "/"

	return templateName, func(t testUtils.BackendT) {
		t.AddTestFile(template+"package.json", "package.json")
		t.AddTestFile(template+t.Backend.Lockfile, t.Backend.Lockfile)

		t.UpmInstall()

		packageDirPath := t.UpmPackageDir()

		_, err := os.Stat(path.Join(t.TestDir(), packageDirPath))
		if err != nil {
			t.Fail("expected package dir to exist: %v", err)
		}
	}
}

func genListTest(locked bool, templateName string, expects ...string) (string, func(t testUtils.BackendT)) {
	template := "/javascript/" + templateName + "/"

	return templateName, func(t testUtils.BackendT) {
		t.AddTestFile(template+"package.json", "package.json")

		if locked {
			t.AddTestFile(template+t.Backend.Lockfile, t.Backend.Lockfile)
		}

		specs := t.UpmListSpecFile()
		for _, expect := range expects {
			found := false
			for ii, spec := range specs {
				if spec.Name == expect {
					found = true
					specs = append(specs[:ii], specs[ii+1:]...)
					break
				}
			}

			if !found {
				t.Fail("expected %s in spec file", expect)
			}
		}

		if len(specs) > 0 {
			t.Fail("unexpected deps listed in spec file: %v", specs)
		}

		lockDeps := t.UpmListLockFile()
		if !locked && len(lockDeps) > 0 {
			t.Fail("unexpected deps listed in lock file: %v", lockDeps)
		} else if locked && len(lockDeps) == 0 {
			t.Fail("expected deps listed in lock file")
		}
	}
}

func genLockTest(templateName string) (string, func(t testUtils.BackendT)) {
	template := "/javascript/" + templateName + "/"

	return templateName, func(t testUtils.BackendT) {
		t.AddTestFile(template+"package.json", "package.json")

		t.UpmLock()

		afterLockDeps := t.UpmListLockFile()
		afterSpecDeps := t.UpmListSpecFile()

		for _, dep := range afterSpecDeps {
			found := false
			for _, lockDep := range afterLockDeps {
				if dep.Name == lockDep.Name {
					found = true
					break
				}
			}

			if !found {
				t.Fail("expected %s in lock file after lock", dep.Name)
			}
		}
	}
}

func genRemoveTest(locked bool, templateName string, remove ...string) (string, func(t testUtils.BackendT)) {
	template := "/javascript/" + templateName + "/"

	return templateName, func(t testUtils.BackendT) {
		t.Subtest("each", func(t testUtils.BackendT) {
			t.AddTestFile(template+"package.json", "package.json")

			if locked {
				t.AddTestFile(template+t.Backend.Lockfile, t.Backend.Lockfile)
			}

			t.UpmRemove("lodash")
			t.UpmRemove("react")
			t.UpmRemove("@replit/protocol")
		})

		t.Subtest("all", func(t testUtils.BackendT) {
			t.AddTestFile(template+"package.json", "package.json")

			if locked {
				t.AddTestFile(template+t.Backend.Lockfile, t.Backend.Lockfile)
			}

			t.UpmRemove("lodash", "react", "@replit/protocol")
		})
	}
}
