package npm

import testUtils "github.com/replit/upm/test-suite/utils"

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
