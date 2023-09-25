package testSuite

import (
	"testing"

	testUtils "github.com/replit/upm/test-suite/utils"
)

func TestList(t *testing.T) {
	for _, bt := range languageBackends {
		bt.Start(t)

		var templatesToPackages map[string][]string
		switch bt.Backend.Name {
		case "bun":
			fallthrough
		case "nodejs-npm":
			fallthrough
		case "nodejs-pnpm":
			fallthrough
		case "nodejs-yarn":
			templatesToPackages = map[string][]string{
				"no-deps": {},
				"one-dep": {"express"},
				"many-deps": {
					"enquirer",
					"eslint",
					"express",
					"svelte",
					"vite",
					"@codemirror/state",
				},
			}
		}

		bt.Subtest(bt.Backend.Name, func(bt testUtils.BackendT) {
			doList(bt, templatesToPackages)
		})
	}
}

func doList(bt testUtils.BackendT, templatesToPackages map[string][]string) {
	for tmpl, expectPkgs := range templatesToPackages {
		template := "/" + bt.Backend.Name + "/" + tmpl + "/"

		bt.Subtest(tmpl, func(bt testUtils.BackendT) {
			bt.AddTestFile(template+bt.Backend.Specfile, bt.Backend.Specfile)
			if tmpl != "no-deps" {
				bt.AddTestFile(template+bt.Backend.Lockfile, bt.Backend.Lockfile)
			}

			specs := bt.UpmListSpecFile()
			if len(specs) != len(expectPkgs) {
				bt.Fail("Expected %v packages, got %v", len(expectPkgs), len(specs))
			}

			locks := bt.UpmListLockFile()
			if len(locks) < len(expectPkgs) {
				bt.Fail("Expected %v packages, got %v", len(expectPkgs), len(locks))
			}

			for _, pkg := range expectPkgs {
				found := false
				for _, spec := range specs {
					if pkg == spec.Name {
						found = true
						break
					}
				}

				if !found {
					bt.Fail("package %v not found in spec list", pkg)
				}

				found = false
				for _, lock := range locks {
					if pkg == lock.Name {
						found = true
						break
					}
				}

				if !found {
					bt.Fail("package %v not found in lock list", pkg)
				}
			}
		})
	}
}
