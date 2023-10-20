package testSuite

import (
	"testing"

	testUtils "github.com/replit/upm/test-suite/utils"
)

func TestAdd(t *testing.T) {
	for _, bt := range languageBackends {
		bt.Start(t)

		var pkgs []string
		switch bt.Backend.Name {
		case "nodejs-yarn":
			fallthrough
		case "bun":
			pkgs = []string{"lodash", "react", "@replit/protocol"}

		default:
			t.Run(bt.Backend.Name, func(t *testing.T) {
				t.Skip("no test")
			})
			continue
		}

		bt.Subtest(bt.Backend.Name, func(bt testUtils.BackendT) {
			doAdd(bt, pkgs...)
		})
	}
}

func doAdd(bt testUtils.BackendT, pkgs ...string) {
	for _, tmpl := range standardTemplates {
		template := bt.Backend.Name + "/" + tmpl + "/"
		bt.Subtest(tmpl, func(bt testUtils.BackendT) {
			if tmpl != "no-deps" {
				bt.Subtest("locked", func(bt testUtils.BackendT) {
					bt.Subtest("each", func(bt testUtils.BackendT) {
						bt.AddTestFile(template+bt.Backend.Specfile, bt.Backend.Specfile)
						bt.AddTestFile(template+bt.Backend.Lockfile, bt.Backend.Lockfile)
						for _, pkg := range pkgs {
							bt.UpmAdd(pkg)
						}
					})

					bt.Subtest("all", func(bt testUtils.BackendT) {
						bt.AddTestFile(template+bt.Backend.Specfile, bt.Backend.Specfile)
						bt.AddTestFile(template+bt.Backend.Lockfile, bt.Backend.Lockfile)
						bt.UpmAdd(pkgs...)
					})
				})
			}

			bt.Subtest("unlocked", func(bt testUtils.BackendT) {
				bt.Subtest("each", func(bt testUtils.BackendT) {
					bt.AddTestFile(template+bt.Backend.Specfile, bt.Backend.Specfile)
					for _, pkg := range pkgs {
						bt.UpmAdd(pkg)
					}
				})

				bt.Subtest("all", func(bt testUtils.BackendT) {
					bt.AddTestFile(template+bt.Backend.Specfile, bt.Backend.Specfile)
					bt.UpmAdd(pkgs...)
				})
			})
		})
	}
}
