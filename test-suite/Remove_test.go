package testSuite

import (
	"testing"

	testUtils "github.com/replit/upm/test-suite/utils"
)

func TestRemove(t *testing.T) {
	for _, bt := range languageBackends {
		bt.Start(t)

		var pkgsToRemove map[string][]string
		switch bt.Backend.Name {
		case "nodejs-npm":
			fallthrough
		case "nodejs-pnpm":
			fallthrough
		case "nodejs-yarn":
			fallthrough
		case "bun":
			pkgsToRemove = map[string][]string{
				"one-dep":   {"express"},
				"many-deps": {"express", "eslint", "svelte"},
			}

		case "python3-poetry", "python3-pip", "python3-uv":
			pkgsToRemove = map[string][]string{
				"one-dep":   {"django"},
				"many-deps": {"django", "boatman", "ws4py"},
			}

		default:
			t.Run(bt.Backend.Name, func(t *testing.T) {
				t.Skip("no test")
			})
			continue
		}

		bt.Subtest(bt.Backend.Name, func(bt testUtils.BackendT) {
			doRemove(bt, pkgsToRemove)
		})
	}
}

func doRemove(bt testUtils.BackendT, templatesToPkgsToRemove map[string][]string) {
	for tmpl, pkgsToRemove := range templatesToPkgsToRemove {
		if tmpl != "no-deps" {
			bt.Subtest(tmpl, func(bt testUtils.BackendT) {
				if bt.Backend.QuirksIsReproducible() {
					bt.Subtest("locked", func(bt testUtils.BackendT) {
						bt.Subtest("each", func(bt testUtils.BackendT) {
							bt.AddTestDir(tmpl)
							for _, pkg := range pkgsToRemove {
								bt.UpmRemove(pkg)
							}
						})

						bt.Subtest("all", func(bt testUtils.BackendT) {
							bt.AddTestDir(tmpl)
							bt.UpmRemove(pkgsToRemove...)
						})
					})
				}

				if !bt.Backend.QuirkRemoveNeedsLockfile() {
					bt.Subtest("unlocked", func(bt testUtils.BackendT) {
						bt.Subtest("each", func(bt testUtils.BackendT) {
							bt.AddTestDir(tmpl)
							bt.RemoveTestFile(bt.Backend.Lockfile)
							for _, pkg := range pkgsToRemove {
								bt.UpmRemove(pkg)
							}
						})

						bt.Subtest("all", func(bt testUtils.BackendT) {
							bt.AddTestDir(tmpl)
							bt.RemoveTestFile(bt.Backend.Lockfile)
							bt.UpmRemove(pkgsToRemove...)
						})
					})
				}
			})
		}
	}
}
