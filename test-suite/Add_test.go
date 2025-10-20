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
		case "nodejs-npm":
			fallthrough
		case "nodejs-pnpm":
			fallthrough
		case "nodejs-yarn":
			fallthrough
		case "bun":
			pkgs = []string{"lodash", "react", "@replit/protocol"}

		case "python3-poetry", "python3-pip", "python3-uv":
			pkgs = []string{"replit-ai", "flask >=2", "pyyaml", "discord-py 2.3.2"}

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

		if tmpl != "no-deps" {
			bt.Subtest(tmpl, func(bt testUtils.BackendT) {
				if bt.Backend.QuirksIsReproducible() {
					bt.Subtest("locked", func(bt testUtils.BackendT) {
						bt.Subtest("each", func(bt testUtils.BackendT) {
							bt.AddTestDir(tmpl)
							for _, pkg := range pkgs {
								bt.UpmAdd(pkg)
							}
						})

						bt.Subtest("all", func(bt testUtils.BackendT) {
							bt.AddTestDir(tmpl)
							bt.UpmAdd(pkgs...)
						})
					})
				}

				bt.Subtest("unlocked", func(bt testUtils.BackendT) {
					bt.Subtest("each", func(bt testUtils.BackendT) {
						bt.AddTestDir(tmpl)
						for _, pkg := range pkgs {
							bt.UpmAdd(pkg)
						}
					})

					bt.Subtest("all", func(bt testUtils.BackendT) {
						bt.AddTestDir(tmpl)
						bt.UpmAdd(pkgs...)
					})
				})
			})
		}
	}
}

func TestAddSkipAudit(t *testing.T) {
	for _, bt := range languageBackends {
		bt.Start(t)

		// Only test skip-audit for npm since it supports --no-audit
		switch bt.Backend.Name {
		case "nodejs-npm":
			bt.Subtest(bt.Backend.Name, func(bt testUtils.BackendT) {
				bt.Subtest("skip-audit-flag", func(bt testUtils.BackendT) {
					bt.AddTestDir("no-deps")
					
					// Test that --skip-audit flag is accepted and works
					_, err := bt.Exec(
						"upm",
						"--lang",
						bt.Backend.Name,
						"add",
						"--skip-audit",
						"lodash",
					)

					if err != nil {
						bt.Fail("upm add with --skip-audit failed: %v", err)
					}

					// Verify the package was actually added
					afterSpecDeps := bt.UpmListSpecFile()
					found := false
					for _, dep := range afterSpecDeps {
						if dep.Name == "lodash" {
							found = true
							break
						}
					}

					if !found {
						bt.Fail("expected lodash in spec file after add with --skip-audit")
					}
				})
			})
		default:
			// Skip other backends as they don't support --skip-audit
			continue
		}
	}
}
