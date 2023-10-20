package testSuite

import (
	"testing"

	testUtils "github.com/replit/upm/test-suite/utils"
)

func TestInfo(t *testing.T) {
	for _, bt := range languageBackends {
		bt.Start(t)

		switch bt.Backend.Name {
		case "nodejs-npm":
			fallthrough
		case "nodejs-yarn":
			fallthrough
		case "bun":
			doInfo(bt, "express", "@replit/crosis")

		default:
			t.Run(bt.Backend.Name, func(t *testing.T) {
				t.Skip("no test")
			})
			continue
		}
	}
}

func doInfo(t testUtils.BackendT, pkgs ...string) {
	t.Subtest(t.Backend.Name, func(t testUtils.BackendT) {
		for _, pkg := range pkgs {
			t.Subtest(pkg, func(t testUtils.BackendT) {
				t.UpmInfo(pkg)
			})
		}
	})
}
