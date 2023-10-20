package testSuite

import (
	"os"
	"testing"

	testUtils "github.com/replit/upm/test-suite/utils"
)

func TestInstallReplitNixSystemDependencies(t *testing.T) {
	for _, bt := range languageBackends {
		bt.Start(t)

		bt.Subtest(bt.Backend.Name, doInstallReplitNixSystemDependencies)
	}
}

func doInstallReplitNixSystemDependencies(bt testUtils.BackendT) {
	os.Setenv("REPL_HOME", ".")
	for _, tmpl := range standardTemplates {
		bt.Subtest(tmpl, func(bt testUtils.BackendT) {
			bt.UpmInstallReplitNixSystemDependencies()
		})
	}
}
