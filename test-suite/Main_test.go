package testSuite

import (
	"github.com/replit/upm/internal/backends"
	"github.com/replit/upm/test-suite/templates"
	testUtils "github.com/replit/upm/test-suite/utils"
)

var languageBackends []testUtils.BackendT

var standardTemplates = []string{
	"no-deps",
	"one-dep",
	"many-deps",
}

func init() {
	backends.SetupAll()

	for _, bn := range backends.GetBackendNames() {
		bt := testUtils.InitBackendT(backends.GetBackend(bn), &templates.FS)
		languageBackends = append(languageBackends, bt)
	}
}
