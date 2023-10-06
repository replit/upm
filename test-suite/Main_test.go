//go:generate statik -src=./templates

package testSuite

import (
	"embed"

	"github.com/replit/upm/internal/backends"
	testUtils "github.com/replit/upm/test-suite/utils"
)

var languageBackends []testUtils.BackendT

var testData embed.FS

var standardTemplates = []string{
	"no-deps",
	"one-dep",
	"many-deps",
}

func init() {
	backends.SetupAll()

	for _, bn := range backends.GetBackendNames() {
		bt := testUtils.InitBackendT(backends.GetBackend(bn), &testData)
		languageBackends = append(languageBackends, bt)
	}
}
