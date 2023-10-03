//go:generate statik -src=./templates

package testSuite

import (
	"net/http"

	"github.com/rakyll/statik/fs"
	"github.com/replit/upm/internal/backends"
	_ "github.com/replit/upm/test-suite/statik"
	testUtils "github.com/replit/upm/test-suite/utils"
)

var languageBackends []testUtils.BackendT

var statikFS http.FileSystem

var standardTemplates = []string{
	"no-deps",
	"one-dep",
	"many-deps",
}

func init() {
	var err error
	statikFS, err = fs.New()

	if err != nil {
		panic(err)
	}

	backends.SetupAll()

	for _, bn := range backends.GetBackendNames() {
		bt := testUtils.InitBackendT(backends.GetBackend(bn), &statikFS)
		languageBackends = append(languageBackends, bt)
	}
}
