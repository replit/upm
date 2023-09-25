//go:generate go run -mod=readonly github.com/rakyll/statik -src=./templates

package testSuite

import (
	"net/http"
	"testing"

	"github.com/rakyll/statik/fs"
	"github.com/replit/upm/internal/backends"
	"github.com/replit/upm/test-suite/backends/npm"
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

func TestList(t *testing.T) {
	for _, bt := range languageBackends {
		bt.Start(t)

		switch bt.Backend.Name {
		case "bun":
			fallthrough
		case "nodejs-npm":
			fallthrough
		case "nodejs-pnpm":
			fallthrough
		case "nodejs-yarn":
			bt.Subtest(bt.Backend.Name, npm.TestList)
		}
	}
}

func TestGuess(t *testing.T) {
	for _, bt := range languageBackends {
		bt.Start(t)

		switch bt.Backend.Name {
		case "bun":
			fallthrough
		case "nodejs-npm":
			fallthrough
		case "nodejs-pnpm":
			fallthrough
		case "nodejs-yarn":
			bt.Subtest(bt.Backend.Name, npm.TestGuess)
		}
	}
}
