//go:generate go run -mod=readonly github.com/rakyll/statik -src=./templates

package testSuite

import (
	"net/http"
	"testing"

	"github.com/rakyll/statik/fs"
	"github.com/replit/upm/internal/backends"
	"github.com/replit/upm/test-suite/backends/bun"
	"github.com/replit/upm/test-suite/backends/npm"
	"github.com/replit/upm/test-suite/backends/yarn"
	_ "github.com/replit/upm/test-suite/statik"
	testUtils "github.com/replit/upm/test-suite/utils"
)

var languageBackends []testUtils.BackendT

var statikFS http.FileSystem

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

func TestWhichLanguage(t *testing.T) {
	for _, bt := range languageBackends {
		bt.Start(t)

		switch bt.Backend.Name {
		case "bun":
			bt.Subtest("bun", bun.TestWhichLanguage)

		case "nodejs-npm":
			bt.Subtest("nodejs-npm", npm.TestWhichLanguage)

		case "nodejs-pnpm":
			bt.Subtest("nodejs-pnpm", npm.TestWhichLanguage)

		case "nodejs-yarn":
			bt.Subtest("nodejs-yarn", yarn.TestWhichLanguage)
		}
	}
}

func Test_Search(t *testing.T) {
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
			bt.Subtest(bt.Backend.Name, func(bt testUtils.BackendT) {
				bt.Subtest("express", func(bt testUtils.BackendT) {
					bt.UpmSearch("express", "express")
				})

				bt.Subtest("@replit->@replit/crosis", func(bt testUtils.BackendT) {
					bt.UpmSearch("@replit", "@replit/crosis")
				})
			})
		}
	}
}

func Test_Info(t *testing.T) {
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
			bt.Subtest(bt.Backend.Name, func(bt testUtils.BackendT) {
				bt.Subtest("express", func(bt testUtils.BackendT) {
					bt.UpmInfo("express")
				})

				bt.Subtest("@replit/crosis", func(bt testUtils.BackendT) {
					bt.UpmInfo("@replit/crosis")
				})
			})
		}
	}
}

func Test_Add(t *testing.T) {
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
			bt.Subtest(bt.Backend.Name, npm.TestAdd)
		}
	}
}

func Test_Remove(t *testing.T) {
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
			bt.Subtest(bt.Backend.Name, npm.TestRemove)
		}
	}
}

func Test_Lock(t *testing.T) {
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
			bt.Subtest(bt.Backend.Name, npm.TestLock)
		}
	}
}

func Test_Install(t *testing.T) {
}

func Test_List(t *testing.T) {
}

func Test_Guess(t *testing.T) {
}
