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
	defaults := map[string]bool{
		"bun": true,
	}

	templates := map[string]string{
		"bun":         "javascript/no-deps",
		"nodejs-npm":  "javascript/no-deps",
		"nodejs-pnpm": "javascript/no-deps",
		"nodejs-yarn": "javascript/no-deps",
	}

	for _, bt := range languageBackends {
		bt.Start(t)

		bt.Subtest(bt.Backend.Name, func(bt testUtils.BackendT) {
			template, ok := templates[bt.Backend.Name]
			if !ok {
				// TODO: skips
				return
			}

			bt.AddTestFile("/"+template+bt.Backend.Specfile, bt.Backend.Specfile)
			bt.AddTestFile("/"+template+bt.Backend.Lockfile, bt.Backend.Lockfile)

			bt.UpmWhichLanguage()
		})

		if defaults[bt.Backend.Name] {
			bt.Subtest(bt.Backend.Name+" by default", func(bt testUtils.BackendT) {
				template := templates[bt.Backend.Name]

				bt.AddTestFile("/"+template+bt.Backend.Specfile, bt.Backend.Specfile)

				bt.UpmWhichLanguage()
			})
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
	for _, bt := range languageBackends {
		bt.Start(t)

		switch bt.Backend.Name {
		case "bun":
			bt.Subtest(bt.Backend.Name, npm.TestInstall)
		case "nodejs-npm":
			bt.Subtest(bt.Backend.Name, npm.TestInstall)
		case "nodejs-pnpm":
			bt.Subtest(bt.Backend.Name, npm.TestInstall)
		case "nodejs-yarn":
			bt.Subtest(bt.Backend.Name, npm.TestInstall)
		}
	}
}

func Test_List(t *testing.T) {
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

func Test_Guess(t *testing.T) {
}
