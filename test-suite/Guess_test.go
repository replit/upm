package testSuite

import (
	"io"
	"strings"
	"testing"

	"github.com/replit/upm/test-suite/templates"
	testUtils "github.com/replit/upm/test-suite/utils"
)

func TestGuess(t *testing.T) {
	for _, bt := range languageBackends {
		bt.Start(t)

		tests := make(map[string]map[string][]string)
		switch bt.Backend.Name {
		case "nodejs-npm":
			fallthrough
		case "nodejs-pnpm":
			fallthrough
		case "nodejs-yarn":
			fallthrough
		case "bun":
			for _, ext := range []string{"js", "jsx", "ts", "tsx"} {
				_, ok := tests[ext]
				if !ok {
					tests[ext] = make(map[string][]string)
				}

				tests[ext]["js"] = []string{
					"basic",
					"dedup",
					"nested",
					"esparse-fail",
					"require",
				}
			}

			for _, ext := range []string{"ts", "tsx"} {
				tests[ext]["ts"] = []string{
					"typeImports",
				}
			}

		case "python3-poetry", "python3-pip", "python3-uv":
			for _, ext := range []string{"py"} {
				_, ok := tests[ext]
				if !ok {
					tests[ext] = make(map[string][]string)
				}

				tests[ext]["py"] = []string{
					"basic",
					"dedup",
					"replit-packages",
				}
			}

		default:
			t.Run(bt.Backend.Name, func(t *testing.T) {
				t.Skip("no test")
			})
			continue
		}

		for ext, guessSuites := range tests {
			bt.Subtest(ext, func(bt testUtils.BackendT) {
				for template, tests := range guessSuites {
					bt.Subtest(template, func(bt testUtils.BackendT) {
						for _, test := range tests {
							testSrcFile := "guess/" + template + "/" + test

							bt.Subtest(test, func(bt testUtils.BackendT) {
								bt.AddTestFile(testSrcFile, test+"."+ext)
								bt.AddTestDir("no-deps")

								expectFile, err := templates.FS.Open(testSrcFile + ".expect")
								if err != nil {
									bt.Fail("No expect file found for %s: %v", testSrcFile, err)
								}

								var expectsText strings.Builder
								_, err = io.Copy(&expectsText, expectFile)
								if err != nil {
									bt.Fail("Failed to read expect file for %s: %v", testSrcFile, err)
								}

								expects := strings.Split(strings.TrimSpace(expectsText.String()), "\n")

								bt.UpmGuess(expects...)
							})
						}
					})
				}
			})
		}
	}
}
