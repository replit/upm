package testSuite

import (
	"io"
	"strings"
	"testing"

	testUtils "github.com/replit/upm/test-suite/utils"
)

func TestGuess(t *testing.T) {
	for _, bt := range languageBackends {
		bt.Start(t)

		tests := make(map[string]map[string][]string)
		switch bt.Backend.Name {
		case "bun":
			fallthrough
		case "nodejs-npm":
			fallthrough
		case "nodejs-pnpm":
			fallthrough
		case "nodejs-yarn":
			for _, ext := range []string{"js", "jsx", "ts", "tsx"} {
				_, ok := tests[ext]
				if !ok {
					tests[ext] = make(map[string][]string)
				}

				tests[ext]["js"] = js
			}

			for _, ext := range []string{"ts", "tsx"} {
				tests[ext]["ts"] = ts
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
								bt.AddTestFile("/"+testSrcFile, test+"."+ext)
								bt.AddTestFile("/"+bt.Backend.Name+"/no-deps/"+bt.Backend.Specfile, bt.Backend.Specfile)

								expectFile, err := statikFS.Open("/" + testSrcFile + ".expect")
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

var js = []string{
	"basic",
	"dedup",
	"nested",
}

var ts = []string{
	"typeImports",
}
