package testSuite

import (
	"testing"

	testUtils "github.com/replit/upm/test-suite/utils"
)

func TestSearch(t *testing.T) {
	for _, bt := range languageBackends {
		bt.Start(t)

		switch bt.Backend.Name {
		case "nodejs-npm":
			fallthrough
		case "nodejs-pnpm":
			fallthrough
		case "nodejs-yarn":
			fallthrough
		case "bun":
			doSearch(bt, []searchTest{
				{"express", "express"},
				{"@replit/crosis", "@replit/crosis"},
				{"@replit", "@replit/crosis"},
			})

		case "python3-poetry", "python3-pip":
			doSearch(bt, []searchTest{
				{"flask", "Flask"},
				{"replit-ai", "replit-ai"},
				{"replit", "replit-ai"},
			})

		default:
			t.Run(bt.Backend.Name, func(t *testing.T) {
				t.Skip("no test")
			})
			continue
		}
	}
}

type searchTest struct {
	query    string
	expected string
}

func doSearch(t testUtils.BackendT, tests []searchTest) {
	t.Subtest(t.Backend.Name, func(t testUtils.BackendT) {
		for _, test := range tests {
			t.Subtest(test.query, func(t testUtils.BackendT) {
				t.UpmSearch(test.query, test.expected)
			})
		}
	})
}
