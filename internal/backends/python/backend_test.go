package python

import (
	"testing"

	assert "github.com/stretchr/testify/assert"

	"github.com/replit/upm/internal/api"
)

func TestNormalizePackageName(t *testing.T) {
	// Take test cases for package normalization from official docs
	// https://packaging.python.org/en/latest/specifications/name-normalization/#normalization
	b := PythonPoetryBackend

	normalizedForm := api.PkgName("friendly-bard")

	cases := []string{
		"Friendly-Bard",
		"FRIENDLY-BARD",
		"friendly.bard",
		"friendly_bard",
		"friendly--bard",
		"FrIeNdLy-._.-bArD",
	}
	for _, name := range cases {
		attempt := b.NormalizePackageName(api.PkgName(name))
		if normalizedForm != attempt {
			t.Errorf("%s != %s (%s)", normalizedForm, attempt, name)
		}
	}
}

func TestNormalizePackageArgs(t *testing.T) {
	// Extra must stay nil unless the argument really carried extras.
	// 'upm add' skips a package that is already in the specfile only when
	// Extra is nil, so an empty-but-not-nil Extra makes it re-add every
	// package, which appends a duplicate line to requirements.txt.
	cases := map[string]api.PkgCoordinates{
		"six":                       {Name: "six", Spec: "", Extra: nil},
		"six==1.16.0":               {Name: "six", Spec: "==1.16.0", Extra: nil},
		"flask >=2":                 {Name: "flask", Spec: ">=2", Extra: nil},
		"requests[security]":        {Name: "requests", Spec: "", Extra: "[security]"},
		"requests[security]>=2.8.1": {Name: "requests", Spec: ">=2.8.1", Extra: "[security]"},
	}

	for arg, expected := range cases {
		want := map[api.PkgName]api.PkgCoordinates{
			normalizePackageName(api.PkgName(expected.Name)): expected,
		}
		assert.Equal(t, want, normalizePackageArgs([]string{arg}), "normalizePackageArgs(%q)", arg)
	}
}
