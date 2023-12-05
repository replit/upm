package python

import (
	"testing"

	"github.com/replit/upm/internal/api"
)

func TestNormalizePackageName(t *testing.T) {
	// Take test cases for package normalization from official docs
	// https://packaging.python.org/en/latest/specifications/name-normalization/#normalization
	b := Python3Backend

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
