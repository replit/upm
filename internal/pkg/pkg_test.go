package pkg

import (
	"strings"
	"testing"

	"github.com/replit/upm/internal/api"
)

func simpleNormalizePackageName(name api.PkgName) api.PkgName {
	return api.PkgName(strings.ToLower(string(name)))
}

func TestSortPrefixSuffix(t *testing.T) {
	names := []string{
		"borp-foo",
		"beep-foo",
		"foo-bar",
		"a-foo-b",
		"unrelated",
		"c-foo-d",
		"foo",
		"also-unrelated",
		"foo2",
		"foo-baz",
		"foo-blix",
	}

	pkgs := []api.PkgInfo{}

	for _, name := range names {
		pkgs = append(pkgs, api.PkgInfo{
			Name: name,
		})
	}

	sorted := SortPrefixSuffix(simpleNormalizePackageName)("foo", pkgs)

	expected := []string{
		"foo",
		"foo-bar",
		"foo2",
		"foo-baz",
		"foo-blix",
		"borp-foo",
		"beep-foo",
		"a-foo-b",
		"c-foo-d",
		"unrelated",
		"also-unrelated",
	}

	for idx, pkg := range sorted {
		if string(pkg.Name) != expected[idx] {
			t.Errorf("Unexpected package ordering: %s != %s", pkg.Name, expected[idx])
		}
	}
}
