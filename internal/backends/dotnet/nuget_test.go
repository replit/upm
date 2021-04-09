package dotnet

import (
	"testing"
)

func TestSearchNuget(t *testing.T) {
	pkgs := search("Microsoft.Extensions.Logging")

	if len(pkgs) < 1 {
		t.Error("No results found for Micorosft.Extensions.Logging")
	}

	for _, pkg := range pkgs {
		if pkg.Name == "" {
			t.Errorf("pkg %q has no name", pkg)
		}
		if pkg.Version == "" {
			t.Errorf("pkg %q has no version", pkg)
		}
	}
}

func TestInfoFromNuget(t *testing.T) {
	pkg := info("Microsoft.Extensions.Logging")

	if pkg.Name == "" {
		t.Errorf("pkg %q has no name", pkg)
	}
	if pkg.Version == "" {
		t.Errorf("pkg %q has no version", pkg)
	}
}
