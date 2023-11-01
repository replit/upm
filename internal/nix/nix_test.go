package nix

import (
	"testing"

	assert "github.com/stretchr/testify/assert"
)

func TestNixPythonMap(t *testing.T) {
	deps := PythonNixDeps("pycairo")

	assert.Equal(t,
		ReplitNixAdd{
			Deps: []string{
				"pkgs.cairo",
				"pkgs.libxcrypt",
				"pkgs.pkg-config",
			},
			PythonLibraryDeps: []string{
				"pkgs.cairo",
				"pkgs.libxcrypt",
			},
		},
		deps)
}

func TestReplitNixAddToNixEditorCmds(t *testing.T) {
	deps := &ReplitNixAdd{
		Deps: []string{
			"pkgs.pkg-config",
			"pkgs.cairo",
		},
		PythonLibraryDeps: []string{
			"pkgs.lib",
		},
	}

	cmds := ReplitNixAddToNixEditorOps(*deps)

	expected := []NixEditorOp{
		{Op: "add", DepType: Regular, Dep: "pkgs.pkg-config"},
		{Op: "add", DepType: Regular, Dep: "pkgs.cairo"},
		{Op: "add", DepType: Python, Dep: "pkgs.lib"},
	}

	assert.Equal(t, expected, cmds)
}
