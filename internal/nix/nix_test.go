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
		},
		deps)
}

func TestReplitNixAddToNixEditorCmds(t *testing.T) {
	deps := &ReplitNixAdd{
		Deps: []string{
			"pkgs.pkg-config",
			"pkgs.cairo",
		},
	}

	cmds := ReplitNixAddToNixEditorOps(*deps)

	expected := []NixEditorOp{
		{Op: "add", DepType: Regular, Dep: "pkgs.pkg-config"},
		{Op: "add", DepType: Regular, Dep: "pkgs.cairo"},
	}

	assert.Equal(t, expected, cmds)
}
