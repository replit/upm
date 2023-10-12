package nix

import (
	"os"
	"testing"

	assert "github.com/stretchr/testify/assert"
)

func TestNixPythonMap(t *testing.T) {
	deps := PythonNixDeps("pycairo")

	assert.Equal(t,
		&ReplitNixAdd{
			Deps: []string{
				"pkgs.pkg-config",
				"pkgs.cairo",
			},
			PythonLibraryDeps: []string{},
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

	err := os.Setenv("REPL_HOME", "/tmp")
	assert.NoError(t, err)

	cmds := ReplitNixAddToNixEditorCmds(*deps)

	expected := [][]string{
		[]string{"nix-editor", "--path", "/tmp/replit.nix", "--add", "pkgs.pkg-config"},
		[]string{"nix-editor", "--path", "/tmp/replit.nix", "--add", "pkgs.cairo"},
		[]string{"nix-editor", "--path", "/tmp/replit.nix", "--dep-type", "python", "--add", "pkgs.lib"},
	}

	assert.Equal(t, expected, cmds)
}

// func integration test with python add calling nix adds
