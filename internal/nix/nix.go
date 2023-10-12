package nix

import (
	_ "embed"
	"encoding/json"
	"log"
	"os"
	"path"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

type ReplitNixAdd struct {
	Deps              []string `json:"deps,omitempty"`
	PythonLibraryDeps []string `json:"libdeps,omitempty"`
}

var (
	python_map_var map[string]ReplitNixAdd

	python_map_var_loaded bool

	//go:embed python_map.json
	python_map_json []byte
)

func DefaultInstallReplitNixSystemDependencies([]api.PkgName) {
	// do nothing by default, if the language doesn't implement a system
	// dependency mapping, there is no work to be done.
}

func python_map() map[string]ReplitNixAdd {
	if python_map_var_loaded {
		return python_map_var
	}
	err := json.Unmarshal(python_map_json, &python_map_var)
	if err != nil {
		log.Fatal("Error during Unmarshal(): ", err)
	}
	python_map_var_loaded = true
	return python_map_var
}

func PythonNixDeps(pack string) ReplitNixAdd {
	val, ok := python_map()[pack]
	if !ok {
		return ReplitNixAdd{}
	}
	return val
}

func ReplitNixAddToNixEditorCmds(replitNixAdd ReplitNixAdd) [][]string {
	result := [][]string{}
	repl_home := os.Getenv("REPL_HOME")
	if repl_home == "" {
		util.Die("REPL_HOME was not set")
	}
	path := path.Join(repl_home, "replit.nix")

	for _, dep := range replitNixAdd.Deps {
		result = append(result, []string{"nix-editor", "--path", path, "--add", dep})
	}

	for _, dep := range replitNixAdd.PythonLibraryDeps {
		result = append(result, []string{"nix-editor", "--path", path, "--dep-type", "python", "--add", dep})
	}

	return result
}

func RunNixEditorCmds(cmds [][]string) {
	for _, cmd := range cmds {
		output := util.GetCmdOutput(cmd)

		var nixEditorStatus struct {
			Status string
			Data   string
		}

		if err := json.Unmarshal(output, &nixEditorStatus); err != nil {
			util.Die("unexpected nix-editor output: %s", err)
		}
		if nixEditorStatus.Status != "success" {
			util.Die("nix-editor error: %s", nixEditorStatus.Data)
		}
		// otherwise we have success and don't need to output
		// anything
	}
}
