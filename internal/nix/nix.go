package nix

import (
	"context"
	_ "embed"
	"encoding/json"
	"errors"
	"io"
	"log"
	"os"
	"os/exec"
	"path"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

type NixEditorDepType string

const (
	Regular NixEditorDepType = "regular"
)

type NixEditorOp struct {
	Op      string           `json:"op"`
	DepType NixEditorDepType `json:"dep_type"`
	Dep     string           `json:"dep"`
}

type ReplitNixAdd struct {
	Deps []string `json:"deps,omitempty"`
}

var (
	python_map_var map[string]ReplitNixAdd

	python_map_var_loaded bool

	//go:embed python_map.json
	python_map_json []byte
)

func DefaultInstallReplitNixSystemDependencies(context.Context, []api.PkgName) {
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

func ReplitNixAddToNixEditorOps(replitNixAdd ReplitNixAdd) []NixEditorOp {
	result := []NixEditorOp{}
	for _, dep := range replitNixAdd.Deps {
		result = append(result, NixEditorOp{Op: "add", Dep: dep, DepType: Regular})
	}
	return result
}

func RunNixEditorOps(ops []NixEditorOp) {
	repl_home := os.Getenv("REPL_HOME")
	if repl_home == "" {
		util.DieInitializationError("REPL_HOME was not set")
	}
	path := path.Join(repl_home, "replit.nix")

	cmd := exec.Command("nix-editor", "--path", path)
	stdin, err := cmd.StdinPipe()
	if err != nil {
		util.Die("couldn't make stdin pipe to nix-editor")
	}
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		util.Die("couldn't make stdout pipe to nix-editor")
	}

	err = cmd.Start()
	if err != nil {
		util.Die("nix-editor error: %s", err)
	}

	encoder := json.NewEncoder(stdin)
	for _, op := range ops {
		err := encoder.Encode(op)
		if err != nil {
			util.DieProtocol("unable to turn op into json: %v error: %s", op, err)
		}
	}
	err = stdin.Close()
	if err != nil {
		util.Die("unable to write to nix-editor")
	}

	decoder := json.NewDecoder(stdout)
	for {
		var nixEditorStatus struct {
			Status string
			Data   string
		}
		err := decoder.Decode(&nixEditorStatus)
		if err != nil {
			if errors.Is(err, io.EOF) {
				break
			}
			util.Die("unexpected nix-editor output: %s", err)
		}
		if nixEditorStatus.Status != "success" {
			util.Die("nix-editor error: %s", nixEditorStatus.Data)
		}
	}
	stdout.Close()

	err = cmd.Wait()
	if err != nil {
		util.Die("nix-editor error: %s", err)
	}
}
