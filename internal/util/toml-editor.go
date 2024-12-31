package util

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os/exec"
)

// TomlEditorOp is the format of the JSON sent to toml-editor
type TomlEditorOp struct {
	Op              string `json:"op"`
	Path            string `json:"path,omitempty"`
	TableHeaderPath string `json:"table_header_path,omitempty"`
	Value           string `json:"value,omitempty"`
}

// TomlEditorResponse is the format of the JSON sent from toml-editor
type TomlEditorResponse struct {
	Status  string        `json:"status"`
	Message string        `json:"message"`
	Results []interface{} `json:"results"`
}

func TomlEditorIsAvailable() bool {
	_, err := exec.LookPath("toml-editor")
	return err == nil
}

func ExecTomlEditor(tomlPath string, ops []TomlEditorOp) (*TomlEditorResponse, error) {
	cmd := exec.Command("toml-editor", "--path", tomlPath)
	stdin, err := cmd.StdinPipe()
	if err != nil {
		return nil, fmt.Errorf("toml-editor error: %s", err)
	}
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return nil, fmt.Errorf("toml-editor error: %s", err)
	}
	err = cmd.Start()
	if err != nil {
		return nil, fmt.Errorf("toml-editor error: %s", err)
	}

	encoder := json.NewEncoder(stdin)
	err = encoder.Encode(ops)
	if err != nil {
		return nil, fmt.Errorf("toml-editor error: %s", err)
	}
	decoder := json.NewDecoder(stdout)
	var tomlEditorResponse TomlEditorResponse
	err = decoder.Decode(&tomlEditorResponse)
	if err != nil {
		if !errors.Is(err, io.EOF) {
			return nil, fmt.Errorf("unexpected toml-editor output: %s", err)
		}
	}
	if tomlEditorResponse.Status != "success" {
		input, _ := json.Marshal(ops)
		return nil, fmt.Errorf("toml-editor error with input %s: %s", input, tomlEditorResponse.Message)
	}

	stdout.Close()
	err = stdin.Close()
	if err != nil {
		return nil, fmt.Errorf("toml-editor error: %s", err)
	}

	err = cmd.Wait()
	if err != nil {
		input, _ := json.Marshal(ops)
		return nil, fmt.Errorf("toml-editor error with input %s: %s", input, err)
	}

	return &tomlEditorResponse, nil
}
