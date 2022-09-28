package main

import (
	"encoding/json"
	"os"
	"os/exec"
)

type InstallDiffResponse struct {
	Modules []string `json:"modules"`
	Error   string   `json:"error"`
}

func InstallDiff(metadata PackageData) ([]string, error) {
	path := "/tmp/pypi/" + metadata.Info.Name

	// Create a virtual enviornment to install the package to
	cmd := exec.Command("python3", "-m", "venv", path)
	err := cmd.Run()

	if err != nil {
		return nil, PypiError{InstallFailure, "Failed to create venv", err}
	}

	// Run a python script to find the newly installed modules
	cmd = exec.Command("sh", "-c", ". "+path+"/bin/activate; python3 ./gen_pypi_map/install_diff.py "+metadata.Info.Name)

	cmdReader, err := cmd.StdoutPipe()

	if err != nil {
		return nil, PypiError{InstallFailure, "Failed to redirect stdout", err}
	}

	err = cmd.Start()
	if err != nil {
		return nil, PypiError{InstallFailure, "Failed to start installer", err}
	}

	decoder := json.NewDecoder(cmdReader)

	var r InstallDiffResponse
	decoder.Decode(&r)

	err = cmd.Wait()
	if err != nil {
		return nil, PypiError{InstallFailure, "Installer failed", err}
	}

	os.RemoveAll(path)

	if r.Error != "" {
		return nil, PypiError{InstallFailure, r.Error, nil}
	}

	return r.Modules, nil
}
