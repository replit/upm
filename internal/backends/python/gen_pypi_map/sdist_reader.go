package main

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
)

type DistPackageMetadata struct {
	Packages        []string `json:"packages"`
	InstallRequires []string `json:"install_requires"`
	Error           string   `json:"error"`
}

func dumpSetupPy(path string) (DistPackageMetadata, error) {
	// Grab metadata from the file
	cmd := exec.Command("python", "dump_setup.py", path)
	cmdReader, err := cmd.StdoutPipe()

	if err != nil {
		return DistPackageMetadata{}, err
	}

	err = cmd.Start()
	if err != nil {
		return DistPackageMetadata{}, err
	}

	decoder := json.NewDecoder(cmdReader)

	var metadata DistPackageMetadata
	err = decoder.Decode(&metadata)
	if err != nil {
		return DistPackageMetadata{}, err
	}

	err = cmd.Wait()
	if err != nil {
		return DistPackageMetadata{}, err
	}

	if metadata.Error != "" {
		return DistPackageMetadata{}, fmt.Errorf("Error reading setup.py: %v", metadata.Error)
	}

	return metadata, nil
}

func ExtractSdist(reader ArchiveReader) ([]string, error) {
	tempDir, _ := os.MkdirTemp(os.TempDir(), "sdist-")
	defer os.RemoveAll(tempDir)
	err := reader.Dump(tempDir)
	if err != nil {
		return nil, err
	}

	contents, err := os.ReadDir(tempDir)
	if err != nil {
		return nil, err
	}

	if len(contents) != 1 {
		return nil, fmt.Errorf("SDist should never contain more then one file in root")
	}

	metadata, err := dumpSetupPy(tempDir + "/" + contents[0].Name() + "/setup.py")
	return metadata.Packages, err
}
