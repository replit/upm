package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
)

type DistPackageMetadata struct {
	Packages        []string  `json:"packages"`
	InstallRequires []string  `json:"install_requires"`
	Error           string    `json:"error"`
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
	decoder.Decode(&metadata)

	cmd.Wait()

	if metadata.Error != "" {
		return DistPackageMetadata{}, fmt.Errorf("Error reading setup.py: %v", metadata.Error)
	}

	return metadata, nil
}

func ExtractSdist(reader ArchiveReader) ([]string, error) {
	tempDir, _ := ioutil.TempDir(os.TempDir(), "sdist-")
	defer os.RemoveAll(tempDir)
	reader.Dump(tempDir)

	contents, err := ioutil.ReadDir(tempDir)
	if err != nil {
		return nil, err
	}

	if len(contents) != 1 {
		return nil, fmt.Errorf("SDist should never contain more then one file in root")
	}

	metadata, err := dumpSetupPy(tempDir + "/" + contents[0].Name() + "/setup.py")
	return metadata.Packages, err
}

