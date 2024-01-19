package main

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

type InstallDiffResponse struct {
	Modules []string `json:"modules"`
	Error   string   `json:"error"`
}

/* discoverValidSuffixes probes the current runtime to discover what Python thinks
 * are importable.
 * This should make this broadly more portable, possibly even to Windows, or other
 * exciting runtimes.
 *
 * The constants here come from:
 *     https://github.com/python/cpython/blob/f59f90b5bccb9e7ac522bc779ab1f6bf11bb4aa3/Lib/modulefinder.py#L272-L274
 * and we should endeavor to keep them in sync.
 */
func discoverValidSuffixes() ([]string, error) {
	script := []string{
		"from importlib.machinery import SOURCE_SUFFIXES, BYTECODE_SUFFIXES, EXTENSION_SUFFIXES",
		"print('\\n'.join(SOURCE_SUFFIXES + BYTECODE_SUFFIXES + EXTENSION_SUFFIXES))",
	}
	cmd := exec.Command("python", "-c", strings.Join(script, "\n"))

	var out bytes.Buffer
	cmd.Stdout = &out

	err := cmd.Run()
	if err != nil {
		return nil, PypiError{InstallFailure, "Installer failed", err}
	}

	res := strings.Split(strings.TrimSpace(out.String()), "\n")

	return res, nil
}

func InstallDiff(metadata PackageData) ([]string, error) {
	root := "/tmp/pypi/" + metadata.Info.Name

	// Run pip to install just the package, so we can statically analyze it
	cmd := exec.Command("pip", "install", "--no-deps", "--target", root, metadata.Info.Name)

	_, err := cmd.StdoutPipe()

	if err != nil {
		return nil, PypiError{InstallFailure, "Failed to redirect stdout", err}
	}

	err = cmd.Run()
	if err != nil {
		return nil, PypiError{InstallFailure, "Failed to start installer", err}
	}

	suffixes, err := discoverValidSuffixes()
	if err != nil {
		return nil, err
	}

	modules := make(map[string]bool)
	err = filepath.WalkDir(root, func(fpath string, entry os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if entry.IsDir() {
			// __pycache__ doesn't seem to have a constant, so we just presume it will
			// never change.
			if strings.HasSuffix(entry.Name(), ".dist-info") || entry.Name() == "__pycache__" {
				return filepath.SkipDir
			}
		}

		for _, suffix := range suffixes {
			if !entry.IsDir() && strings.HasSuffix(entry.Name(), suffix) {
				relpath, _ := filepath.Rel(root, fpath)
				relpath = strings.TrimSuffix(relpath, suffix)
				// Intention here is to trim "special" files, __init__.py and __main__.py, among others
				if dir, last := filepath.Split(relpath); strings.HasPrefix(last, "__") && strings.HasSuffix(last, "__") {
					relpath = dir
				}
				relpath = strings.TrimSuffix(relpath, string(filepath.Separator))
				module := strings.ReplaceAll(relpath, string(filepath.Separator), ".")
				modules[module] = true
			}
		}

		return nil
	})

	os.RemoveAll(root)

	moduleList := []string{}
	for module := range modules {
		moduleList = append(moduleList, module)
	}

	return moduleList, err
}
