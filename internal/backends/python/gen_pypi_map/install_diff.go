package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"
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

func pipInstall(pkg string, root string, timeout time.Duration) error {
	// Run pip to install just the package, so we can statically analyze it
	cmd := exec.Command("pip", "install", "--no-deps", "--target", root, pkg)

	if cmd == nil {
		return PypiError{InstallFailure, "Failed to initialize exec.Command", nil}
	}

	killed := false
	installing := true
	go func() {
		start := time.Now()
		for installing {
			elapsed := time.Since(start)
			if elapsed > timeout {
				err := cmd.Process.Signal(os.Interrupt)
				if err == os.ErrProcessDone {
					break
				}
				if err != nil {
					err = cmd.Process.Signal(os.Kill)
				}
				if err == os.ErrProcessDone {
					break
				}
				if err != nil {
					fmt.Fprintf(os.Stderr, "Struggling to kill %d, %v\n", cmd.Process.Pid, err)
				} else {
					killed = true
				}
				break
			}
			time.Sleep(1 * time.Second)
		}
	}()

	_, err := cmd.StdoutPipe()

	if err != nil {
		return PypiError{InstallFailure, "Failed to redirect stdout", err}
	}

	err = cmd.Run()
	if err != nil {
		if killed {
			return PypiError{InstallFailure, "Exceeded timeout", err}
		} else {
			return PypiError{InstallFailure, "Failed to install", err}
		}
	}

	installing = false
	return nil
}

func InstallDiff(metadata PackageData, timeout time.Duration) ([]string, error) {
	root := "/tmp/pypi/" + metadata.Info.Name

	var err error
	for attempts := 5; attempts > 0; attempts -= 1 {
		_ = os.RemoveAll(root)
		err = pipInstall(metadata.Info.Name, root, timeout)
		if err == nil {
			break
		}
	}

	if err != nil {
		return nil, err
	}

	suffixes, err := discoverValidSuffixes()
	if err != nil {
		return nil, err
	}

	var fewestSlashes *int
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

				// Do our best to find the lowest-common module root to avoid ballooning search space
				// A good example of this is flask-mysql, which installs into flaskext.mysql
				currentSlashes := strings.Count(relpath, string(filepath.Separator))
				if fewestSlashes == nil || currentSlashes < *fewestSlashes {
					fewestSlashes = &currentSlashes
					modules = make(map[string]bool)
					modules[module] = true
				} else if currentSlashes == *fewestSlashes {
					modules[module] = true
				}
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
