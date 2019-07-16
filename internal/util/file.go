package util

import (
	"bytes"
	"io"
	"io/ioutil"
	"net/http"
	"os"
	"path/filepath"
	"regexp"

	"github.com/natefinch/atomic"
)

// IgnoredPaths is a slice of file patterns that are totally ignored
// by UPM. This means primarily that regexp searches won't descend
// into directories by these names. Gotcha: external tools run by UPM
// won't necessarily respect this list, although UPM makes an effort
// to tell them to when possible.
var IgnoredPaths = []string{
	".bundle",
	".cache",
	".cask",
	".config",
	".git",
	".hg",
	".local",
	".next",
	".npm",
	".svn",
	".tox",
	"__generated__",
	"__pycache__",
	"__tests__",
	"doc",
	"docs",
	"documentation",
	"node_modules",
	"test",
	"tests",
	"vendor",
	"venv",
}

// TryWriteAtomic tries to write contents to filename atomically,
// retrying non-atomically if it can't. If both attempts fail,
// TryWriteAtomic terminates the process.
func TryWriteAtomic(filename string, contents []byte) {
	if err1 := atomic.WriteFile(filename, bytes.NewReader(contents)); err1 != nil {
		if err2 := ioutil.WriteFile(filename, contents, 0666); err2 != nil {
			Die("%s: %s; on non-atomic retry: %s", filename, err1, err2)
		}
	}
}

// FileExists returns true if the given file exists. If an I/O error
// occurs, FileExists terminates the process.
func FileExists(filename string) bool {
	if _, err := os.Stat(filename); os.IsNotExist(err) {
		return false
	} else if err != nil {
		Die("%s: %s", filename, err)
		return false
	} else {
		return true
	}
}

// PatternExists returns true if the given glob matches any file in
// the current directory.
func PatternExists(pattern string) bool {
	if matches, err := filepath.Glob(pattern); err != nil {
		panic(err)
	} else {
		return len(matches) > 0
	}
}

// SearchRecursive does a recursive regexp search in the current
// directory. Only files whose basenames match one of the globs in
// patterns will be searched. The return value is a list of matches as
// would be returned by regexp.FindAllStringSubmatch. Matches are
// returned in a deterministic order. If an I/O error occurs,
// SearchRecursive terminates the process.
func SearchRecursive(r *regexp.Regexp, patterns []string) [][]string {
	matches := [][]string{}
	filepath.Walk(".", func(path string, info os.FileInfo, err error) error {
		if err != nil {
			Die("%s: %s", path, err)
		}
		for _, name := range IgnoredPaths {
			if filepath.Base(path) == name {
				return filepath.SkipDir
			}
		}
		didMatch := false
		for _, pattern := range patterns {
			matched, err := filepath.Match(pattern, filepath.Base(path))
			if err != nil {
				panic(err)
			}
			if matched {
				didMatch = true
				break
			}
		}
		if !didMatch {
			return nil
		}
		if info.Mode().IsRegular() {
			contentsB, err := ioutil.ReadFile(path)
			if err != nil {
				Die("%s: %s", path, err)
			}
			contents := string(contentsB)

			matches = append(matches,
				r.FindAllStringSubmatch(contents, -1)...,
			)
		}
		return nil
	})

	return matches
}

// DownloadFile emulates wget, overwriting any existing file. See
// https://golangcode.com/download-a-file-from-a-url/.
func DownloadFile(filepath string, url string) {
	ProgressMsg("download " + url)
	resp, err := http.Get(url)
	if err != nil {
		Die("%s: %s", url, err)
	}
	defer resp.Body.Close()

	out, err := os.Create(filepath)
	if err != nil {
		Die("%s: %s", filepath, err)
	}
	defer out.Close()

	if _, err := io.Copy(out, resp.Body); err != nil {
		Die("%s: %s", filepath, err)
	}
}
