package util

import (
	"bytes"
	"io"
	"io/ioutil"
	"net/http"
	"os"
	"path"
	"path/filepath"
	"regexp"

	"github.com/natefinch/atomic"
	sfs "github.com/rakyll/statik/fs"
	_ "github.com/replit/upm/internal/statik"
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
	"examples",
	"node_modules",
	"test",
	"tests",
	"vendor",
	"venv",
}

// AddIngoredPaths globally appends to the IngoredPaths list.
func AddIngoredPaths(paths []string) {
	IgnoredPaths = append(IgnoredPaths, paths...)
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

// Exists returns true if a directory entry by the given filename
// exists. If an I/O error occurs, FileExists terminates the process.
func Exists(filename string) bool {
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

// TempDir creates and returns the name of temporary directory. If
// creation fails, it terminates the process. The caller is
// responsible for cleaning up the temporary directory afterwards.
func TempDir() string {
	if tempdir, err := ioutil.TempDir("", "upm"); err != nil {
		Die("%s", err)
		return ""
	} else {
		return tempdir
	}
}

// hfs is the statik http.FileSystem, once initialized.
var statikFS *http.FileSystem

// GetResourceBytes is like GetResource but returns a []byte.
func GetResourceBytes(url string) []byte {
	if statikFS == nil {
		if hfs, err := sfs.New(); err != nil {
			panic(err)
		} else {
			statikFS = &hfs
		}
	}
	if bytes, err := sfs.ReadFile(*statikFS, url); err != nil {
		panic(err)
	} else {
		return bytes
	}
}

// GetResource returns a statik resource as a string. Resources are
// inside the resources/ directory of the UPM source repository. url
// is HTTP-style, e.g. "/nodejs/bare-imports.js". The return value is
// the file contents. If the resource does not exist, GetResource
// panics.
func GetResource(url string) string {
	return string(GetResourceBytes(url))
}

// WriteResource writes a statik resource to a temporary directory.
// url is as in GetResource. The file is put inside tempdir, with the
// same basename as from url. If the resource does not exist,
// WriteResource panics. If the write fails, it terminates the
// process. Otherwise, it returns the name of the newly created file.
func WriteResource(url string, tempdir string) string {
	contents := GetResource(url)
	basename := path.Base(url)
	filename := filepath.Join(tempdir, basename)
	if err := ioutil.WriteFile(filename, []byte(contents), 0666); err != nil {
		Die("%s", err)
	}
	return filename
}

// ChdirToUPM traverses upwards in the filesystem from the current
// directory until it finds a directory entry named .upm, and changes
// to the directory containing it. If it doesn't find any such
// directory entry, ChdirToUPM doesn't do anything.  If UPM_PROJECT is
// set, it is used as the directory to which to change.
func ChdirToUPM() {
	if dir := os.Getenv("UPM_PROJECT"); dir != "" {
		if err := os.Chdir(dir); err != nil {
			Die("%s", err)
		}
		return
	}

	cur, err := os.Getwd()
	if err != nil {
		Die("%s", err)
	}
	for {
		if Exists(filepath.Join(cur, ".upm")) {
			if err := os.Chdir(cur); err != nil {
				Die("%s", err)
			}
			return
		}

		next := filepath.Dir(cur)
		if next == cur {
			// We reached the filesystem root without
			// finding .upm.
			return
		}

		cur = next
	}
}
