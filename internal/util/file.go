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

func TryWriteAtomic(filename string, contents []byte) {
	if err1 := atomic.WriteFile(filename, bytes.NewReader(contents)); err1 != nil {
		if err2 := ioutil.WriteFile(filename, contents, 0666); err2 != nil {
			Die("%s: %s; on non-atomic retry: %s", filename, err1, err2)
		}
	}
}

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

func PatternExists(pattern string) bool {
	if matches, err := filepath.Glob(pattern); err != nil {
		panic(err)
	} else {
		return len(matches) > 0
	}
}

func SearchRecursive(expr string, patterns []string) [][]string {
	r, err := regexp.Compile(expr)
	if err != nil {
		panic(err)
	}

	matches := [][]string{}
	filepath.Walk(".", func(path string, info os.FileInfo, err error) error {
		if err != nil {
			Die("%s: %s", path, err)
		}
		for _, name := range []string{
			".bundle",
			".cache",
			".cask",
			".config",
			".git",
			".local",
			".next",
			".npm",
			"__generated__",
			"__pycache__",
			"__tests__",
			"node_modules",
			"test",
			"vendor",
		} {
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

// https://golangcode.com/download-a-file-from-a-url/
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
