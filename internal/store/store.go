// Package store handles reading and writing the .upm/store.json file.
// This file is used to cache several things for performance reasons,
// as described in the README.
package store

import (
	"encoding/json"
	"io/ioutil"
	"os"
	"path/filepath"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

// st is a global object representing the store data. All functions in
// this file read and write to it. Only one store is supported.
var st *store

// getStoreLocation returns the file path of the JSON store.
func getStoreLocation() string {
	loc, ok := os.LookupEnv("UPM_STORE")
	if ok {
		return loc
	} else {
		return ".upm/store.json"
	}
}

// read reads the store from disk and writes it into the global
// variable st. If there is an error, it terminates the process.
func read() {
	st = &store{}

	filename := getStoreLocation()
	bytes, err := ioutil.ReadFile(filename)

	if err != nil {
		if os.IsNotExist(err) {
			return
		}
		util.Die("%s: %s", filename, err)
	}

	if err = json.Unmarshal(bytes, st); err != nil {
		util.Die("%s: %s", filename, err)
	}
}

// readMaybe reads the store if it hasn't been read yet.
func readMaybe() {
	if st == nil {
		read()
	}
}

// Write writes the current contents of the store from memory back to
// disk. If there is an error, it terminates the process.
func Write() {
	filename := getStoreLocation()

	filename, err := filepath.Abs(filename)
	if err != nil {
		util.Die("%s: %s", filename, err)
	}

	directory, _ := filepath.Split(filename)
	if err := os.MkdirAll(directory, 0777); err != nil {
		util.Die("%s: %s", directory, err)
	}

	content, err := json.Marshal(st)
	if err != nil {
		util.Panicf("Store.Write: %s", err)
	}
	content = append(content, '\n')

	util.TryWriteAtomic(filename, content)
}

// HasSpecfileChanged returns true if the specfile has changed since
// the last time UpdateFileHashes was called. It also returns true if
// UpdateFileHashes has never been called, or if the specfile does not
// exist, or if it did not exist then.
func HasSpecfileChanged(b api.LanguageBackend) bool {
	readMaybe()
	return st.SpecfileHash == "" || hashFile(b.Specfile) != st.SpecfileHash
}

// HasLockfileChanged returns true if the lockfile has changed since
// the last time UpdateFileHashes was called. It also returns true if
// UpdateFileHashes has never been called, or if the lockfile does not
// exist, or if it did not exist then.
func HasLockfileChanged(b api.LanguageBackend) bool {
	readMaybe()
	return st.LockfileHash == "" || hashFile(b.Lockfile) != st.LockfileHash
}

// GuessWithCache returns b.Guess(), but re-uses a cached return value
// if possible. The cache is used if the matches of b.GuessRegexps
// against b.FilenamePatterns has not changed since the last time
// GuessWithCache was invoked. (This is only possible if the backend
// specifies b.GuessRegexps, which is not always the case. If the
// backend does specify b.GuessRegexps, then the return value of this
// function is cached.)
func GuessWithCache(b api.LanguageBackend) map[api.PkgName]bool {
	readMaybe()
	old := st.GuessedImportsHash
	var new hash = "n/a"
	// If no regexps, then we can't hash imports. Skip reading and
	// writing the hash.
	if len(b.GuessRegexps) > 0 {
		new = hashImports(b)
		st.GuessedImportsHash = new
	}
	if new != old {
		pkgs := b.Guess()
		// Only cache result if we are going to use the cache.
		if len(b.GuessRegexps) > 0 {
			st.GuessedImports = []string{}
			for name := range pkgs {
				st.GuessedImports =
					append(st.GuessedImports, string(name))
			}
		}
		return pkgs
	} else {
		pkgs := map[api.PkgName]bool{}
		for _, name := range st.GuessedImports {
			pkgs[api.PkgName(name)] = true
		}
		return pkgs
	}
}

// UpdateFileHashes caches the current states of the specfile and
// lockfile. Neither file need exist.
func UpdateFileHashes(b api.LanguageBackend) {
	readMaybe()
	st.SpecfileHash = hashFile(b.Specfile)
	st.LockfileHash = hashFile(b.Lockfile)
}
