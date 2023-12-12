// Package store handles reading and writing the .upm/store.json file.
// This file is used to cache several things for performance reasons,
// as described in the README.
package store

import (
	"context"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
	"gopkg.in/DataDog/dd-trace-go.v1/ddtrace/tracer"
)

// st is a global object representing the store data. All functions in
// this file read and write to it. Only one store is supported.
var st *store

// currentVersion is the current store schema version. See the Version
// field in the store struct.
const currentVersion = 2

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
	defer func() {
		st.Version = currentVersion
	}()

	filename := getStoreLocation()
	bytes, err := os.ReadFile(filename)

	if err != nil {
		if os.IsNotExist(err) {
			return
		}
		util.Die("%s: %s", filename, err)
	}

	if len(strings.TrimSpace(string(bytes))) > 0 {
		if err = json.Unmarshal(bytes, st); err != nil {
			util.Die("%s: %s", filename, err)
		}
	}

	if st.Version != currentVersion {
		st = &store{}
	}
}

// readMaybe reads the store if it hasn't been read yet.
func readMaybe() {
	if st == nil {
		read()
	}
}

// initLanguage creates an entry in the store for the given language,
// if necessary. (A language is just the name of a backend.)
func initLanguage(language, languageAlias string) {
	if st.Languages == nil {
		st.Languages = map[string]*storeLanguage{}
	}
	if st.Languages[language] == nil && (languageAlias == "" || st.Languages[languageAlias] == nil) {
		st.Languages[language] = &storeLanguage{}
	}
}

func getLanguageCache(language, languageAlias string) *storeLanguage {
	if st.Languages == nil {
		return nil
	}
	if st.Languages[language] != nil {
		return st.Languages[language]
	}
	if languageAlias != "" && st.Languages[languageAlias] != nil {
		return st.Languages[languageAlias]
	}
	return nil
}

// Write writes the current contents of the store from memory back to
// disk. If there is an error, it terminates the process.
func Write(ctx context.Context) {
	//nolint:ineffassign,wastedassign,staticcheck
	span, ctx := tracer.StartSpanFromContext(ctx, "store.Write")
	defer span.Finish()
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

// HasSpecfileChanged returns false if the specfile exists and has not
// changed since the last time UpdateFileHashes was called, or if it
// doesn't exist and it didn't exist last time either. Otherwise, it
// returns true.
func HasSpecfileChanged(b api.LanguageBackend) bool {
	readMaybe()
	initLanguage(b.Name, b.Alias)
	return hashFile(b.Specfile) != getLanguageCache(b.Name, b.Alias).SpecfileHash
}

// HasLockfileChanged returns false if the lockfile exists and has not
// changed since the last time UpdateFileHashes was called, or if it
// doesn't exist and it didn't exist last time either. Otherwise, it
// returns true.
func HasLockfileChanged(b api.LanguageBackend) bool {
	readMaybe()
	initLanguage(b.Name, b.Alias)
	return hashFile(b.Lockfile) != getLanguageCache(b.Name, b.Alias).LockfileHash
}

// GuessWithCache returns b.Guess(), but re-uses a cached return value
// if possible. The cache is used if the matches of b.GuessRegexps
// against b.FilenamePatterns has not changed since the last time
// GuessWithCache was invoked. (This is only possible if the backend
// specifies b.GuessRegexps, which is not always the case. If the
// backend does specify b.GuessRegexps, then the return value of this
// function is cached.) If forceGuess is true, then write to but do
// not read from the cache.
func GuessWithCache(ctx context.Context, b api.LanguageBackend, forceGuess bool) map[api.PkgName]bool {
	span, ctx := tracer.StartSpanFromContext(ctx, "GuessWithCache")
	defer span.Finish()
	readMaybe()
	initLanguage(b.Name, b.Alias)
	cache := getLanguageCache(b.Name, b.Alias)
	old := cache.GuessedImportsHash
	var new hash = "n/a"
	// If no regexps, then we can't hash imports. Skip reading and
	// writing the hash.
	if len(b.GuessRegexps) > 0 {
		new = hashImports(b)
		cache.GuessedImportsHash = new
	}
	if forceGuess || new != old {
		var pkgs map[api.PkgName]bool
		success := true
		if new != "" {
			pkgs, success = b.Guess(ctx)
		} else {
			// If new is the empty string, that means
			// (according to the interface of hashImports)
			// that there were no regexp matches. In that
			// case we shouldn't have any packages
			// returned by the bare imports search. Might
			// as well just skip the search, right?
			pkgs = map[api.PkgName]bool{}
		}
		if !success {
			// If bare imports search is not successful,
			// e.g. due to syntax error, then don't update
			// the hash. This will force the search to be
			// redone next time.
			cache.GuessedImportsHash = old
		}
		// Only cache result if we are going to use the cache,
		// and skip caching if bare imports search was not
		// successful (e.g. due to syntax error). Also, avoid
		// updating the imports cache if search was not
		// successful. You might think this condition isn't
		// needed, but it actually is for the case where you
		// run guess on import A, then on import B with syntax
		// error, then on import A again.
		if len(b.GuessRegexps) > 0 && success {
			guessed := []string{}
			for name := range pkgs {
				guessed = append(guessed, string(name))
			}
			cache.GuessedImports = guessed
		}
		return pkgs
	} else {
		pkgs := map[api.PkgName]bool{}
		for _, name := range cache.GuessedImports {
			pkgs[api.PkgName(name)] = true
		}
		return pkgs
	}
}

// UpdateFileHashes caches the current states of the specfile and
// lockfile. Neither file need exist.
func UpdateFileHashes(ctx context.Context, b api.LanguageBackend) {
	//nolint:ineffassign,wastedassign,staticcheck
	span, ctx := tracer.StartSpanFromContext(ctx, "store.UpdateFileHashes")
	defer span.Finish()
	readMaybe()
	initLanguage(b.Name, b.Alias)
	cache := getLanguageCache(b.Name, b.Alias)
	cache.SpecfileHash = hashFile(b.Specfile)
	cache.LockfileHash = hashFile(b.Lockfile)
}
