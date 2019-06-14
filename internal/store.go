package internal

import (
	"encoding/json"
	"io/ioutil"
	"os"
	"path/filepath"
)

func getStoreLocation() string {
	loc, ok := os.LookupEnv("UPM_STORE")
	if ok {
		return loc
	} else {
		return ".upm/store.json"
	}
}

func readStore() store {
	filename := getStoreLocation()
	bytes, err := ioutil.ReadFile(filename)

	if err != nil {
		if os.IsNotExist(err) {
			return store{}
		}
		die("%s: %s", filename, err)
	}

	var store store
	err = json.Unmarshal(bytes, &store)

	if err != nil {
		die("%s: %s", filename, err)
	}

	return store
}

func writeStore(store store) {
	filename := getStoreLocation()

	filename, err := filepath.Abs(filename)
	if err != nil {
		die("%s: %s", filename, err)
	}

	directory, _ := filepath.Split(filename)
	if err := os.MkdirAll(directory, 0777); err != nil {
		die("%s: %s", directory, err)
	}

	content, err := json.MarshalIndent(store, "", "  ")
	if err != nil {
		panicf("writeStore: json.MarshalIndent failed", err)
	}
	content = append(content, '\n')

	tryWriteAtomic(filename, content)
}

func doesStoreSpecfileHashMatch(store store, specfile string) bool {
	return store.LockfileHash != "" && hashFile(specfile) == store.SpecfileHash
}

func doesStoreLockfileHashMatch(store store, lockfile string) bool {
	return store.LockfileHash != "" && hashFile(lockfile) == store.LockfileHash
}

func updateStoreHashes(store store, specfile string, lockfile string) {
	store.SpecfileHash = hashFile(specfile)
	store.LockfileHash = hashFile(lockfile)

	if store.SpecfileHash == "" {
		die("file does not exist: %s", specfile)
	}

	if store.LockfileHash == "" {
		die("file does not exist: %s", lockfile)
	}

	writeStore(store)
}
