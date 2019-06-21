package store

import (
	"encoding/json"
	"io/ioutil"
	"os"
	"path/filepath"

	"github.com/replit/upm/internal/util"
)

func getStoreLocation() string {
	loc, ok := os.LookupEnv("UPM_STORE")
	if ok {
		return loc
	} else {
		return ".upm/store.json"
	}
}

func Read() Store {
	filename := getStoreLocation()
	bytes, err := ioutil.ReadFile(filename)

	if err != nil {
		if os.IsNotExist(err) {
			return Store{}
		}
		util.Die("%s: %s", filename, err)
	}

	var store Store
	err = json.Unmarshal(bytes, &store)

	if err != nil {
		util.Die("%s: %s", filename, err)
	}

	return store
}

func (st *Store) Write() {
	filename := getStoreLocation()

	filename, err := filepath.Abs(filename)
	if err != nil {
		util.Die("%s: %s", filename, err)
	}

	directory, _ := filepath.Split(filename)
	if err := os.MkdirAll(directory, 0777); err != nil {
		util.Die("%s: %s", directory, err)
	}

	content, err := json.MarshalIndent(st, "", "  ")
	if err != nil {
		util.Panicf("writeStore: json.MarshalIndent failed", err)
	}
	content = append(content, '\n')

	util.TryWriteAtomic(filename, content)
}

func (st *Store) DoesSpecfileHashMatch(specfile string) bool {
	return st.LockfileHash != "" && hashFile(specfile) == st.SpecfileHash
}

func (st *Store) DoesLockfileHashMatch(lockfile string) bool {
	return st.LockfileHash != "" && hashFile(lockfile) == st.LockfileHash
}

func (st *Store) UpdateHashes(specfile string, lockfile string) {
	st.SpecfileHash = hashFile(specfile)
	st.LockfileHash = hashFile(lockfile)

	if st.SpecfileHash == "" {
		util.Die("file does not exist: %s", specfile)
	}

	if st.LockfileHash == "" {
		util.Die("file does not exist: %s", lockfile)
	}

	st.Write()
}
