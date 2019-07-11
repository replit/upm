package store

import (
	"encoding/json"
	"io/ioutil"
	"os"
	"path/filepath"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

var st *store

func getStoreLocation() string {
	loc, ok := os.LookupEnv("UPM_STORE")
	if ok {
		return loc
	} else {
		return ".upm/store.json"
	}
}

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

func readMaybe() {
	if st == nil {
		read()
	}
}

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

func HasSpecfileChanged(b api.LanguageBackend) bool {
	readMaybe()
	return hashFile(b.Specfile) != st.SpecfileHash
}

func HasLockfileChanged(b api.LanguageBackend) bool {
	readMaybe()
	return hashFile(b.Lockfile) != st.LockfileHash
}

func GuessWithCache(b api.LanguageBackend) map[api.PkgName]bool {
	readMaybe()
	old := st.GuessedImportsHash
	new := hashImports(&b)
	st.GuessedImportsHash = new
	if new != old {
		pkgs := b.Guess()
		st.GuessedImports = []string{}
		for name := range pkgs {
			st.GuessedImports = append(st.GuessedImports, string(name))
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

func UpdateFileHashes(b api.LanguageBackend) {
	readMaybe()
	st.SpecfileHash = hashFile(b.Specfile)
	st.LockfileHash = hashFile(b.Lockfile)
}
