package internal

import (
	"encoding/json"
	"fmt"
	"os"
	"strings"
)

func runWhichLanguage() {
	backend := getBackend("")
	fmt.Println(backend.name)
}

func runListLanguages() {
	for _, backendName := range getBackendNames() {
		fmt.Println(backendName)
	}
}

func runSearch(language string, queries []string, outputFormat outputFormat) {
	results := getBackend(language).search(queries)
	fmt.Printf("output %#v in format %#v\n", results, outputFormat)
	notImplemented()
}

func runInfo(language string, pkg string, outputFormat outputFormat) {
	info := getBackend(language).info(pkgName(pkg))
	fmt.Printf("output %#v in format %#v\n", info, outputFormat)
	notImplemented()
}

func runAdd(language string, args []string, guess bool) {
	pkgs := map[pkgName]pkgSpec{}
	for _, arg := range args {
		fields := strings.SplitN(arg, " ", 2)
		name := pkgName(fields[0])
		var spec pkgSpec
		if len(fields) >= 2 {
			spec = pkgSpec(fields[1])
		}

		pkgs[name] = spec
	}

	backend := getBackend(language)

	if guess {
		for name, _ := range backend.guess() {
			if _, ok := pkgs[name]; !ok {
				pkgs[name] = ""
			}
		}
	}

	if _, err := os.Stat(backend.specfile); os.IsNotExist(err) {
		// Nothing to see here.
	} else if err != nil {
		die("%s: %s", backend.specfile, err)
	} else {
		for name, _ := range backend.listSpecfile() {
			delete(pkgs, name)
		}
	}

	if len(pkgs) >= 1 {
		backend.add(pkgs)
	}

	store := readStore()

	if quirksIsReproducible(backend) {
		if !doesStoreSpecfileHashMatch(store, backend.specfile) {
			backend.lock()
		}
		if !doesStoreLockfileHashMatch(store, backend.lockfile) {
			backend.install()
		}
	} else {
		if !doesStoreSpecfileHashMatch(store, backend.specfile) {
			backend.install()
		}
	}

	updateStoreHashes(store, backend.specfile, backend.lockfile)
}

func runRemove(language string, args []string) {
	backend := getBackend(language)

	if _, err := os.Stat(backend.specfile); os.IsNotExist(err) {
		return
	} else if err != nil {
		die("%s: %s", backend.specfile, err)
	}
	specfilePkgs := backend.listSpecfile()

	pkgs := map[pkgName]bool{}
	for _, arg := range args {
		name := pkgName(arg)
		if _, ok := specfilePkgs[name]; ok {
			pkgs[name] = true
		}
	}

	if len(pkgs) >= 1 {
		backend.remove(pkgs)
	}

	store := readStore()

	if quirksIsReproducible(backend) {
		if !doesStoreSpecfileHashMatch(store, backend.specfile) {
			backend.lock()
		}
		if !doesStoreLockfileHashMatch(store, backend.lockfile) {
			backend.install()
		}
	} else {
		if !doesStoreSpecfileHashMatch(store, backend.specfile) {
			backend.install()
		}
	}

	updateStoreHashes(store, backend.specfile, backend.lockfile)
}

func runLock(language string, force bool) {
	backend := getBackend(language)
	store := readStore()
	if doesStoreSpecfileHashMatch(store, backend.specfile) && !force {
		return
	}
	if quirksIsReproducible(backend) {
		backend.lock()
		if !doesStoreLockfileHashMatch(store, backend.lockfile) {
			backend.install()
		}
	} else {
		backend.install()
	}

	updateStoreHashes(store, backend.specfile, backend.lockfile)
}

func runInstall(language string, force bool) {
	backend := getBackend(language)
	store := readStore()
	if doesStoreLockfileHashMatch(store, backend.lockfile) && !force {
		return
	}
	backend.install()
	updateStoreHashes(store, backend.specfile, backend.lockfile)
}

type listSpecfileJsonEntry struct {
	Name string `json:"name"`
	Spec string `json:"spec"`
}

func runList(language string, all bool, outputFormat outputFormat) {
	backend := getBackend(language)
	if !all {
		if _, err := os.Stat(backend.specfile); os.IsNotExist(err) {
			fmt.Fprintln(os.Stderr, "no specfile")
			return
		} else if err != nil {
			die("%s: %s", backend.specfile, err)
		}
		results := backend.listSpecfile()
		if len(results) == 0 {
			fmt.Fprintln(os.Stderr, "no packages in specfile")
			return
		}
		switch outputFormat {
		case outputFormatTable:
			t := makeTable("name", "spec")
			for name, spec := range results {
				t.addRow(string(name), string(spec))
			}
			t.sortBy("name")
			t.print()

		case outputFormatJSON:
			j := []listSpecfileJsonEntry{}
			for name, spec := range results {
				j = append(j, listSpecfileJsonEntry{
					Name: string(name),
					Spec: string(spec),
				})
			}
			outputB, err := json.MarshalIndent(j, "", "  ")
			if err != nil {
				panic("couldn't marshal json")
			}
			fmt.Println(string(outputB))

		default:
			panicf("unknown output format %d", outputFormat)
		}
	} else {
		if _, err := os.Stat(backend.lockfile); os.IsNotExist(err) {
			fmt.Fprintln(os.Stderr, "no lockfile")
			return
		} else if err != nil {
			die("%s: %s", backend.lockfile, err)
		}
		results := backend.listLockfile()
		if len(results) == 0 {
			fmt.Fprintln(os.Stderr, "no packages in lockfile")
			return
		}
		fmt.Printf("output %#v in format %#v\n", results, outputFormat)
		notImplemented()
	}
}

func runGuess(language string, all bool) {
	backend := getBackend(language)
	pkgs := backend.guess()

	if !all {
		for name, _ := range backend.listSpecfile() {
			delete(pkgs, name)
		}
	}

	for name, _ := range pkgs {
		fmt.Println(name)
	}
}
