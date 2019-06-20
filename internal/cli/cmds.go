package cli

import (
	"encoding/json"
	"fmt"
	"os"
	"strings"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/backends"
	"github.com/replit/upm/internal/store"
	"github.com/replit/upm/internal/table"
	"github.com/replit/upm/internal/util"
)

func runWhichLanguage() {
	backend := backends.GetBackend("")
	fmt.Println(backend.Name)
}

func runListLanguages() {
	for _, backendName := range backends.GetBackendNames() {
		fmt.Println(backendName)
	}
}

func runSearch(language string, queries []string, outputFormat outputFormat) {
	results := backends.GetBackend(language).Search(queries)
	fmt.Printf("output %#v in format %#v\n", results, outputFormat)
	util.NotImplemented()
}

func runInfo(language string, pkg string, outputFormat outputFormat) {
	info := backends.GetBackend(language).Info(api.PkgName(pkg))
	fmt.Printf("output %#v in format %#v\n", info, outputFormat)
	util.NotImplemented()
}

func runAdd(language string, args []string, guess bool) {
	pkgs := map[api.PkgName]api.PkgSpec{}
	for _, arg := range args {
		fields := strings.SplitN(arg, " ", 2)
		name := api.PkgName(fields[0])
		var spec api.PkgSpec
		if len(fields) >= 2 {
			spec = api.PkgSpec(fields[1])
		}

		pkgs[name] = spec
	}

	backend := backends.GetBackend(language)

	if guess {
		for name, _ := range backend.Guess() {
			if _, ok := pkgs[name]; !ok {
				pkgs[name] = ""
			}
		}
	}

	if _, err := os.Stat(backend.Specfile); os.IsNotExist(err) {
		// Nothing to see here.
	} else if err != nil {
		util.Die("%s: %s", backend.Specfile, err)
	} else {
		for name, _ := range backend.ListSpecfile() {
			delete(pkgs, name)
		}
	}

	if len(pkgs) >= 1 {
		backend.Add(pkgs)
	}

	store := store.Read()

	if api.QuirksIsReproducible(backend) {
		if !store.DoesSpecfileHashMatch(backend.Specfile) {
			backend.Lock()
		}
		if !store.DoesLockfileHashMatch(backend.Lockfile) {
			backend.Install()
		}
	} else {
		if !store.DoesSpecfileHashMatch(backend.Specfile) {
			backend.Install()
		}
	}

	store.UpdateHashes(backend.Specfile, backend.Lockfile)
}

func runRemove(language string, args []string) {
	backend := backends.GetBackend(language)

	if _, err := os.Stat(backend.Specfile); os.IsNotExist(err) {
		return
	} else if err != nil {
		util.Die("%s: %s", backend.Specfile, err)
	}
	specfilePkgs := backend.ListSpecfile()

	pkgs := map[api.PkgName]bool{}
	for _, arg := range args {
		name := api.PkgName(arg)
		if _, ok := specfilePkgs[name]; ok {
			pkgs[name] = true
		}
	}

	if len(pkgs) >= 1 {
		backend.Remove(pkgs)
	}

	store := store.Read()

	if api.QuirksIsReproducible(backend) {
		if !store.DoesSpecfileHashMatch(backend.Specfile) {
			backend.Lock()
		}
		if !store.DoesLockfileHashMatch(backend.Lockfile) {
			backend.Install()
		}
	} else {
		if !store.DoesSpecfileHashMatch(backend.Specfile) {
			backend.Install()
		}
	}

	store.UpdateHashes(backend.Specfile, backend.Lockfile)
}

func runLock(language string, force bool) {
	backend := backends.GetBackend(language)
	store := store.Read()
	if store.DoesSpecfileHashMatch(backend.Specfile) && !force {
		return
	}
	if api.QuirksIsReproducible(backend) {
		backend.Lock()
		if !store.DoesLockfileHashMatch(backend.Lockfile) {
			backend.Install()
		}
	} else {
		backend.Install()
	}

	store.UpdateHashes(backend.Specfile, backend.Lockfile)
}

func runInstall(language string, force bool) {
	backend := backends.GetBackend(language)
	store := store.Read()
	if store.DoesLockfileHashMatch(backend.Lockfile) && !force {
		return
	}
	backend.Install()
	store.UpdateHashes(backend.Specfile, backend.Lockfile)
}

type listSpecfileJsonEntry struct {
	Name string `json:"name"`
	Spec string `json:"spec"`
}

type listLockfileJsonEntry struct {
	Name    string `json:"name"`
	Version string `json:"version"`
}

func runList(language string, all bool, outputFormat outputFormat) {
	backend := backends.GetBackend(language)
	if !all {
		if _, err := os.Stat(backend.Specfile); os.IsNotExist(err) {
			fmt.Fprintln(os.Stderr, "no specfile")
			return
		} else if err != nil {
			util.Die("%s: %s", backend.Specfile, err)
		}
		results := backend.ListSpecfile()
		if len(results) == 0 {
			fmt.Fprintln(os.Stderr, "no packages in specfile")
			return
		}
		switch outputFormat {
		case outputFormatTable:
			t := table.New("name", "spec")
			for name, spec := range results {
				t.AddRow(string(name), string(spec))
			}
			t.SortBy("name")
			t.Print()

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
			util.Panicf("unknown output format %d", outputFormat)
		}
	} else {
		if _, err := os.Stat(backend.Lockfile); os.IsNotExist(err) {
			fmt.Fprintln(os.Stderr, "no lockfile")
			return
		} else if err != nil {
			util.Die("%s: %s", backend.Lockfile, err)
		}
		results := backend.ListLockfile()
		if len(results) == 0 {
			fmt.Fprintln(os.Stderr, "no packages in lockfile")
			return
		}
		switch outputFormat {
		case outputFormatTable:
			t := table.New("name", "version")
			for name, version := range results {
				t.AddRow(string(name), string(version))
			}
			t.SortBy("name")
			t.Print()

		case outputFormatJSON:
			j := []listLockfileJsonEntry{}
			for name, version := range results {
				j = append(j, listLockfileJsonEntry{
					Name:    string(name),
					Version: string(version),
				})
			}
			outputB, err := json.MarshalIndent(j, "", "  ")
			if err != nil {
				panic("couldn't marshal json")
			}
			fmt.Println(string(outputB))

		default:
			util.Panicf("unknown output format %d", outputFormat)
		}
	}
}

func runGuess(language string, all bool) {
	backend := backends.GetBackend(language)
	pkgs := backend.Guess()

	if !all {
		for name, _ := range backend.ListSpecfile() {
			delete(pkgs, name)
		}
	}

	for name, _ := range pkgs {
		fmt.Println(name)
	}
}
