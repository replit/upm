package cli

import (
	"encoding/json"
	"fmt"
	"os"
	"reflect"
	"sort"
	"strings"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/backends"
	"github.com/replit/upm/internal/store"
	"github.com/replit/upm/internal/table"
	"github.com/replit/upm/internal/util"
)

func runWhichLanguage(language string) {
	backend := backends.GetBackend(language)
	fmt.Println(backend.Name)
}

func runListLanguages() {
	for _, backendName := range backends.GetBackendNames() {
		fmt.Println(backendName)
	}
}

func runSearch(language string, args []string, outputFormat outputFormat) {
	results := backends.GetBackend(language).Search(
		strings.Join(args, " "),
	)
	// Output a reasonable number of results.
	if len(results) > 20 {
		results = results[:20]
	}
	switch outputFormat {
	case outputFormatTable:
		if len(results) == 0 {
			fmt.Fprintln(os.Stderr, "no search results")
			return
		}
		t := table.FromStructs(results)
		t.Print()

	case outputFormatJSON:
		outputB, err := json.Marshal(results)
		if err != nil {
			panic(err)
		}
		fmt.Println(string(outputB))
	}
}

type infoLine struct {
	Field string
	Value string
}

func runInfo(language string, pkg string, outputFormat outputFormat) {
	backend := backends.GetBackend(language)
	info := backend.Info(api.PkgName(pkg))
	if info.Name == "" {
		util.Die("no such package: %s", pkg)
	}

	switch outputFormat {
	case outputFormatTable:
		infoT := reflect.TypeOf(info)
		infoV := reflect.ValueOf(info)
		rows := []infoLine{}
		for i := 0; i < infoT.NumField(); i++ {
			field := infoT.Field(i).Tag.Get("pretty")
			var value string
			switch infoV.Field(i).Kind() {
			case reflect.String:
				value = infoV.Field(i).String()
			case reflect.Slice:
				parts := []string{}
				length := infoV.Field(i).Len()
				for j := 0; j < length; j++ {
					str := infoV.Field(i).Index(j).String()
					parts = append(parts, str)
				}
				value = strings.Join(parts, ", ")
			}
			if value == "" {
				continue
			}

			rows = append(rows, infoLine{Field: field, Value: value})
		}

		if len(rows) == 0 {
			util.Panicf(
				"no fields returned from backend %s",
				backend.Name,
			)
		}

		width := len(rows[0].Field)
		for i := 1; i < len(rows); i++ {
			if len(rows[i].Field) > width {
				width = len(rows[i].Field)
			}
		}

		for _, row := range rows {
			padLength := width - len(row.Field)
			padding := strings.Repeat(" ", padLength)
			fmt.Println(row.Field + ":" + padding + "   " + row.Value)
		}

	case outputFormatJSON:
		outputB, err := json.Marshal(info)
		if err != nil {
			panic(err)
		}
		fmt.Println(string(outputB))
	}
}

func maybeLock(b api.LanguageBackend, forceLock bool) {
	if b.QuirksIsNotReproducible() {
		return
	}

	if !forceLock && !store.HasSpecfileChanged(b) {
		return
	}

	if !util.FileExists(b.Specfile) {
		return
	}

	b.Lock()
}

func maybeInstall(b api.LanguageBackend, forceInstall bool) {
	if !forceInstall && !store.HasLockfileChanged(b) {
		return
	}

	if b.QuirksIsReproducible() {
		if !util.FileExists(b.Lockfile) {
			return
		}
	} else {
		if !util.FileExists(b.Specfile) {
			return
		}
	}

	b.Install()
}

func runAdd(
	language string, args []string, guess bool,
	forceLock bool, forceInstall bool) {

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

	b := backends.GetBackend(language)

	if guess {
		for name, _ := range store.GuessWithCache(b) {
			if _, ok := pkgs[name]; !ok {
				pkgs[name] = ""
			}
		}
	}

	if util.FileExists(b.Specfile) {
		for name, _ := range b.ListSpecfile() {
			delete(pkgs, name)
		}
	}

	if len(pkgs) >= 1 {
		b.Add(pkgs)

		if b.QuirksDoesAddRemoveAlsoInstall() {
			store.UpdateFileHashes(b)
			store.Write()
			return
		}
	}

	maybeLock(b, forceLock)

	if b.QuirksDoesLockNotAlsoInstall() {
		maybeInstall(b, forceInstall)
	}

	store.UpdateFileHashes(b)
	store.Write()
}

func runRemove(language string, args []string, forceLock bool, forceInstall bool) {
	b := backends.GetBackend(language)

	if !util.FileExists(b.Specfile) {
		return
	}
	specfilePkgs := b.ListSpecfile()

	pkgs := map[api.PkgName]bool{}
	for _, arg := range args {
		name := api.PkgName(arg)
		if _, ok := specfilePkgs[name]; ok {
			pkgs[name] = true
		}
	}

	if len(pkgs) >= 1 {
		b.Remove(pkgs)

		if b.QuirksDoesAddRemoveAlsoInstall() {
			store.UpdateFileHashes(b)
			store.Write()
			return
		}
	}

	maybeLock(b, forceLock)

	if b.QuirksDoesLockNotAlsoInstall() {
		maybeInstall(b, forceInstall)
	}

	store.UpdateFileHashes(b)
	store.Write()
}

func runLock(language string, forceLock bool, forceInstall bool) {
	b := backends.GetBackend(language)

	maybeLock(b, forceLock)

	if b.QuirksDoesLockNotAlsoInstall() {
		maybeInstall(b, forceInstall)
	}

	store.UpdateFileHashes(b)
	store.Write()
}

func runInstall(language string, force bool) {
	b := backends.GetBackend(language)

	maybeInstall(b, force)

	store.UpdateFileHashes(b)
	store.Write()
}

type listSpecfileJSONEntry struct {
	Name string `json:"name"`
	Spec string `json:"spec"`
}

type listLockfileJSONEntry struct {
	Name    string `json:"name"`
	Version string `json:"version"`
}

func runList(language string, all bool, outputFormat outputFormat) {
	backend := backends.GetBackend(language)
	if !all {
		var results map[api.PkgName]api.PkgSpec = nil
		fileExists := util.FileExists(backend.Specfile)
		if fileExists {
			results = backend.ListSpecfile()
		}
		switch outputFormat {
		case outputFormatTable:
			switch {
			case !fileExists:
				fmt.Fprintln(os.Stderr, "no specfile")
				return
			case len(results) == 0:
				fmt.Fprintln(os.Stderr, "no packages in specfile")
				return
			}
			t := table.New("name", "spec")
			for name, spec := range results {
				t.AddRow(string(name), string(spec))
			}
			t.SortBy("name")
			t.Print()

		case outputFormatJSON:
			j := []listSpecfileJSONEntry{}
			for name, spec := range results {
				j = append(j, listSpecfileJSONEntry{
					Name: string(name),
					Spec: string(spec),
				})
			}
			outputB, err := json.Marshal(j)
			if err != nil {
				panic("couldn't marshal json")
			}
			fmt.Println(string(outputB))

		default:
			util.Panicf("unknown output format %d", outputFormat)
		}
	} else {
		var results map[api.PkgName]api.PkgVersion = nil
		fileExists := util.FileExists(backend.Lockfile)
		if fileExists {
			results = backend.ListLockfile()
		}
		switch outputFormat {
		case outputFormatTable:
			switch {
			case !fileExists:
				fmt.Fprintln(os.Stderr, "no lockfile")
				return
			case len(results) == 0:
				fmt.Fprintln(os.Stderr, "no packages in lockfile")
				return
			}
			t := table.New("name", "version")
			for name, version := range results {
				t.AddRow(string(name), string(version))
			}
			t.SortBy("name")
			t.Print()

		case outputFormatJSON:
			j := []listLockfileJSONEntry{}
			for name, version := range results {
				j = append(j, listLockfileJSONEntry{
					Name:    string(name),
					Version: string(version),
				})
			}
			outputB, err := json.Marshal(j)
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
	pkgs := store.GuessWithCache(backend)

	if !all {
		if util.FileExists(backend.Specfile) {
			for name, _ := range backend.ListSpecfile() {
				delete(pkgs, name)
			}
		}
	}

	lines := []string{}
	for pkg := range pkgs {
		lines = append(lines, string(pkg))
	}
	sort.Strings(lines)

	for _, line := range lines {
		fmt.Println(line)
	}

	store.Write()
}
