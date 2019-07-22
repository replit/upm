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

// runWhichLanguage implements 'upm which-language'.
func runWhichLanguage(language string) {
	b := backends.GetBackend(language)
	fmt.Println(b.Name)
}

// runListLanguages implements 'upm list-languages'.
func runListLanguages() {
	for _, backendName := range backends.GetBackendNames() {
		fmt.Println(backendName)
	}
}

// runSearch implements 'upm search'.
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

// infoLine represents one line in the table emitted by 'upm info'.
type infoLine struct {
	Field string
	Value string
}

// runInfo implements 'upm info'.
func runInfo(language string, pkg string, outputFormat outputFormat) {
	b := backends.GetBackend(language)
	info := b.Info(api.PkgName(pkg))
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
				b.Name,
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

// deleteLockfile deletes the project's lockfile, if one exists.
func deleteLockfile(b api.LanguageBackend) {
	if util.Exists(b.Lockfile) {
		util.ProgressMsg("delete " + b.Lockfile)
		os.Remove(b.Lockfile)
	}
}

// maybeLock either runs lock or not, depending on the backend, store,
// and command-line options.
func maybeLock(b api.LanguageBackend, forceLock bool) {
	if b.QuirksIsNotReproducible() {
		return
	}

	if !util.Exists(b.Specfile) {
		return
	}

	if forceLock || !util.Exists(b.Lockfile) || store.HasSpecfileChanged(b) {
		b.Lock()
	}
}

// maybeInstall either runs install or not, depending on the backend,
// store, and command-line options.
func maybeInstall(b api.LanguageBackend, forceInstall bool) {
	if b.QuirksIsReproducible() {
		if !util.Exists(b.Lockfile) {
			return
		}
		if forceInstall || store.HasLockfileChanged(b) {
			b.Install()
		}
	} else {
		if !util.Exists(b.Specfile) {
			return
		}
		if forceInstall || store.HasSpecfileChanged(b) {
			b.Install()
		}
	}
}

// runAdd implements 'upm add'.
func runAdd(
	language string, args []string, upgrade bool,
	guess bool, forceGuess bool, ignoredPackages []string,
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
		guessed := store.GuessWithCache(b, forceGuess)
		for _, pkg := range ignoredPackages {
			delete(guessed, api.PkgName(pkg))
		}

		for name, _ := range guessed {
			if _, ok := pkgs[name]; !ok {
				pkgs[name] = ""
			}
		}
	}

	if util.Exists(b.Specfile) {
		for name, _ := range b.ListSpecfile() {
			delete(pkgs, name)
		}
	}

	if upgrade {
		deleteLockfile(b)
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

// runRemove implements 'upm remove'.
func runRemove(language string, args []string, upgrade bool,
	forceLock bool, forceInstall bool) {

	b := backends.GetBackend(language)

	if !util.Exists(b.Specfile) {
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

	if upgrade {
		deleteLockfile(b)
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

// runLock implements 'upm lock'.
func runLock(language string, upgrade bool, forceLock bool, forceInstall bool) {
	b := backends.GetBackend(language)

	if upgrade {
		deleteLockfile(b)
	}

	maybeLock(b, forceLock)

	if b.QuirksDoesLockNotAlsoInstall() {
		maybeInstall(b, forceInstall)
	}

	store.UpdateFileHashes(b)
	store.Write()
}

// runInstall implements 'upm install'.
func runInstall(language string, force bool) {
	b := backends.GetBackend(language)

	maybeInstall(b, force)

	store.UpdateFileHashes(b)
	store.Write()
}

// listSpecfileJSONEntry represents one entry in the JSON list emitted
// by 'upm list'.
type listSpecfileJSONEntry struct {
	Name string `json:"name"`
	Spec string `json:"spec"`
}

// listLockfileJSONEntry represents one entry in the JSON list emitted
// by 'upm list -a'.
type listLockfileJSONEntry struct {
	Name    string `json:"name"`
	Version string `json:"version"`
}

// runList implements 'upm list'.
func runList(language string, all bool, outputFormat outputFormat) {
	b := backends.GetBackend(language)
	if !all {
		var results map[api.PkgName]api.PkgSpec = nil
		fileExists := util.Exists(b.Specfile)
		if fileExists {
			results = b.ListSpecfile()
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
		fileExists := util.Exists(b.Lockfile)
		if fileExists {
			results = b.ListLockfile()
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

// runGuess implements 'upm guess'.
func runGuess(
	language string, all bool,
	forceGuess bool, ignoredPackages []string) {

	b := backends.GetBackend(language)
	pkgs := store.GuessWithCache(b, forceGuess)

	if !all {
		if util.Exists(b.Specfile) {
			for name, _ := range b.ListSpecfile() {
				delete(pkgs, name)
			}
		}
	}

	for _, pkg := range ignoredPackages {
		delete(pkgs, api.PkgName(pkg))
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
