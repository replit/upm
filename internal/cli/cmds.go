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
	"github.com/replit/upm/internal/config"
	"github.com/replit/upm/internal/store"
	"github.com/replit/upm/internal/table"
	"github.com/replit/upm/internal/util"
)

// subroutineSilencer is used to easily enable and restore
// config.Quiet for part of a function.
type subroutineSilencer struct {
	origQuiet bool
}

// silenceSubroutines turns on config.Quiet and returns a struct that
// can be used to restore its value. This only happens if
// UPM_SILENCE_SUBROUTINES is non-empty.
func silenceSubroutines() subroutineSilencer {
	s := subroutineSilencer{origQuiet: config.Quiet}
	if os.Getenv("UPM_SILENCE_SUBROUTINES") != "" {
		config.Quiet = true
	}
	return s
}

// restore restores the previous value of config.Quiet.
func (s *subroutineSilencer) restore() {
	config.Quiet = s.origQuiet
}

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
	query := strings.Join(args, " ")
	b := backends.GetBackend(language)

	var results []api.PkgInfo
	if strings.TrimSpace(query) == "" {
		results = []api.PkgInfo{}
	} else {
		results = b.Search(query)
	}

	// Output a reasonable number of results.
	if len(results) > 20 {
		results = results[:20]
	}

	switch outputFormat {
	case outputFormatTable:
		if len(results) == 0 {
			util.Log("no search results")
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
// and command-line options. It returns true if it actually ran lock.
func maybeLock(b api.LanguageBackend, forceLock bool) bool {
	if b.QuirksIsNotReproducible() {
		return false
	}

	if !util.Exists(b.Specfile) {
		return false
	}

	if forceLock || !util.Exists(b.Lockfile) || store.HasSpecfileChanged(b) {
		b.Lock()
		return true
	}

	return false
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

// pkgNameAndSpec is a tuple of a PkgName and a PkgSpec. It's used to
// put both of them as a value in the same map entry.
type pkgNameAndSpec struct {
	name api.PkgName
	spec api.PkgSpec
}

// runAdd implements 'upm add'.
func runAdd(
	language string, args []string, upgrade bool,
	guess bool, forceGuess bool, ignoredPackages []string,
	forceLock bool, forceInstall bool) {

	b := backends.GetBackend(language)

	// Map from normalized package names to the corresponding
	// original package names and specs.
	normPkgs := map[api.PkgName]pkgNameAndSpec{}
	for _, arg := range args {
		fields := strings.SplitN(arg, " ", 2)
		name := api.PkgName(fields[0])
		var spec api.PkgSpec
		if len(fields) >= 2 {
			spec = api.PkgSpec(fields[1])
		}

		normPkgs[b.NormalizePackageName(name)] = pkgNameAndSpec{
			name: name,
			spec: spec,
		}
	}

	if guess {
		guessed := store.GuessWithCache(b, forceGuess)

		// Map from normalized package names to original
		// names.
		guessedNorm := map[api.PkgName]api.PkgName{}
		for name := range guessed {
			guessedNorm[b.NormalizePackageName(name)] = name
		}

		for _, pkg := range ignoredPackages {
			delete(guessedNorm, b.NormalizePackageName(api.PkgName(pkg)))
		}

		for name, _ := range guessed {
			if _, ok := normPkgs[b.NormalizePackageName(name)]; !ok {
				normPkgs[b.NormalizePackageName(name)] = pkgNameAndSpec{
					name: name,
					spec: "",
				}
			}
		}
	}

	if util.Exists(b.Specfile) {
		s := silenceSubroutines()
		for name, _ := range b.ListSpecfile() {
			delete(normPkgs, b.NormalizePackageName(name))
		}
		s.restore()
	}

	if upgrade {
		deleteLockfile(b)
	}

	if len(normPkgs) >= 1 {
		pkgs := map[api.PkgName]api.PkgSpec{}
		for _, nameAndSpec := range normPkgs {
			pkgs[nameAndSpec.name] = nameAndSpec.spec
		}
		b.Add(pkgs)
	}

	if len(normPkgs) == 0 || b.QuirksDoesAddRemoveNotAlsoLock() {
		didLock := maybeLock(b, forceLock)

		if !(didLock && b.QuirksDoesLockAlsoInstall()) {
			maybeInstall(b, forceInstall)
		}
	} else if len(normPkgs) == 0 || b.QuirksDoesAddRemoveNotAlsoInstall() {
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

	s := silenceSubroutines()
	specfilePkgs := b.ListSpecfile()
	s.restore()

	// Map whose keys are normalized package names.
	normSpecfilePkgs := map[api.PkgName]bool{}
	for name := range specfilePkgs {
		normSpecfilePkgs[b.NormalizePackageName(name)] = true
	}

	// Map from normalized package names to original package
	// names.
	normPkgs := map[api.PkgName]api.PkgName{}
	for _, arg := range args {
		name := api.PkgName(arg)
		norm := b.NormalizePackageName(name)
		if _, ok := normSpecfilePkgs[norm]; ok {
			normPkgs[norm] = name
		}
	}

	if upgrade {
		deleteLockfile(b)
	}

	if len(normPkgs) >= 1 {
		pkgs := map[api.PkgName]bool{}
		for _, name := range normPkgs {
			pkgs[name] = true
		}
		b.Remove(pkgs)
	}

	if len(normPkgs) == 0 || b.QuirksDoesAddRemoveNotAlsoLock() {
		didLock := maybeLock(b, forceLock)

		if !(didLock && b.QuirksDoesLockAlsoInstall()) {
			maybeInstall(b, forceInstall)
		}
	} else if len(normPkgs) == 0 || b.QuirksDoesAddRemoveNotAlsoInstall() {
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

	didLock := maybeLock(b, forceLock)

	if !(didLock && b.QuirksDoesLockAlsoInstall()) {
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
				util.Log("no specfile")
				return
			case len(results) == 0:
				util.Log("no packages in specfile")
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
				util.Log("no lockfile")
				return
			case len(results) == 0:
				util.Log("no packages in lockfile")
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

	// Map from normalized to original names.
	normPkgs := map[api.PkgName]api.PkgName{}
	for pkg := range pkgs {
		normPkgs[b.NormalizePackageName(pkg)] = pkg
	}

	if !all {
		if util.Exists(b.Specfile) {
			for name, _ := range b.ListSpecfile() {
				delete(normPkgs, b.NormalizePackageName(name))
			}
		}
	}

	for _, pkg := range ignoredPackages {
		delete(normPkgs, b.NormalizePackageName(api.PkgName(pkg)))
	}

	lines := []string{}
	for _, pkg := range normPkgs {
		lines = append(lines, string(pkg))
	}
	sort.Strings(lines)

	for _, line := range lines {
		fmt.Println(line)
	}

	store.Write()
}

// runShowSpecfile implements 'upm show-specfile'.
func runShowSpecfile(language string) {
	fmt.Println(backends.GetBackend(language).Specfile)
}

// runShowLockfile implements 'upm show-lockfile'.
func runShowLockfile(language string) {
	fmt.Println(backends.GetBackend(language).Lockfile)
}

// runShowPackageDir implements 'upm show-package-dir'.
func runShowPackageDir(language string) {
	b := backends.GetBackend(language)
	dir := b.GetPackageDir()
	fmt.Println(dir)
}
