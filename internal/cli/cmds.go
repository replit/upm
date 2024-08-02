package cli

import (
	"context"
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
	"github.com/replit/upm/internal/trace"
	"github.com/replit/upm/internal/util"
	"gopkg.in/DataDog/dd-trace-go.v1/ddtrace/tracer"
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
	b := backends.GetBackend(context.Background(), language)
	fmt.Println(b.Name)
}

// runListLanguages implements 'upm list-languages'.
func runListLanguages() {
	for _, info := range backends.GetBackendNames() {
		if info.Available {
			fmt.Println(info.Name)
		} else {
			fmt.Println(info.Name + "  (unavailable)")
		}
	}
}

func makeLoweredHM(normalizePackageName func(api.PkgName) api.PkgName, names []string) map[api.PkgName]bool {
	// Build a hashset. struct{}{} purportedly is of size 0, so this is as good as we get
	set := make(map[api.PkgName]bool)
	for _, pkg := range names {
		normal := normalizePackageName(api.PkgName(pkg))
		set[normal] = true
	}
	return set
}

// runSearch implements 'upm search'.
func runSearch(language string, args []string, outputFormat outputFormat, ignoredPackages []string) {
	query := strings.Join(args, " ")
	b := backends.GetBackend(context.Background(), language)

	var results []api.PkgInfo
	if strings.TrimSpace(query) == "" {
		results = []api.PkgInfo{}
	} else {
		results = b.Search(query)
	}

	{ // Filter out ignored packages
		ignoredPackageSet := makeLoweredHM(b.NormalizePackageName, ignoredPackages)
		filtered := []api.PkgInfo{}
		for _, pkg := range results {
			lower := b.NormalizePackageName(api.PkgName(pkg.Name))
			if ignoredPackageSet[lower] {
				continue
			}
			filtered = append(filtered, pkg)
		}

		results = filtered
	}

	// Apply some heuristics to give results that more closely resemble the user's query
	if b.SortPackages != nil {
		results = b.SortPackages(query, results)
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
	b := backends.GetBackend(context.Background(), language)
	info := b.Info(api.PkgName(pkg))
	if info.Name == "" {
		util.DieConsistency("no such package: %s", pkg)
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
func deleteLockfile(ctx context.Context, b api.LanguageBackend) {
	//nolint:ineffassign,wastedassign,staticcheck
	span, ctx := tracer.StartSpanFromContext(ctx, "deleteLockfile")
	defer span.Finish()
	if util.Exists(b.Lockfile) {
		util.ProgressMsg("delete " + b.Lockfile)
		os.Remove(b.Lockfile)
	}
}

// maybeLock either runs lock or not, depending on the backend, store,
// and command-line options. It returns true if it actually ran lock.
func maybeLock(ctx context.Context, b api.LanguageBackend, forceLock bool) bool {
	span, ctx := tracer.StartSpanFromContext(ctx, "maybeLock")
	defer span.Finish()
	if b.QuirksIsNotReproducible() {
		return false
	}

	if !util.Exists(b.Specfile) {
		return false
	}

	shouldLock := forceLock || !util.Exists(b.Lockfile) || store.HasSpecfileChanged(b)
	if !shouldLock {
		if packageDir := b.GetPackageDir(); packageDir != "" && !util.Exists(packageDir) {
			// Only run lock if a specfile exists and it lists at least one package.
			shouldLock = util.Exists(b.Specfile) && len(b.ListSpecfile(true)) > 0
		}
	}

	if shouldLock {
		b.Lock(ctx)
		return true
	}

	return false
}

// maybeInstall either runs install or not, depending on the backend,
// store, and command-line options.
func maybeInstall(ctx context.Context, b api.LanguageBackend, forceInstall bool) {
	span, ctx := tracer.StartSpanFromContext(ctx, "maybeInstall")
	defer span.Finish()
	if b.QuirksIsReproducible() {
		if !util.Exists(b.Lockfile) {
			return
		}
		var needsPackageDir bool
		if packageDir := b.GetPackageDir(); packageDir != "" {
			needsPackageDir = !util.Exists(packageDir)
		}
		if forceInstall || store.HasSpecfileChanged(b) || needsPackageDir {
			b.Install(ctx)
		}
	} else {
		if !util.Exists(b.Specfile) {
			return
		}
		var needsPackageDir bool
		if packageDir := b.GetPackageDir(); packageDir != "" {
			needsPackageDir = !util.Exists(packageDir)
		}
		if forceInstall || store.HasSpecfileChanged(b) || needsPackageDir {
			b.Install(ctx)
		}
	}
}

// runAdd implements 'upm add'.
func runAdd(
	language string, args []string, upgrade bool,
	guess bool, forceGuess bool, ignoredPackages []string,
	forceLock bool, forceInstall bool, name string) {
	span, ctx := trace.StartSpanFromExistingContext("runAdd")
	defer span.Finish()
	b := backends.GetBackend(ctx, language)

	normPkgs := b.NormalizePackageArgs(args)

	if guess {
		guessed := store.GuessWithCache(ctx, b, forceGuess)

		// Map from normalized package names to original
		// names.
		guessedNorm := map[string][]api.PkgName{}
		for key, guesses := range guessed {
			normalized := []api.PkgName{}
			for _, guess := range guesses {
				normalized = append(normalized, b.NormalizePackageName(guess))
			}
			guessedNorm[key] = normalized
		}

		for _, pkg := range ignoredPackages {
			pkg := b.NormalizePackageName(api.PkgName(pkg))
			for key, guesses := range guessedNorm {
				for _, guess := range guesses {
					if pkg == guess {
						delete(guessedNorm, key)
					}
				}
			}
		}

		for _, guesses := range guessedNorm {
			found := false
			for _, guess := range guesses {
				if _, ok := normPkgs[guess]; !ok {
					found = true
					break
				}
			}
			if !found {
				normPkgs[b.NormalizePackageName(guesses[0])] = api.PkgCoordinates{
					Name: string(guesses[0]),
					Spec: "",
				}
			}
		}
	}

	if util.Exists(b.Specfile) {
		s := silenceSubroutines()
		for name, spec := range b.ListSpecfile(true) {
			if spec == normPkgs[b.NormalizePackageName(name)].Spec {
				delete(normPkgs, b.NormalizePackageName(name))
			}
		}
		s.restore()
	}

	if upgrade {
		deleteLockfile(ctx, b)
	}

	if len(normPkgs) >= 1 {
		pkgs := map[api.PkgName]api.PkgSpec{}
		for _, nameAndSpec := range normPkgs {
			pkgs[api.PkgName(nameAndSpec.Name)] = nameAndSpec.Spec
		}

		b.Add(ctx, pkgs, name)
	}

	if len(normPkgs) == 0 || b.QuirksDoesAddRemoveNotAlsoLock() {
		didLock := maybeLock(ctx, b, forceLock)

		if !(didLock && b.QuirksDoesLockAlsoInstall()) {
			maybeInstall(ctx, b, forceInstall)
		}
	} else if len(normPkgs) == 0 || b.QuirksDoesAddRemoveNotAlsoInstall() {
		maybeInstall(ctx, b, forceInstall)
	}

	store.Read(ctx, b)
	store.ClearGuesses(ctx, b)
	store.UpdateFileHashes(ctx, b)
	store.Write(ctx)
}

// runRemove implements 'upm remove'.
func runRemove(language string, args []string, upgrade bool,
	forceLock bool, forceInstall bool) {
	span, ctx := trace.StartSpanFromExistingContext("runRemove")
	defer span.Finish()
	b := backends.GetBackend(ctx, language)

	if !util.Exists(b.Specfile) {
		return
	}

	s := silenceSubroutines()
	specfilePkgs := b.ListSpecfile(true)
	s.restore()

	// Map whose keys are normalized package names.
	normSpecfilePkgs := map[api.PkgName]bool{}
	for name := range specfilePkgs {
		normSpecfilePkgs[b.NormalizePackageName(name)] = true
	}

	// Map from normalized package names to original package
	// names.
	normPkgs := map[api.PkgName]string{}
	for _, arg := range args {
		name := arg
		norm := b.NormalizePackageName(api.PkgName(arg))
		if normSpecfilePkgs[norm] {
			normPkgs[norm] = name
		}
	}

	if upgrade {
		deleteLockfile(ctx, b)
	}

	if len(normPkgs) >= 1 {
		pkgs := map[api.PkgName]bool{}
		for name := range normPkgs {
			pkgs[name] = true
		}
		b.Remove(ctx, pkgs)
	}

	if len(normPkgs) == 0 || b.QuirksDoesAddRemoveNotAlsoLock() {
		didLock := maybeLock(ctx, b, forceLock)

		if !(didLock && b.QuirksDoesLockAlsoInstall()) {
			maybeInstall(ctx, b, forceInstall)
		}
	} else if len(normPkgs) == 0 || b.QuirksDoesAddRemoveNotAlsoInstall() {
		maybeInstall(ctx, b, forceInstall)
	}

	store.Read(ctx, b)
	store.ClearGuesses(ctx, b)
	store.UpdateFileHashes(ctx, b)
	store.Write(ctx)
}

// runLock implements 'upm lock'.
func runLock(language string, upgrade bool, forceLock bool, forceInstall bool) {
	span, ctx := trace.StartSpanFromExistingContext("runLock")
	defer span.Finish()
	b := backends.GetBackend(ctx, language)

	if upgrade {
		deleteLockfile(ctx, b)
	}

	didLock := maybeLock(ctx, b, forceLock)

	if !(didLock && b.QuirksDoesLockAlsoInstall()) {
		maybeInstall(ctx, b, forceInstall)
	}

	store.Read(ctx, b)
	store.UpdateFileHashes(ctx, b)
	store.Write(ctx)
}

// runInstall implements 'upm install'.
func runInstall(language string, force bool) {
	span, ctx := trace.StartSpanFromExistingContext("runInstall")
	defer span.Finish()
	b := backends.GetBackend(ctx, language)

	maybeInstall(ctx, b, force)

	store.Read(ctx, b)
	store.UpdateFileHashes(ctx, b)
	store.Write(ctx)
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
	span, ctx := trace.StartSpanFromExistingContext("runList")
	defer span.Finish()
	b := backends.GetBackend(ctx, language)
	if !all {
		var results map[api.PkgName]api.PkgSpec = nil
		fileExists := util.Exists(b.Specfile)
		if fileExists {
			results = b.ListSpecfile(true)
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
	span, ctx := trace.StartSpanFromExistingContext("runGuess")
	defer span.Finish()
	b := backends.GetBackend(ctx, language)
	guessed := store.GuessWithCache(ctx, b, forceGuess)

	// Map from normalized to original names.
	normPkgs := map[string][]api.PkgName{}
	for key, guesses := range guessed {
		normalized := []api.PkgName{}
		for _, guess := range guesses {
			normalized = append(normalized, b.NormalizePackageName(guess))
		}
		normPkgs[key] = normalized
	}

	if !all {
		if util.Exists(b.Specfile) {
			for name := range b.ListSpecfile(true) {
				name := b.NormalizePackageName(name)
				for key, pkgs := range normPkgs {
					for _, pkg := range pkgs {
						if pkg == name {
							delete(normPkgs, key)
						}
					}
				}
			}
		}
	}

	for _, ignored := range ignoredPackages {
		ignored := b.NormalizePackageName(api.PkgName(ignored))
		for key, pkgs := range normPkgs {
			for _, pkg := range pkgs {
				if pkg == ignored {
					delete(normPkgs, key)
				}
			}
		}
	}

	lines := []string{}
	for _, pkgs := range normPkgs {
		lines = append(lines, string(pkgs[0]))
	}
	sort.Strings(lines)

	for _, line := range lines {
		fmt.Println(line)
	}

	store.Write(ctx)
}

// runShowSpecfile implements 'upm show-specfile'.
func runShowSpecfile(language string) {
	fmt.Println(backends.GetBackend(context.Background(), language).Specfile)
}

// runShowLockfile implements 'upm show-lockfile'.
func runShowLockfile(language string) {
	b := backends.GetBackend(context.Background(), language)
	if b.Lockfile != "" {
		fmt.Println(b.Lockfile)
	}
}

// runShowPackageDir implements 'upm show-package-dir'.
func runShowPackageDir(language string) {
	b := backends.GetBackend(context.Background(), language)
	dir := b.GetPackageDir()
	if dir != "" {
		fmt.Println(dir)
	}
}

// runInstallReplitNixSystemDependencies implements 'upm install-replit-nix-system-dependencies'.
func runInstallReplitNixSystemDependencies(language string, args []string) {
	span, ctx := trace.StartSpanFromExistingContext("runInstallReplitNixSystemDependencies")
	defer span.Finish()
	b := backends.GetBackend(ctx, language)
	normPkgs := b.NormalizePackageArgs(args)
	pkgs := []api.PkgName{}
	for p := range normPkgs {
		pkgs = append(pkgs, p)
	}
	b.InstallReplitNixSystemDependencies(ctx, pkgs)
}
