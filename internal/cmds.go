package internal

import (
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
		fields := strings.Fields(arg)
		if !(len(fields) >= 1 && len(fields) <= 2) {
			fmt.Fprintf(os.Stderr, "invalid package/spec: %#v\n", arg)
			os.Exit(1)
		}

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

	for name, _ := range backend.listSpecfile() {
		delete(pkgs, name)
	}

	if len(pkgs) >= 1 {
		backend.add(pkgs)
	}
}

func runRemove(language string, args []string) {
	backend := getBackend(language)
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
}

func runLock(language string, force bool) {
	notImplemented()
}

func runInstall(language string, force bool) {
	notImplemented()
}

func runList(language string, all bool, outputFormat outputFormat) {
	backend := getBackend(language)
	if all {
		results := backend.listLockfile()
		fmt.Printf("output %#v in format %#v\n", results, outputFormat)
		notImplemented()
	} else {
		results := backend.listSpecfile()
		fmt.Printf("output %#v in format %#v\n", results, outputFormat)
		notImplemented()
	}
}

func runGuess(language string, all bool) {
	backend := getBackend(language)
	pkgs := backend.guess()

	if (!all) {
		for name, _ := range backend.listSpecfile() {
			delete(pkgs, name)
		}
	}

	for name, _ := range pkgs {
		fmt.Println(name)
	}
}
