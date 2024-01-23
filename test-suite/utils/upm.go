package testUtils

import (
	"encoding/json"
	"strings"

	"github.com/replit/upm/internal/api"
)

func (bt *BackendT) UpmAdd(pkgs ...string) {
	beforeSpecDeps := bt.UpmListSpecFile()

	var beforeLockDeps []api.PkgInfo
	if bt.Backend.QuirksIsReproducible() {
		beforeLockDeps = bt.UpmListLockFile()
	}

	args := []string{
		"--lang",
		bt.Backend.Name,
		"add",
	}
	_, err := bt.Exec(
		"upm",
		append(args, pkgs...)...,
	)

	if err != nil {
		bt.Fail("upm failed to add: %v", err)
	}

	afterSpecDeps := bt.UpmListSpecFile()
	var afterLockDeps []api.PkgInfo

	if bt.Backend.QuirksIsReproducible() {
		afterLockDeps = bt.UpmListLockFile()
		if len(beforeLockDeps) >= len(afterLockDeps) {
			bt.Fail("expected more deps in lock file after add (before %d, after %d)", len(beforeLockDeps), len(afterLockDeps))
		}
	}

	if len(beforeSpecDeps) >= len(afterSpecDeps) {
		bt.Fail("expected more deps in lock file after add (before %d, after %d)", len(beforeSpecDeps), len(afterSpecDeps))
	}

	for pkg := range bt.Backend.NormalizePackageArgs(pkgs) {
		if bt.Backend.QuirksIsReproducible() {
			found := false
			for _, dep := range afterLockDeps {
				if bt.Backend.NormalizePackageName(api.PkgName(dep.Name)) == pkg {
					found = true
					break
				}
			}
			if !found {
				bt.Fail("expected %s in lock file after add", pkg)
			}
		}

		found := false
		for _, dep := range afterSpecDeps {
			if bt.Backend.NormalizePackageName(api.PkgName(dep.Name)) == pkg {
				found = true
				break
			}
		}
		if !found {
			bt.Fail("expected %s in spec file after add", pkg)
		}
	}
}

func (bt *BackendT) UpmGuess(expect ...string) {
	out, err := bt.Exec(
		"upm",
		"--lang",
		bt.Backend.Name,
		"guess",
	)

	if err != nil {
		bt.Fail("upm failed to guess: %v", err)
	}

	guesses := strings.Split(strings.TrimSpace(out.Stdout), "\n")
	for len(guesses) > 0 {
		guess := guesses[0]
		guesses = guesses[1:]
		if guess == "" {
			continue
		}

		found := false
		for ii, expected := range expect {
			if guess == expected {
				found = true
				expect = append(expect[:ii], expect[ii+1:]...)
				break
			}
		}

		if !found {
			bt.Fail("unexpected guess %s", guess)
		}
	}

	if len(expect) != 0 {
		bt.Fail("expected guesses %v", expect)
	}
}

func (bt *BackendT) UpmInfo(pkg string) {
	out, err := bt.Exec(
		"upm",
		"--lang",
		bt.Backend.Name,
		"info",
		"--format",
		"json",
		pkg,
	)

	if err != nil {
		bt.Fail("upm failed to get info: %v", err)
	}

	var info api.PkgInfo
	err = json.NewDecoder(strings.NewReader(out.Stdout)).Decode(&info)
	if err != nil {
		bt.Fail("failed to decode json: %v", err)
	}

	if info.Name != pkg {
		bt.Fail("expected info for %s, got %s", pkg, info.Name)
	}
}

func (bt *BackendT) UpmInstall() {
	_, err := bt.Exec(
		"upm",
		"--lang",
		bt.Backend.Name,
		"install",
	)

	if err != nil {
		bt.Fail("upm failed to install: %v", err)
	}
}

func normalizePackageNames(bt *BackendT, pkgs []api.PkgInfo) []api.PkgInfo {
	for idx, pkg := range pkgs {
		out := (string)(bt.Backend.NormalizePackageName(api.PkgName(pkg.Name)))
		if pkgs[idx].Name != out {
			bt.t.Log("Inconsistently normalized package name in results: ", pkgs[idx].Name, "did not equal", out)
		}
		pkgs[idx].Name = out
	}
	return pkgs
}

func (bt *BackendT) UpmListLockFile() []api.PkgInfo {
	out, err := bt.Exec(
		"upm",
		"--lang",
		bt.Backend.Name,
		"list",
		"--all",
		"--format",
		"json",
	)

	if err != nil {
		bt.Fail("upm failed to list: %v", err)
	}

	var results []api.PkgInfo
	err = json.NewDecoder(strings.NewReader(out.Stdout)).Decode(&results)
	if err != nil {
		bt.Fail("failed to decode json: %v", err)
	}

	return normalizePackageNames(bt, results)
}

func (bt *BackendT) UpmListSpecFile() []api.PkgInfo {
	out, err := bt.Exec(
		"upm",
		"--lang",
		bt.Backend.Name,
		"list",
		"--format",
		"json",
	)

	if err != nil {
		bt.Fail("upm failed to list: %v", err)
	}

	var results []api.PkgInfo
	err = json.NewDecoder(strings.NewReader(out.Stdout)).Decode(&results)
	if err != nil {
		bt.Fail("failed to decode json: %v", err)
	}

	return normalizePackageNames(bt, results)
}

func (bt *BackendT) UpmLock() {
	_, err := bt.Exec(
		"upm",
		"--lang",
		bt.Backend.Name,
		"lock",
	)

	if err != nil {
		bt.Fail("upm failed to lock: %v", err)
	}
}

func (bt *BackendT) UpmPackageDir() string {
	out, err := bt.Exec(
		"upm",
		"--lang",
		bt.Backend.Name,
		"show-package-dir",
	)

	if err != nil {
		bt.Fail("upm failed to show package dir: %v", err)
	}

	return strings.TrimSpace(out.Stdout)
}

func (bt *BackendT) UpmRemove(pkgs ...string) {
	beforeSpecDeps := bt.UpmListSpecFile()

	if len(beforeSpecDeps) < len(pkgs) {
		bt.Fail("expected deps to be in spec file before remove %v", pkgs)
	}

	specsExpectedToStay := []string{}

	for _, dep := range beforeSpecDeps {
		removing := false
		for _, pkg := range pkgs {
			if dep.Name == pkg {
				removing = true
				break
			}
		}

		if !removing {
			specsExpectedToStay = append(specsExpectedToStay, dep.Name)
		}
	}

	args := []string{
		"--lang",
		bt.Backend.Name,
		"remove",
	}
	_, err := bt.Exec(
		"upm",
		append(args, pkgs...)...,
	)

	if err != nil {
		bt.Fail("upm failed to remove: %v", err)
	}

	afterLockDeps := bt.UpmListLockFile()
	afterSpecDeps := bt.UpmListSpecFile()

	if len(beforeSpecDeps) <= len(afterSpecDeps) {
		bt.Fail("expected fewer deps in spec file after remove (before %d, after %d)", len(beforeSpecDeps), len(afterSpecDeps))
	}

	for _, pkg := range pkgs {
		if bt.Backend.QuirksIsReproducible() {
			for _, dep := range afterLockDeps {
				if dep.Name == pkg {
					bt.Fail("expected %s not in lock file after remove", pkg)
				}
			}
		}

		for _, dep := range afterSpecDeps {
			if dep.Name == pkg {
				bt.Fail("expected %s not in spec file after remove", pkg)
			}
		}
	}

	for _, afterDep := range afterSpecDeps {
		for ii, beforeDep := range specsExpectedToStay {
			if afterDep.Name == beforeDep {
				specsExpectedToStay = append(specsExpectedToStay[:ii], specsExpectedToStay[ii+1:]...)
				break
			}
		}
	}

	if len(specsExpectedToStay) != 0 {
		bt.Fail("upm removed %v from spec file", specsExpectedToStay)
	}
}

func (bt *BackendT) UpmSearch(query, expectName string) {
	out, err := bt.Exec(
		"upm",
		"--lang",
		bt.Backend.Name,
		"search",
		"--format",
		"json",
		query,
	)

	if err != nil {
		bt.t.Fatalf("upm failed to search: %v", err)
	}

	var results []api.PkgInfo
	err = json.NewDecoder(strings.NewReader(out.Stdout)).Decode(&results)
	if err != nil {
		bt.Fail("failed to decode json: %v", err)
	}

	found := false
	for _, result := range results {
		if result.Name == expectName {
			found = true
			break
		}
	}

	if !found {
		bt.Fail("expected %s in search results for query %s", expectName, query)
	}
}

func (bt *BackendT) UpmWhichLanguage() {
	out, err := bt.Exec("upm", "which-language")
	if err != nil {
		bt.Fail("upm failed to detect language: %v", err)
	}

	detected := strings.TrimSpace(out.Stdout)
	if detected != bt.Backend.Name {
		bt.Fail("expected %s, got %s", bt.Backend.Name, detected)
	}
}

func (bt *BackendT) UpmInstallReplitNixSystemDependencies() {
	_, err := bt.Exec(
		"upm",
		"--lang",
		bt.Backend.Name,
		"install-replit-nix-system-dependencies",
	)

	if err != nil {
		bt.Fail("upm failed to install-replit-nix-system-dependencies: %v", err)
	}
}
