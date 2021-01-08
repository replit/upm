// Package nix provides backends for the nix package manager. This is
// experimental.
package nix

import (
	"encoding/json"
	"os/exec"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

type nixSearchJSON map[string]struct {
	PkgName     string `json:"pkgName"`
	Version     string `json:"version"`
	Description string `json:"description"`
}

func nixSearch(query string) []api.PkgInfo {
	cmd := exec.Command("nix", "search", "--json", "--", query)
	out, err := cmd.Output()
	if err != nil {

		/*
			if eerr, ok := err.(*exec.ExitError); ok {
				util.Die("nix search failed %v: %s", err, eerr.Stderr)
			} else {
				util.Die("nix search failed %v", err)
			}
		*/
	}

	var nixSearchResult nixSearchJSON

	if err := json.Unmarshal(out, &nixSearchResult); err != nil {
		util.Die("nix output parsing failed %v", err)
	}

	var pkgs []api.PkgInfo

	for _, p := range nixSearchResult {
		pkgs = append(pkgs, api.PkgInfo{
			Name:        p.PkgName,
			Description: p.Description,
			Version:     p.Version,
		})
	}

	return pkgs
}

func nixInfo(api.PkgName) api.PkgInfo {
	return api.PkgInfo{}
}

func nixAdd(pkgs map[api.PkgName]api.PkgSpec, projectName string) {
	cmd := []string{"nix-env", "-iA"}
	for name, _ := range pkgs {
		name := "nixpkgs." + string(name)
		cmd = append(cmd, name)
	}
	util.RunCmd(cmd)
}

func nixRemove(pkgs map[api.PkgName]bool) {
}

func listLockfile() map[api.PkgName]api.PkgVersion {
	return nil
}

func nixInstall() {
}

func nixLock() {
}

func listSpecfile() map[api.PkgName]api.PkgSpec {
	return nil
}

// NixBackend is a UPM backend for Node.js that uses NPM.
var NixBackend = api.LanguageBackend{
	Name:             "nix",
	Specfile:         "TODO",
	Lockfile:         "TODO",
	FilenamePatterns: []string{"*"},
	Quirks: api.QuirksAddRemoveAlsoLocks |
		api.QuirksAddRemoveAlsoInstalls |
		api.QuirksLockAlsoInstalls, // TODO
	GetPackageDir: func() string {
		return "TODO"
	},
	Search:       nixSearch,
	Info:         nixInfo,
	Add:          nixAdd,
	Remove:       nixRemove,
	Lock:         nixLock,
	Install:      nixInstall,
	ListSpecfile: listSpecfile,
	ListLockfile: listLockfile,
}
