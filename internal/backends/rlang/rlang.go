package rlang

import (
	"fmt"
	"regexp"
	"github.com/ALANVF/rgo"
	
	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

func getImports(imports string) []string {
	return regexp.MustCompile(`[a-zA-Z_]\w*`).FindAllString(imports, -1)
}

func createRPkgDir() {
	if _, err := os.Stat("~/R"); os.IsNotExist(err) {
		if err := os.MkdirAll("~/R/x86_64-pc-linux-gnu-library/3.4", os.ModeDir); err != nil {
			panic(err)
		}
	} else if err != nil {
		panic(err)
	}
}

func updateLibPaths() {
	rgo.Eval(`.libPaths("~/R/x86_64-pc-linux-gnu-library/3.4")`)
}

var Rlang = api.LanguageBackend {
	Name: "rlang",
	Specfile: "Rconfig.json",
	Lockfile: "Rconfig.json.lock",
	FilenamePatterns: []string {"*.r", "*.R"},
	Quirks: api.QuirksNone,
	Search: func(query string) []api.PkgInfo {
		pkgs := []api.PkgInfo {}
		
		for _, hit := range SearchPackages(query) {
			pkg := api.PkgInfo {
				Name: hit.Source.Package,
				Description: hit.Source.Title,
				Version: hit.Source.Version,
				HomepageURL: hit.Source.URL,
				DocumentationURL: "",
				SourceCodeURL: hit.Source.Repository,
				BugTrackerURL: hit.Source.BugReports,
				Author: hit.Source.Author,
				License: hit.Source.License,
				Dependencies: getImports(hit.Source.Imports),
			}

			pkgs = append(pkgs, pkg)
		}

		return pkgs
	},
	Info: func(name api.PkgName) api.PkgInfo {
		if pkg := SearchPackage(string(name)); pkg != nil {
			hit := *pkg
			return api.PkgInfo {
				Name: hit.Source.Package,
				Description: hit.Source.Title,
				Version: hit.Source.Version,
				HomepageURL: hit.Source.URL,
				DocumentationURL: "",
				SourceCodeURL: hit.Source.Repository,
				BugTrackerURL: hit.Source.BugReports,
				Author: hit.Source.Author,
				License: hit.Source.License,
				Dependencies: getImports(hit.Source.Imports),
			}
		} else {
			return api.PkgInfo {}
		}
	},
	Add: func(packages map[api.PkgName]api.PkgSpec) {
		for name, info := range packages {
			pkg := RPackage {
				Name: string(name),
				Version: string(info),
			}
			RAdd(pkg)
		}
	},
	Remove: func(packages map[api.PkgName]bool) {
		rgo.Open()

		updateLibPaths()

		for name, _ := range packages {
			RRemove(RPackage {Name: string(name)})

			rgo.Eval(`remove.packages("` + string(name) + `")`)
		}

		rgo.Close()
	},
	Lock: RLock,
	Install: func() {
		rgo.Open()

		createRPkgDir()
		updateLibPaths()

		for _, pkg := range RGetSpecFile().Packages {
			if !rgo.Eval(`find.package("` + pkg.Name + `", quiet=T)`).IsValidString() {
				rgo.Eval(`install.packages("` + pkg.Name + `")`)
			}
		}

		rgo.Close()
	},
	ListSpecfile: func() map[api.PkgName]api.PkgSpec {
		out := map[api.PkgName]api.PkgSpec {}

		for _, pkg := range RGetSpecFile().Packages {
			out[api.PkgName(pkg.Name)] = api.PkgSpec(pkg.Version)
		}

		return out
	},
	ListLockfile: func() map[api.PkgName]api.PkgVersion {
		out := map[api.PkgName]api.PkgVersion {}

		for _, pkg := range RGetSpecFile().Packages {
			out[api.PkgName(pkg.Name)] = api.PkgVersion(pkg.Version)
		}

		return out
	},
	//GuessRegexps: []*regexp.Regexp {regexp.MustCompile(`\brequire[ \t]*\(\s*([a-zA-Z_]\w*)\s*`)},
	Guess: func() (map[api.PkgName]bool, bool) {
		util.NotImplemented()
		
		return nil, false
	},
}