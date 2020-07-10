package rlang

import (
	"os"
	"regexp"
	"path"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

func getImports(imports string) []string {
	return regexp.MustCompile(`[a-zA-Z_]\w*`).FindAllString(imports, -1)
}

func getRPkgDir() string {
	if rLibsUser := os.Getenv("R_LIBS_USER"); rLibsUser != "" {
		return rLibsUser
	}

	return path.Join(os.Getenv("HOME"), "R", "x86_64-pc-linux-gnu-library", "3.4")
}

func createRPkgDir() {
	dir := getRPkgDir()

	if _, err := os.Stat(dir); os.IsNotExist(err) {
		if err = os.MkdirAll(dir, os.ModeDir + os.ModePerm); err != nil {
			panic(err)
		}
	} else if err != nil {
		panic(err)
	}
}

// RlangBackend is a custom UPM backend for R
var RlangBackend = api.LanguageBackend{
	Name:             "rlang",
	Specfile:         "Rconfig.json",
	Lockfile:         "Rconfig.json.lock",
	FilenamePatterns: []string{"*.r", "*.R"},
	Quirks:           api.QuirksLockAlsoInstalls,
	GetPackageDir:    getRPkgDir,
	Search: func(query string) []api.PkgInfo {
		pkgs := []api.PkgInfo{}

		for _, hit := range SearchPackages(query) {
			pkg := api.PkgInfo{
				Name:             hit.Source.Package,
				Description:      hit.Source.Title,
				Version:          hit.Source.Version,
				HomepageURL:      hit.Source.URL,
				DocumentationURL: "",
				SourceCodeURL:    hit.Source.Repository,
				BugTrackerURL:    hit.Source.BugReports,
				Author:           hit.Source.Author,
				License:          hit.Source.License,
				Dependencies:     getImports(hit.Source.Imports),
			}

			pkgs = append(pkgs, pkg)
		}

		return pkgs
	},
	Info: func(name api.PkgName) api.PkgInfo {
		if pkg := SearchPackage(string(name)); pkg != nil {
			hit := *pkg
			return api.PkgInfo{
				Name:             hit.Source.Package,
				Description:      hit.Source.Title,
				Version:          hit.Source.Version,
				HomepageURL:      hit.Source.URL,
				DocumentationURL: "",
				SourceCodeURL:    hit.Source.Repository,
				BugTrackerURL:    hit.Source.BugReports,
				Author:           hit.Source.Author,
				License:          hit.Source.License,
				Dependencies:     getImports(hit.Source.Imports),
			}
		}

		return api.PkgInfo{}
	},
	Add: func(packages map[api.PkgName]api.PkgSpec) {
		createRPkgDir()
		
		for name, info := range packages {
			pkg := RPackage{
				Name:    string(name),
				Version: string(info),
			}
			RAdd(pkg)
			
			util.RunCmd([]string{
				"R",
				"--no-echo",
				"-e",
				"if(length(find.package('" + pkg.Name + "', quiet=T)) == 0) install.packages('" + pkg.Name + "')",
			})
		}
	},
	Remove: func(packages map[api.PkgName]bool) {
		for name := range packages {
			RRemove(RPackage{Name: string(name)})
			
			util.RunCmd([]string{
				"R",
				"--no-echo",
				"-e",
				"remove.packages('" + string(name) + "')",
			})
		}
	},
	Lock: RLock,
	Install: func() {
		createRPkgDir()
		
		for _, pkg := range RGetSpecFile().Packages {
			util.RunCmd([]string{
				"R",
				"--no-echo",
				"-e",
				"if(length(find.package('" + pkg.Name + "', quiet=T)) == 0) install.packages('" + pkg.Name + "')",
			})
		}
	},
	ListSpecfile: func() map[api.PkgName]api.PkgSpec {
		out := map[api.PkgName]api.PkgSpec{}

		for _, pkg := range RGetSpecFile().Packages {
			out[api.PkgName(pkg.Name)] = api.PkgSpec(pkg.Version)
		}

		return out
	},
	ListLockfile: func() map[api.PkgName]api.PkgVersion {
		out := map[api.PkgName]api.PkgVersion{}

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
