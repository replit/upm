package rlang

import (
	"os"
	"path"
	"regexp"
	"strings"

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
		if err = os.MkdirAll(dir, os.ModeDir+os.ModePerm); err != nil {
			panic(err)
		}
	} else if err != nil {
		panic(err)
	}
}

func installRPkg(name string) {
	if strings.Contains(name, "'") {
		name = strings.ReplaceAll(name, "'", `\'`)
	}

	// Checking this first makes sure that we aren't running regex on everything
	if strings.HasSuffix(name, `\`) {
		suffix := regexp.MustCompile(`\\+$`).FindString(name)
		if len(suffix)%2 == 1 {
			name += `\`
		}
	}

	util.RunCmd([]string{
		"R",
		"--no-echo",
		"-e",
		"if(length(find.package('" + name + "', quiet=T)) == 0) install.packages('" + name + "')",
	})
}

func normalizePkgName(name string) string {
	if strings.Contains(name, "'") {
		name = strings.ReplaceAll(name, "'", `\'`)
	}

	// Checking this first makes sure that we aren't running regex on everything
	if strings.HasSuffix(name, `\`) {
		suffix := regexp.MustCompile(`\\+$`).FindString(name)
		if len(suffix)%2 == 1 {
			name += `\`
		}
	}

	return name
}

// RlangBackend is a custom UPM backend for R
var RlangBackend = api.LanguageBackend{
	Name:             "rlang",
	Specfile:         "Rconfig.json",
	Lockfile:         "Rconfig.json.lock",
	FilenamePatterns: []string{"*.r", "*.R"},
	Quirks:           api.QuirksLockAlsoInstalls,
	GetPackageDir:    getRPkgDir,
	Search: func(query string) (pkgs []api.PkgInfo) {
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
		return
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
			installRPkg(pkg.Name)
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
			installRPkg(pkg.Name)
		}
	},
	ListSpecfile: func() (pkgs map[api.PkgName]api.PkgSpec) {
		for _, pkg := range RGetSpecFile().Packages {
			pkgs[api.PkgName(pkg.Name)] = api.PkgSpec(pkg.Version)
		}
		return
	},
	ListLockfile: func() (pkgs map[api.PkgName]api.PkgVersion) {
		for _, pkg := range RGetSpecFile().Packages {
			pkgs[api.PkgName(pkg.Name)] = api.PkgVersion(pkg.Version)
		}
		return
	},
	//GuessRegexps: []*regexp.Regexp {regexp.MustCompile(`\brequire[ \t]*\(\s*([a-zA-Z_]\w*)\s*`)},
	Guess: func() (map[api.PkgName]bool, bool) {
		util.NotImplemented()

		return nil, false
	},
}
