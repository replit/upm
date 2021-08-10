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

	libPath := string(util.GetCmdOutput([]string{
		"R",
		"-s",
		"-e",
		`cat(.expand_R_libs_env_var("R/%p-library/%v"))`,
	}))

	libPath = path.Join(os.Getenv("HOME"), libPath)

	os.Setenv("R_LIBS_USER", libPath)

	return libPath
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

func installRPkg(name string) bool {
	name = normalizePkgName(name)

	ifNotInstalled := "if(length(find.package('" + name + "', quiet=T)) == 0) "

	return util.GetExitCode([]string{
		"R",
		"-q",
		"-e",
		ifNotInstalled + "install.packages('" + name + "'); " + ifNotInstalled + "q('no', 1)",
	}, false, true) == 0
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
	Lockfile:         "Rconfig.lock.json",
	FilenamePatterns: []string{"*.r", "*.R"},
	Quirks:           api.QuirksNone,
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
	Add: func(packages map[api.PkgName]api.PkgSpec, projectName string) {
		for name, info := range packages {
			RAdd(RPackage{
				Name:    string(name),
				Version: string(info),
			})
		}
	},
	Remove: func(packages map[api.PkgName]bool) {
		for name := range packages {
			RRemove(RPackage{Name: string(name)})

			_ = util.GetExitCode([]string{
				"R",
				"-q",
				"-e",
				"remove.packages('" + normalizePkgName(string(name)) + "')",
			}, false, true)
		}
	},
	Lock: RLock,
	Install: func() {
		createRPkgDir()

		for _, pkg := range RGetSpecFile().Packages {
			if !installRPkg(pkg.Name) {
				RRemove(pkg)
				RLock()
			}
		}
	},
	ListSpecfile: func() map[api.PkgName]api.PkgSpec {
		pkgs := map[api.PkgName]api.PkgSpec{}
		for _, pkg := range RGetSpecFile().Packages {
			pkgs[api.PkgName(pkg.Name)] = api.PkgSpec(pkg.Version)
		}
		return pkgs
	},
	ListLockfile: func() map[api.PkgName]api.PkgVersion {
		pkgs := map[api.PkgName]api.PkgVersion{}
		for _, pkg := range RGetSpecFile().Packages {
			pkgs[api.PkgName(pkg.Name)] = api.PkgVersion(pkg.Version)
		}
		return pkgs
	},
	//GuessRegexps: []*regexp.Regexp {regexp.MustCompile(`\brequire[ \t]*\(\s*([a-zA-Z_]\w*)\s*`)},
	Guess: func() (map[api.PkgName]bool, bool) {
		util.NotImplemented()

		return nil, false
	},
}
