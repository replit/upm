package dotnet

import (
	"fmt"
	"io/ioutil"
	"strings"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

func removePackages(pkgs map[api.PkgName]bool) {}

func addPackages(pkgs map[api.PkgName]api.PkgSpec, projectName string) {}

func search(query string) []api.PkgInfo {
	return []api.PkgInfo{}
}

func findSpecFile() string {
	files, err := ioutil.ReadDir("./")
	if err != nil {
		util.Die("Can't read current directory")
	}

	for _, f := range files {
		if strings.HasSuffix(f.Name(), ".csproj") {
			return f.Name()
		}
	}

	return ".csproj"
}

func info(pkgName api.PkgName) api.PkgInfo {
	return api.PkgInfo{}
}

func listSpecfile() map[api.PkgName]api.PkgSpec {
	pkgs := map[api.PkgName]api.PkgSpec{}
	util.ProgressMsg("looking for project file")
	projectFile := findSpecFile()
	if util.Exists(projectFile) {
		util.ProgressMsg(fmt.Sprintf("Found %s", projectFile))
	}
	return pkgs
}

func listLockfile() map[api.PkgName]api.PkgVersion {
	pkgs := map[api.PkgName]api.PkgVersion{}
	return pkgs
}

//DotNetBackend is the UPM language backend for C# using dotnet
var DotNetBackend = api.LanguageBackend{
	Name:             "csharp-dotnet",
	Specfile:         findSpecFile(),
	Lockfile:         "packages.lock.json",
	FilenamePatterns: []string{"*.cs", "*.csproj"},
	Remove:           removePackages,
	Add:              addPackages,
	Search:           search,
	Info:             info,
	Install:          func() {},
	Lock:             func() {},
	ListSpecfile:     listSpecfile,
	ListLockfile:     listLockfile,
	GetPackageDir: func() string {
		return "bin/"
	},
}
