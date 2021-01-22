package dotnet

import (
	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

func removePackages(pkgs map[api.PkgName]bool) {}

func addPackages(pkgs map[api.PkgName]api.PkgSpec, projectName string) {}

func search(query string) []api.PkgInfo {
	return []api.PkgInfo{}
}

func info(pkgName api.PkgName) api.PkgInfo {
	return api.PkgInfo{}
}

func listSpecfile() map[api.PkgName]api.PkgSpec {
	pkgs := map[api.PkgName]api.PkgSpec{}
	util.ProgressMsg("looking for project file")

	return pkgs
}

func listLockfile() map[api.PkgName]api.PkgVersion {
	pkgs := map[api.PkgName]api.PkgVersion{}
	return pkgs
}

//DotNetBackend is the UPM language backend for C# using dotnet
var DotNetBackend = api.LanguageBackend{
	Name:             "csharp-dotnet",
	Specfile:         "*.csproj",
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
