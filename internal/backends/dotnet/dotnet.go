// Package dotnet provides a backend for c# using dotnet and nuget.org
package dotnet

import (
	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

// DotNetBackend is the UPM language backend .NET languages with support for C#
var DotNetBackend = api.LanguageBackend{
	Name:             "dotnet",
	Specfile:         findSpecFile(),
	Lockfile:         lockFileName,
	FilenamePatterns: []string{"*.cs", "*.csproj", "*.fs", "*.fsproj"},
	Remove:           func(pkgs map[api.PkgName]bool) { removePackages(pkgs, findSpecFile(), util.RunCmd) },
	Add: func(pkgs map[api.PkgName]api.PkgSpec, projectName string) {
		addPackages(pkgs, projectName, util.RunCmd)
	},
	Search:       search,
	Info:         info,
	Install:      func() { install(util.RunCmd) },
	Lock:         func() { lock(util.RunCmd) },
	ListSpecfile: listSpecfile,
	ListLockfile: listLockfile,
	GetPackageDir: func() string {
		return "bin/"
	},
	Quirks: api.QuirksAddRemoveAlsoLocks |
		api.QuirksAddRemoveAlsoInstalls |
		api.QuirksLockAlsoInstalls,
}
