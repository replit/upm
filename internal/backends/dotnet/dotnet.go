// Package dotnet provides a backend for c# using dotnet and nuget.org
package dotnet

import (
	"github.com/replit/upm/internal/api"
)

//DotNetBackend is the UPM language backend for C# using dotnet
var DotNetBackend = api.LanguageBackend{
	Name:             "csharp-dotnet",
	Specfile:         findSpecFile(),
	Lockfile:         lockFile,
	FilenamePatterns: []string{"*.cs", "*.csproj"},
	Remove:           removePackages,
	Add:              addPackages,
	Search:           search,
	Info:             info,
	Install:          install,
	Lock:             lock,
	ListSpecfile:     listSpecfile,
	ListLockfile:     listLockfile,
	GetPackageDir: func() string {
		return "bin/"
	},
	Quirks: api.QuirksAddRemoveAlsoLocks |
		api.QuirksAddRemoveAlsoInstalls |
		api.QuirksLockAlsoInstalls,
}
