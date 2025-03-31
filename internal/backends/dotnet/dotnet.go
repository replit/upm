// Package dotnet provides a backend for c# using dotnet and nuget.org
package dotnet

import (
	"context"
	"os/exec"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/nix"
	"github.com/replit/upm/internal/util"
)

func dotnetIsAvailable() bool {
	_, err := exec.LookPath("dotnet")
	return err == nil
}

// DotNetBackend is the UPM language backend .NET languages with support for C#
var DotNetBackend = api.LanguageBackend{
	Name:             "dotnet",
	Specfile:         findSpecFile(),
	Lockfile:         lockFileName,
	IsAvailable:      dotnetIsAvailable,
	FilenamePatterns: []string{"*.cs", "*.csproj", "*.fs", "*.fsproj"},
	Remove: func(ctx context.Context, pkgs map[api.PkgName]bool) {
		removePackages(ctx, pkgs, findSpecFile(), util.RunCmd)
	},
	Add: func(ctx context.Context, pkgs map[api.PkgName]api.PkgCoordinates, projectName string) {
		addPackages(ctx, pkgs, projectName, util.RunCmd)
	},
	Search:       search,
	Info:         info,
	Install:      func(ctx context.Context) { install(ctx, util.RunCmd) },
	Lock:         func(ctx context.Context) { lock(ctx, util.RunCmd) },
	ListSpecfile: listSpecfile,
	ListLockfile: listLockfile,
	GetPackageDir: func() string {
		return "bin/"
	},
	Quirks: api.QuirksAddRemoveAlsoLocks |
		api.QuirksAddRemoveAlsoInstalls |
		api.QuirksLockAlsoInstalls,
	InstallReplitNixSystemDependencies: nix.DefaultInstallReplitNixSystemDependencies,
	InstallDotReplitSystemDependencies: nix.DefaultInstallDotReplitSystemDependencies,
}
