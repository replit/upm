package dotnet

import (
	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

// removes packages using dotnet command and updates lock file
func removePackages(pkgs map[api.PkgName]bool) {
	for packageName := range pkgs {
		command := []string{"dotnet", "remove", findSpecFile(), "package", string(packageName)}
		util.RunCmd(command)
	}
	lock()
}

// adds packages using dotnet command which automatically updates lock files
func addPackages(pkgs map[api.PkgName]api.PkgSpec, projectName string) {
	for packageName, spec := range pkgs {
		command := []string{"dotnet", "add", "package", string(packageName)}
		if string(spec) != "" {
			command = append(command, "--version", string(spec))
		}
		util.RunCmd(command)
	}
}

// installs all packages using dotnet command
func install() {
	util.RunCmd([]string{"dotnet", "restore"})
}

// generates or updates the lock file using dotnet command
func lock() {
	util.RunCmd([]string{"dotnet", "restore", "--use-lock-file"})
}
