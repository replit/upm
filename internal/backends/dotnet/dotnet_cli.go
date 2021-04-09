package dotnet

import (
	"github.com/replit/upm/internal/api"
)

// removes packages using dotnet command and updates lock file
func removePackages(pkgs map[api.PkgName]bool, specFileName string, cmdRunner func([]string)) {
	for packageName := range pkgs {
		command := []string{"dotnet", "remove", specFileName, "package", string(packageName)}
		cmdRunner(command)
	}
	lock(cmdRunner)
}

// adds packages using dotnet command which automatically updates lock files
func addPackages(pkgs map[api.PkgName]api.PkgSpec, projectName string, cmdRunner func([]string)) {
	for packageName, spec := range pkgs {
		command := []string{"dotnet", "add", "package", string(packageName)}
		if string(spec) != "" {
			command = append(command, "--version", string(spec))
		}
		cmdRunner(command)
	}
}

// installs all packages using dotnet command
func install(cmdRunner func([]string)) {
	cmdRunner([]string{"dotnet", "restore"})
}

// generates or updates the lock file using dotnet command
func lock(cmdRunner func([]string)) {
	cmdRunner([]string{"dotnet", "restore", "--use-lock-file"})
}
