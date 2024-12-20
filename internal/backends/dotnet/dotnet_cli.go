package dotnet

import (
	"context"

	"github.com/replit/upm/internal/api"
	"gopkg.in/DataDog/dd-trace-go.v1/ddtrace/tracer"
)

// removes packages using dotnet command and updates lock file
func removePackages(ctx context.Context, pkgs map[api.PkgName]bool, specFileName string, cmdRunner func([]string)) {
	span, ctx := tracer.StartSpanFromContext(ctx, "dotnet remove")
	defer span.Finish()
	for packageName := range pkgs {
		command := []string{"dotnet", "remove", specFileName, "package", string(packageName)}
		cmdRunner(command)
	}
	lock(ctx, cmdRunner)
}

// adds packages using dotnet command which automatically updates lock files
func addPackages(ctx context.Context, pkgs map[api.PkgName]api.PkgCoordinates, projectName string, cmdRunner func([]string)) {
	//nolint:ineffassign,wastedassign,staticcheck
	span, ctx := tracer.StartSpanFromContext(ctx, "dotnet add package")
	defer span.Finish()
	for packageName, coords := range pkgs {
		command := []string{"dotnet", "add", "package", string(packageName)}
		if string(coords.Spec) != "" {
			command = append(command, "--version", string(coords.Spec))
		}
		cmdRunner(command)
	}
}

// installs all packages using dotnet command
func install(ctx context.Context, cmdRunner func([]string)) {
	//nolint:ineffassign,wastedassign,staticcheck
	span, ctx := tracer.StartSpanFromContext(ctx, "dotnet restore")
	defer span.Finish()
	cmdRunner([]string{"dotnet", "restore"})
}

// generates or updates the lock file using dotnet command
func lock(ctx context.Context, cmdRunner func([]string)) {
	//nolint:ineffassign,wastedassign,staticcheck
	span, ctx := tracer.StartSpanFromContext(ctx, "dotnet restore --use-lock-file")
	defer span.Finish()
	cmdRunner([]string{"dotnet", "restore", "--use-lock-file"})
}
