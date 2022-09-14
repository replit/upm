package dotnet

import (
	"strings"
	"testing"

	"github.com/replit/upm/internal/api"
)

func TestAddPackages(t *testing.T) {
	cmds := []string{}
	cmdRunner := func(cmd []string) {
		cmds = append(cmds, strings.Join(cmd, " "))
	}

	addPackages(map[api.PkgName]api.PkgSpec{"package": "1.0"}, "", cmdRunner)

	if len(cmds) != 1 {
		t.Errorf("Expected one command but got %q", len(cmds))
	}

	if cmds[0] != "dotnet add package package --version 1.0" {
		t.Errorf("Wrong command executed %s", cmds[0])
	}
}

func TestAddPackagesWithoutVersion(t *testing.T) {
	cmds := []string{}
	cmdRunner := func(cmd []string) {
		cmds = append(cmds, strings.Join(cmd, " "))
	}

	addPackages(map[api.PkgName]api.PkgSpec{"package": ""}, "", cmdRunner)

	if len(cmds) != 1 {
		t.Errorf("Expected one command but got %q", len(cmds))
	}

	if cmds[0] != "dotnet add package package" {
		t.Errorf("Wrong command executed %s", cmds[0])
	}
}

func TestRemovePackages(t *testing.T) {
	cmds := []string{}
	cmdRunner := func(cmd []string) {
		cmds = append(cmds, strings.Join(cmd, " "))
	}

	removePackages(map[api.PkgName]bool{"package": true}, "specFile.csproj", cmdRunner)

	if len(cmds) != 2 {
		t.Errorf("Expected two command but got %q", len(cmds))
	}

	if cmds[0] != "dotnet remove specFile.csproj package package" {
		t.Errorf("Wrong remove command executed %s", cmds[0])
	}

	if cmds[1] != "dotnet restore --use-lock-file" {
		t.Errorf("Wrong lock command executed %s", cmds[1])
	}
}

func TestLock(t *testing.T) {
	cmds := []string{}
	cmdRunner := func(cmd []string) {
		cmds = append(cmds, strings.Join(cmd, " "))
	}

	lock(cmdRunner)

	if len(cmds) != 1 {
		t.Errorf("Expected one command but got %q", len(cmds))
	}

	if cmds[0] != "dotnet restore --use-lock-file" {
		t.Errorf("Wrong command executed %s", cmds[0])
	}
}

func TestInstall(t *testing.T) {
	cmds := []string{}
	cmdRunner := func(cmd []string) {
		cmds = append(cmds, strings.Join(cmd, " "))
	}

	install(cmdRunner)

	if len(cmds) != 1 {
		t.Errorf("Expected one command but got %q", len(cmds))
	}

	if cmds[0] != "dotnet restore" {
		t.Errorf("Wrong command executed %s", cmds[0])
	}
}
