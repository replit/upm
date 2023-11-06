package backends

import (
	"context"
	"os"
	"path/filepath"
	"testing"
)

func TestGetBackends(t *testing.T) {
	fileToBackend := map[string]string{
		"Setup.cs":       "dotnet",
		"project.csproj": "dotnet",
		"Setup.fs":       "dotnet",
		"project.fsproj": "dotnet",
		"pom.xml":        "java-maven",
		"package.json":   "nodejs-npm",
		"Cargo.toml":     "rust",
	}

	dir, err := os.MkdirTemp("", "TestGetBackends")
	if err != nil {
		t.Errorf("failed to create a temp directory %v", err)
	}
	defer os.RemoveAll(dir)

	for file, backend := range fileToBackend {
		tmpfile := filepath.Join(dir, file)

		if err := os.WriteFile(tmpfile, []byte{}, 0666); err != nil {
			t.Errorf("failed to create empty file: %s err: %v", tmpfile, err)
		}

		if err := os.Chdir(dir); err != nil {
			t.Errorf("failed to change to directory: %s err: %v", dir, err)
		}

		actualBackend := GetBackend(context.Background(), "")
		if backend != actualBackend.Name {
			t.Errorf("expected backend: %s but got backend %s", backend, actualBackend.Name)
		}
		os.Remove(tmpfile)
	}
}
