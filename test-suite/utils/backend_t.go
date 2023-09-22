package testUtils

import (
	"io"
	"net/http"
	"os"
	"os/exec"
	"path"
	"strings"
	"testing"

	"github.com/replit/upm/internal/api"
)

type BackendT struct {
	Backend   api.LanguageBackend
	T         *testing.T
	templates *http.FileSystem
	testDir   string
}

func InitBackendT(
	backend api.LanguageBackend,
	templates *http.FileSystem,
) BackendT {
	return BackendT{
		Backend:   backend,
		templates: templates,
	}
}

func (bt *BackendT) TestDir() string {
	if bt.testDir == "" {
		bt.testDir = bt.T.TempDir()
	}

	return bt.testDir
}

func (bt *BackendT) AddTestFile(template, as string) {
	f, err := (*bt.templates).Open(template)
	if err != nil {
		bt.T.Fatalf("failed to get template %s: %v", template, err)
	}

	dstPath := path.Join(bt.TestDir(), as)
	dst, err := os.OpenFile(dstPath, os.O_CREATE|os.O_TRUNC|os.O_WRONLY, 0644)
	if err != nil {
		bt.T.Fatalf("failed to open or create test file %s: %v", dstPath, err)
	}

	_, err = io.Copy(dst, f)
	if err != nil {
		bt.T.Fatalf("failed to read template %s: %v", template, err)
	}
}

func (bt *BackendT) Exec(command string, args ...string) (struct{ Stdout, Stderr string }, error) {
	cmd := exec.Command(command, args...)
	cmd.Dir = bt.testDir

	stdout := strings.Builder{}
	cmd.Stdout = &stdout

	stderr := strings.Builder{}
	cmd.Stderr = &stderr

	err := cmd.Run()

	return struct{ Stdout, Stderr string }{
		Stdout: stdout.String(),
		Stderr: stderr.String(),
	}, err
}

func (bt *BackendT) UpmWhichLanguage() (string, error) {
	out, err := bt.Exec("upm", "which-language")
	if err != nil {
		return "", err
	}

	return strings.TrimSpace(out.Stdout), nil
}
