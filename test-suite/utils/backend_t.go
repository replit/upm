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
	t         *testing.T
	templates *http.FileSystem
	testDir   string
}

func (bt BackendT) assertT() {
	if bt.t == nil {
		panic("BackendT not started")
	}
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

func (bt *BackendT) Start(t *testing.T) {
	bt.t = t
}

func (bt BackendT) Subtest(name string, test func(bt BackendT)) {
	bt.assertT()
	bt.t.Run(name, func(t *testing.T) {
		inner := InitBackendT(bt.Backend, bt.templates)
		inner.t = t
		test(inner)
	})
}

func (bt *BackendT) TestDir() string {
	if bt.testDir == "" {
		bt.assertT()
		bt.testDir = bt.t.TempDir()
	}

	return bt.testDir
}

func (bt *BackendT) AddTestFile(template, as string) {
	templates := *bt.templates

	f, err := templates.Open(template)
	if err != nil {
		bt.t.Fatalf("failed to get template %s: %v", template, err)
	}

	dstPath := path.Join(bt.TestDir(), as)
	dst, err := os.OpenFile(dstPath, os.O_CREATE|os.O_TRUNC|os.O_WRONLY, 0644)
	if err != nil {
		bt.t.Fatalf("failed to open or create test file %s: %v", dstPath, err)
	}

	_, err = io.Copy(dst, f)
	if err != nil {
		bt.t.Fatalf("failed to read template %s: %v", template, err)
	}
}

func (bt *BackendT) Exec(command string, args ...string) (struct{ Stdout, Stderr string }, error) {
	cmd := exec.Command(command, args...)
	cmd.Dir = bt.TestDir()

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

func (bt *BackendT) UpmWhichLanguage(expect string) {
	out, err := bt.Exec("upm", "which-language")
	if err != nil {
		bt.t.Fatalf("upm failed to detect language: %v", err)
	}

	detected := strings.TrimSpace(out.Stdout)
	if detected != expect {
		bt.t.Fatalf("expected %s, got %s", expect, detected)
	}
}
