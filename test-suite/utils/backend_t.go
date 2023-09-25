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

func (bt BackendT) Fail(fmt string, args ...interface{}) {
	bt.t.Helper()
	bt.t.Fatalf(fmt, args...)
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
	bt.Log("$ %s %s", command, strings.Join(args, " "))

	cmd := exec.Command(command, args...)
	cmd.Dir = bt.TestDir()

	stdout := strings.Builder{}
	cmd.Stdout = &stdout

	stderr := strings.Builder{}
	cmd.Stderr = &stderr

	err := cmd.Run()

	Stdout := stdout.String()
	Stderr := stderr.String()

	bt.Log("=== STDOUT ===\n%s", Stdout)
	bt.Log("=== STDERR ===\n%s", Stderr)

	return struct{ Stdout, Stderr string }{
		Stdout,
		Stderr,
	}, err
}

func (bt *BackendT) Log(format string, args ...interface{}) {
	bt.t.Logf(format, args...)
}
