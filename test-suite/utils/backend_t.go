package testUtils

import (
	"embed"
	"io"
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
	templates *embed.FS
	testDir   string
}

func (bt BackendT) assertT() {
	if bt.t == nil {
		panic("BackendT not started")
	}
}

func InitBackendT(
	backend api.LanguageBackend,
	templates *embed.FS,
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

// AddTestDir relies on test-suite/templates/templates.go's embedded files,
// iterating over every bt.Backend.Name+"/"+tmpl recursively and copying them
// into the ephemeral test directory.
//
// Note, this relies on empty path segments, since the source and target path
// formats are slightly different. The source path includes the
// test classifier (no-deps, one-dep, etc), whereas the target expects every
// copied file to be copied into TestDir directly.
func (bt *BackendT) AddTestDir(tmpl string) {
	var rec func(cur string)
	rec = func(cur string) {
		entries, _ := bt.templates.ReadDir(path.Join(bt.Backend.Name, tmpl, cur))
		for _, entry := range entries {
			// Combine the current path segment with the src or dst roots
			srcpath := path.Join(bt.Backend.Name, tmpl, cur, entry.Name())
			dstpath := path.Join(bt.TestDir(), cur, entry.Name())
			if entry.IsDir() {
				err := os.MkdirAll(dstpath, 0o755)
				if err != nil {
					bt.Fail("Unable to create %s", dstpath)
				}
				rec(path.Join(cur, entry.Name()))
			} else {
				f, err := bt.templates.Open(srcpath)
				if err != nil {
					bt.t.Fatalf("failed to get template %s: %v", srcpath, err)
				}

				dst, err := os.OpenFile(dstpath, os.O_CREATE|os.O_TRUNC|os.O_WRONLY, 0o644)
				if err != nil {
					bt.t.Fatalf("failed to open or create test file %s: %v", dstpath, err)
				}

				_, err = io.Copy(dst, f)
				if err != nil {
					bt.t.Fatalf("failed to read template %s: %v", srcpath, err)
				}
			}
		}
	}

	rec("")
}

func (bt *BackendT) RemoveTestFile(fpath string) {
	fullPath := path.Join(bt.TestDir(), fpath)
	os.Remove(fullPath)
}

func (bt *BackendT) AddTestFile(template, as string) {
	templates := *bt.templates

	f, err := templates.Open(template)
	if err != nil {
		bt.t.Fatalf("failed to get template %s: %v", template, err)
	}

	dstPath := path.Join(bt.TestDir(), as)
	dst, err := os.OpenFile(dstPath, os.O_CREATE|os.O_TRUNC|os.O_WRONLY, 0o644)
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
