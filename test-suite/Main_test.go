package testSuite

import (
	"context"
	"fmt"
	"os"
	"strings"

	"github.com/replit/upm/internal/backends"
	"github.com/replit/upm/test-suite/templates"
	testUtils "github.com/replit/upm/test-suite/utils"
)

var languageBackends []testUtils.BackendT

var standardTemplates = []string{
	"no-deps",
	"one-dep",
	"many-deps",
}

func init() {
	backends.SetupAll()

	fmt.Println("Preparing test suites:")
	for _, bn := range backends.GetBackendNames() {
		prefix := os.Getenv("UPM_SUITE_PREFIX")
		if !strings.HasPrefix(bn, prefix) {
			continue
		}
		fmt.Println("- " + bn)
		bt := testUtils.InitBackendT(backends.GetBackend(context.Background(), bn), &templates.FS)
		languageBackends = append(languageBackends, bt)
	}
	fmt.Println()
}
