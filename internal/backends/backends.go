package backends

import (
	"strings"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

var languageBackends = []api.LanguageBackend{
	pythonBackend,
	nodejsBackend,
	rubyBackend,
	elispBackend,
}

// Keep up to date with languageBackend in types.go
func CheckAll() {
	for _, b := range languageBackends {
		if b.Name == "" ||
			b.Specfile == "" ||
			b.Lockfile == "" ||
			b.Search == nil ||
			b.Info == nil ||
			b.Add == nil ||
			b.Remove == nil ||
			// The lock method should be unimplemented if
			// and only if builds are not reproducible.
			((b.Lock == nil) != api.QuirksIsNotReproducible(b)) ||
			b.Install == nil ||
			b.ListSpecfile == nil ||
			b.ListLockfile == nil ||
			b.Guess == nil {
			util.Panicf("language backend %s is incomplete", b.Name)
		}
	}
}

func GetBackend(language string) api.LanguageBackend {
	backends := languageBackends
	if language != "" {
		for _, b := range backends {
			if b.Name == language {
				return b
			}
		}
		filteredBackends := []api.LanguageBackend{}
		for _, b := range backends {
			if strings.HasPrefix(b.Name, language+"-") {
				filteredBackends = append(filteredBackends, b)
			}
		}
		if len(filteredBackends) == 0 {
			util.Die("no such language: %s", language)
		}
		backends = filteredBackends
	}
	for _, b := range backends {
		if util.FileExists(b.Specfile) ||
			util.FileExists(b.Lockfile) {
			return b
		}
	}
	for _, b := range backends {
		for _, p := range b.FilenamePatterns {
			if util.PatternExists(p) {
				return b
			}
		}
	}
	util.Die("could not autodetect a language for your project")
	return api.LanguageBackend{}
}

func GetBackendNames() []string {
	backendNames := []string{}
	for _, b := range languageBackends {
		backendNames = append(backendNames, b.Name)
	}
	return backendNames
}
