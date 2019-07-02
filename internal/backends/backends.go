package backends

import (
	"strings"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

// If more than one backend might match the same project, then one
// that comes first in this list will be used.
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

func matchesLanguage(backend api.LanguageBackend, language string) bool {
	bParts := map[string]bool{}
	for _, bPart := range strings.Split(backend.Name, "-") {
		bParts[bPart] = true
	}
	for _, lPart := range strings.Split(language, "-") {
		if !bParts[lPart] {
			return false
		}
	}
	return true
}

func GetBackend(language string) api.LanguageBackend {
	backends := languageBackends
	if language != "" {
		filteredBackends := []api.LanguageBackend{}
		for _, b := range backends {
			if matchesLanguage(b, language) {
				filteredBackends = append(filteredBackends, b)
			}
		}
		switch len(filteredBackends) {
		case 0:
			util.Die("no such language: %s", language)
		case 1:
			return filteredBackends[0]
		default:
			backends = filteredBackends
		}

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
	if language == "" {
		util.Die("could not autodetect a language for your project")
	}
	return backends[0]
}

func GetBackendNames() []string {
	backendNames := []string{}
	for _, b := range languageBackends {
		backendNames = append(backendNames, b.Name)
	}
	return backendNames
}
