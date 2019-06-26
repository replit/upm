package backends

import (
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
			b.Detect == nil ||
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
	if language != "" {
		for _, languageBackend := range languageBackends {
			if languageBackend.Name == language {
				return languageBackend
			}
		}
	}
	for _, languageBackend := range languageBackends {
		if languageBackend.Detect() {
			return languageBackend
		}
	}
	if language != "" {
		util.Die("no such language: %s", language)
	} else {
		util.Die("could not autodetect a language for your project")
	}
	return api.LanguageBackend{}
}

func GetBackendNames() []string {
	backendNames := []string{}
	for _, languageBackend := range languageBackends {
		backendNames = append(backendNames, languageBackend.Name)
	}
	return backendNames
}
