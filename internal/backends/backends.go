package backends

import (
	"strings"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/backends/elisp"
	"github.com/replit/upm/internal/backends/nodejs"
	"github.com/replit/upm/internal/backends/python"
	"github.com/replit/upm/internal/backends/ruby"
	"github.com/replit/upm/internal/util"
)

// If more than one backend might match the same project, then one
// that comes first in this list will be used.
var languageBackends = []api.LanguageBackend{
	python.Python3Backend,
	python.Python2Backend,
	nodejs.NodejsBackend,
	ruby.RubyBackend,
	elisp.ElispBackend,
}

func matchesLanguage(b api.LanguageBackend, language string) bool {
	bParts := map[string]bool{}
	for _, bPart := range strings.Split(b.Name, "-") {
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

func CheckAll() {
	for _, b := range languageBackends {
		b.Check()
	}
}
