package api

import (
	"github.com/replit/upm/internal/util"
)

func QuirksIsNotReproducible(b LanguageBackend) bool {
	return (b.Quirks & QuirksNotReproducible) != 0
}

func QuirksIsReproducible(b LanguageBackend) bool {
	return (b.Quirks & QuirksNotReproducible) == 0
}

func (b *LanguageBackend) SpecfileExists() bool {
	for _, specfile := range b.Specfiles {
		if util.FileExists(specfile) {
			return true
		}
	}

	return false
}
