package api

func QuirksIsNotReproducible(b LanguageBackend) bool {
	return (b.Quirks & QuirksNotReproducible) != 0
}

func QuirksIsReproducible(b LanguageBackend) bool {
	return (b.Quirks & QuirksNotReproducible) == 0
}
