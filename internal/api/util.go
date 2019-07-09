package api

func QuirksIsNotReproducible(b LanguageBackend) bool {
	return (b.Quirks & QuirksNotReproducible) != 0
}

func QuirksIsReproducible(b LanguageBackend) bool {
	return (b.Quirks & QuirksNotReproducible) == 0
}

func QuirksDoesAddRemoveAlsoInstall(b LanguageBackend) bool {
	return (b.Quirks & QuirksAddRemoveAlsoInstalls) != 0
}

func QuirksDoesAddRemoveNotAlsoInstall(b LanguageBackend) bool {
	return (b.Quirks & QuirksAddRemoveAlsoInstalls) == 0
}

func QuirksDoesLockAlsoInstall(b LanguageBackend) bool {
	return (b.Quirks & QuirksLockAlsoInstalls) != 0
}

func QuirksDoesLockNotAlsoInstall(b LanguageBackend) bool {
	return (b.Quirks & QuirksLockAlsoInstalls) == 0
}
