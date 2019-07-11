package api

func (b *LanguageBackend) QuirksIsNotReproducible() bool {
	return (b.Quirks & QuirksNotReproducible) != 0
}

func (b *LanguageBackend) QuirksIsReproducible() bool {
	return (b.Quirks & QuirksNotReproducible) == 0
}

func (b *LanguageBackend) QuirksDoesAddRemoveAlsoInstall() bool {
	return (b.Quirks & QuirksAddRemoveAlsoInstalls) != 0
}

func (b *LanguageBackend) QuirksDoesAddRemoveNotAlsoInstall() bool {
	return (b.Quirks & QuirksAddRemoveAlsoInstalls) == 0
}

func (b *LanguageBackend) QuirksDoesLockAlsoInstall() bool {
	return (b.Quirks & QuirksLockAlsoInstalls) != 0
}

func (b *LanguageBackend) QuirksDoesLockNotAlsoInstall() bool {
	return (b.Quirks & QuirksLockAlsoInstalls) == 0
}
