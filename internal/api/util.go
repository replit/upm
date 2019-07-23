package api

// QuirksIsNotReproducible returns true if the language backend
// specifies QuirksNotReproducible, i.e. the package manager doesn't
// support a lockfile and one must be generated after install.
func (b *LanguageBackend) QuirksIsNotReproducible() bool {
	return (b.Quirks & QuirksNotReproducible) != 0
}

// QuirksIsNotReproducible returns true if the language backend does
// *not* specify QuirksNotReproducible, i.e. the package manager
// supports a lockfile, as expected.
func (b *LanguageBackend) QuirksIsReproducible() bool {
	return (b.Quirks & QuirksNotReproducible) == 0
}

// QuirksDoesAddRemoveAlsoInstall returns true if the language backend
// specifies QuirksAddRemoveAlsoInstalls, i.e. it also runs lock and
// install when running add or remove.
func (b *LanguageBackend) QuirksDoesAddRemoveAlsoInstall() bool {
	return (b.Quirks & QuirksAddRemoveAlsoInstalls) != 0
}

// QuirksDoesAddRemoveNotAlsoInstall returns true if the language
// backend does *not* specify QuirksAddRemoveAlsoInstalls, i.e.
// running add or remove does *not* also run install (though it might
// run lock; see QuirksDoesAddRemoveAlsoLock).
func (b *LanguageBackend) QuirksDoesAddRemoveNotAlsoInstall() bool {
	return (b.Quirks & QuirksAddRemoveAlsoInstalls) == 0
}

// QuirksDoesAddRemoveAlsoLock returns true if the language backend
// specifies QuirksAddRemoveAlsoLocks, i.e. it also runs lock (but not
// install) when running add or remove.
func (b *LanguageBackend) QuirksDoesAddRemoveAlsoLock() bool {
	return (b.Quirks & QuirksAddRemoveAlsoLocks) != 0
}

// QuirksDoesAddRemoveNotAlsoLock returns true if the language backend
// does *not* specify QuirksAddRemoveAlsoLocks, i.e. running add or
// remove does *not* also run lock or install.
func (b *LanguageBackend) QuirksDoesAddRemoveNotAlsoLock() bool {
	return (b.Quirks & QuirksAddRemoveAlsoLocks) == 0
}

// QuirksDoesLockAlsoInstall returns true if the language backend
// specifies QuirksLockAlsoInstalls, i.e. running lock also runs
// install.
func (b *LanguageBackend) QuirksDoesLockAlsoInstall() bool {
	return (b.Quirks & QuirksLockAlsoInstalls) != 0
}

// QuirksDoesLockNotAlsoInstall returns true if the language backend
// does *not* specify QuirksLockAlsoInstalls, i.e. running lock does
// *not* also run install.
func (b *LanguageBackend) QuirksDoesLockNotAlsoInstall() bool {
	return (b.Quirks & QuirksLockAlsoInstalls) == 0
}
