package store

// hash is used in the store to represent a serializable MD5 hash.
type hash string

type storeLanguage struct {

	// The hash of the specfile, or an empty string to indicate
	// that the file doesn't exist or the hash has never been
	// computed.
	SpecfileHash hash `json:"specfileHash,omitempty"`

	// The hash of the lockfile, or an empty string to indicate
	// that the file doesn't exist or the hash has never been
	// computed.
	LockfileHash hash `json:"lockfileHash,omitempty"`

	// The last return value of b.Guess(), converted to a slice.
	// This is only set if the language backend provides
	// GuessRegexps.
	GuessedImports []string `json:"guessedImports,omitempty"`

	// The hash of the last sequence of matches for GuessRegexps
	// against the project code.
	GuessedImportsHash hash `json:"guessedImportsHash,omitempty"`
}

// store represents the JSON written (by default) to .upm/store.json.
type store struct {

	// The version of the store file. This gets incremented every
	// time we make a backwards-incompatible change, and causes
	// the store to be invalidated.
	Version int `json:"version,omitempty"`

	// Map from backend names to per-backend data.
	Languages map[string]*storeLanguage `json:"languages,omitempty"`
}
