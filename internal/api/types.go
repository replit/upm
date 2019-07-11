package api

import "regexp"

type PkgName string
type PkgSpec string
type PkgVersion string

// Note: this struct is parsed with reflection. It must have json and
// pretty tags, and the only allowed types are string and []string.
// Any instance of this struct *must* have a non-empty Name.
type PkgInfo struct {
	Name             string   `json:"name,omitempty" pretty:"Name"`
	Description      string   `json:"description,omitempty" pretty:"Description"`
	Version          string   `json:"version,omitempty" pretty:"Version"`
	HomepageURL      string   `json:"homepageURL,omitempty" pretty:"Homepage"`
	DocumentationURL string   `json:"documentationURL,omitempty" pretty:"Documentation"`
	SourceCodeURL    string   `json:"sourceCodeURL,omitempty" pretty:"Source code"`
	BugTrackerURL    string   `json:"bugTrackerURL,omitempty" pretty:"Bug tracker"`
	Author           string   `json:"author,omitempty" pretty:"Author"`
	License          string   `json:"license,omitempty" pretty:"License"`
	Dependencies     []string `json:"dependencies,omitempty" pretty:"Dependencies"`
}

type Quirks uint8

const (
	QuirksNone            Quirks = 0
	QuirksNotReproducible        = 1 << iota
	QuirksAddRemoveAlsoInstalls
	QuirksLockAlsoInstalls
)

// Keep up to date with checkBackends in backends.go
type LanguageBackend struct {
	Name             string
	Specfile         string
	Lockfile         string
	FilenamePatterns []string
	Quirks           Quirks
	Search           func(string) []PkgInfo
	Info             func(PkgName) PkgInfo
	Add              func(map[PkgName]PkgSpec)
	Remove           func(map[PkgName]bool)
	Lock             func()
	Install          func()
	ListSpecfile     func() map[PkgName]PkgSpec
	ListLockfile     func() map[PkgName]PkgVersion
	GuessRegexps     []*regexp.Regexp
	Guess            func() map[PkgName]bool
}
