package api

import (
	"regexp"

	"github.com/replit/upm/internal/util"
)

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

func (b *LanguageBackend) Check() {
	if b.Name == "" ||
		b.Specfile == "" ||
		b.Lockfile == "" ||
		b.Search == nil ||
		b.Info == nil ||
		b.Add == nil ||
		b.Remove == nil ||
		// The lock method should be unimplemented if
		// and only if builds are not reproducible.
		((b.Lock == nil) != b.QuirksIsNotReproducible()) ||
		b.Install == nil ||
		b.ListSpecfile == nil ||
		b.ListLockfile == nil ||
		b.Guess == nil {
		util.Panicf("language backend %s is incomplete", b.Name)
	}
}
