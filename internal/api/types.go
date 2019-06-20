package api

type PkgName string
type PkgSpec string
type PkgVersion string

type PkgInfo struct {
	Name             string
	Description      string
	Version          string
	HomepageURL      string
	DocumentationURL string
	SourceCodeURL    string
	BugTrackerURL    string
	Author           string
	License          string
	Dependencies     []string
}

type Quirks uint8

const (
	QuirksNone            Quirks = 0
	QuirksNotReproducible        = 1 << iota
)

// Keep up to date with checkBackends in backends.go
type LanguageBackend struct {
	Name         string
	Specfile     string
	Lockfile     string
	Quirks       Quirks
	Detect       func() bool
	Search       func([]string) []PkgInfo
	Info         func(PkgName) *PkgInfo
	Add          func(map[PkgName]PkgSpec)
	Remove       func(map[PkgName]bool)
	Lock         func()
	Install      func()
	ListSpecfile func() map[PkgName]PkgSpec
	ListLockfile func() map[PkgName]PkgVersion
	Guess        func() map[PkgName]bool
}
