package internal

// Shared between cli and cmds

type outputFormat int

const (
	outputFormatTable outputFormat = iota
	outputFormatJSON
)

// Shared between cmds and store

type hash string

type store struct {
	SpecfileHash hash
	LockfileHash hash
}

// Shared between cmds and backends

type pkgName string
type pkgSpec string
type pkgVersion string

type pkgInfo struct {
	name             string
	description      string
	version          string
	homepageUrl      string
	documentationUrl string
	sourceCodeUrl    string
	bugTrackerUrl    string
	author           string
	license          string
	dependencies     []string
}

type quirks uint8

const (
	quirksNone            quirks = 0
	quirksNotReproducible        = 1 << iota
)

// Keep up to date with checkBackends in backends.go
type languageBackend struct {
	name         string
	specfile     string
	lockfile     string
	quirks       quirks
	detect       func() bool
	search       func([]string) []pkgInfo
	info         func(pkgName) *pkgInfo
	add          func(map[pkgName]pkgSpec)
	remove       func(map[pkgName]bool)
	lock         func()
	install      func()
	listSpecfile func() map[pkgName]pkgSpec
	listLockfile func() map[pkgName]pkgVersion
	guess        func() map[pkgName]bool
}
