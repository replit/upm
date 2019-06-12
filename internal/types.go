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
	name string
	description string
	version string
	homepageURL string
	documentationURL string
	sourceCodeURL string
	author string
	license string
	dependencies []string
}

// Keep up to date with checkBackends in backends.go
type languageBackend struct {
	name string
	specfile string
	lockfile string
	detect func () bool
	search func ([]string) []pkgInfo
	info func (pkgName) pkgInfo
	add func (map[pkgName]pkgSpec)
	remove func (map[pkgName]bool)
	lock func ()
	install func ()
	listSpecfile func () map[pkgName]pkgSpec
	listLockfile func () map[pkgName]pkgVersion
	guess func () map[pkgName]bool
}
