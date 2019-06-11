package internal

type pkgSpec struct {
	pkg string
	spec string
}

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

type languageBackend struct {
	name string
	specfile string
	lockfile string
	detect func () bool
	search func ([]string) []pkgInfo
	info func (string) pkgInfo
	add func ([]pkgSpec)
	remove func ([]string)
	lock func ()
	install func ()
	list func (bool) []pkgInfo
	guess func () []string
}
