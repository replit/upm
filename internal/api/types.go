package api

import (
	"regexp"

	"github.com/replit/upm/internal/util"
)

// PkgName represents the name of a package, e.g. "flask" for Python.
type PkgName string

// PkgSpec represents a package version constraint, e.g. "^1.1" or ">=
// 1.1, <2.0" for Python.
type PkgSpec string

// PkgVersion represents an exact package version, e.g. "1.1" or
// "1.0b2.post345.dev456" for Python.
type PkgVersion string

// PkgInfo is a general-purpose struct for representing package
// metadata. Any of the fields may be zeroed except for Name. Which
// fields are nonzero depends on the context and language backend.
//
// Note: the PkgInfo struct is parsed with reflection in several
// places. It must have "json" and "pretty" tags, and the only allowed
// types are string and []string.
type PkgInfo struct {

	// The name of the package, e.g. "flask". Package names cannot
	// contain spaces.
	Name string `json:"name,omitempty" pretty:"Name"`

	// Brief description of the package, e.g. "A simple framework
	// for building complex web applications.".
	Description string `json:"description,omitempty" pretty:"Description"`

	// Version number of the package, e.g. "1.1.1". No particular
	// format is enforced.
	Version string `json:"version,omitempty" pretty:"Version"`

	// URL for the package's home page, e.g.
	// "https://palletsprojects.com/p/flask/".
	HomepageURL string `json:"homepageURL,omitempty" pretty:"Homepage"`

	// URL for the package's documentation, e.g.
	// "https://flask.palletsprojects.com/".
	DocumentationURL string `json:"documentationURL,omitempty" pretty:"Documentation"`

	// URL for the package's source code, e.g.
	// "https://github.com/pallets/flask".
	SourceCodeURL string `json:"sourceCodeURL,omitempty" pretty:"Source code"`

	// URL for the package's bug tracker, e.g.
	// "https://github.com/pallets/flask/issues".
	BugTrackerURL string `json:"bugTrackerURL,omitempty" pretty:"Bug tracker"`

	// Author of the package. Only one author is supported; if
	// there are multiple, we either pick one or simply
	// concatenate them into a single Author.
	Author string `json:"author,omitempty" pretty:"Author"`

	// License of the package. No particular format is enforced.
	// If the package has multiple licenses, we just concatenate
	// them into one string.
	License string `json:"license,omitempty" pretty:"License"`

	// Names of packages which are dependencies of this package.
	// There is no way to distinguish between a package that has
	// no dependencies and a package whose language backend did
	// not provide dependency information.
	Dependencies []string `json:"dependencies,omitempty" pretty:"Dependencies"`
}

// Quirks is a bitmask enum used to indicate how specific language
// backends behave differently from the core abstractions of UPM, and
// therefore require some different treatment by the command-line
// interface layer. See the constants of this type for more
// information.
type Quirks uint8

// Constants of type Quirks, used to denote whether a language backend
// follows the expected abstractions of UPM or if it needs special
// treatment.
const (
	// By default, UPM assumes that each language backend
	// implements the add/remove, lock, and install operations as
	// totally separate steps: add/remove only modifies the
	// specfile, lock only updates the lockfile from the specfile,
	// and install only installs packages from the lockfile.
	QuirksNone Quirks = 0

	// This constant indicates that the package manager doesn't
	// have any concept of a lockfile, so the backend implements
	// its own. In this case, the functioning of add/remove is
	// unchanged. However, lock does nothing (and the backend must
	// not implement it), while install installs packages directly
	// from the specfile and then generates a lockfile from what
	// was installed.
	QuirksNotReproducible = 1 << iota

	// This constant indicates that add/remove also executes lock
	// subsequently, so it doesn't need to be run afterwards.
	QuirksAddRemoveAlsoLocks

	// This constant indicates that add/remove also executes lock
	// and install subsequently, so they don't need to be run
	// afterwards. If specified, then QuirksAddRemoveAlsoLocks
	// also must be specified.
	QuirksAddRemoveAlsoInstalls

	// This constant indicates that lock also executes install
	// subsequently, so it doesn't need to be run afterwards.
	QuirksLockAlsoInstalls
)

// LanguageBackend is the core abstraction of UPM. It represents an
// implementation of all the core package management functionality of
// UPM, for a specific programming language and package manager. For
// example, python-python3-poetry and python-python2-poetry would be
// different backends, as would python-python3-poetry and
// python-python3-pipenv.
//
// Most of the fields of this struct are mandatory, and the Check
// method will panic at UPM startup if they are not provided. Not all
// language backends necessarily need to implement all operations; in
// this case, the relevant functions should call util.NotImplemented,
// which will cause UPM to exit with an appropriate error message.
// (The limitation should be noted in the backend feature matrix in
// the README.)
//
// Make sure to update the Check method when adding/removing fields
// from this struct.
type LanguageBackend struct {

	// The name of the language backend. This is matched against
	// the value of the --lang argument on the command line. The
	// format is a sequence of one or more tokens separated by
	// hyphens, as in ruby-bundler or python-python3-poetry. Each
	// token gets matched separately against the --lang argument,
	// so that python-python3-poetry will match against python or
	// python3 or poetry or python-poetry. The name must be unique
	// among all language backends.
	//
	// This field is mandatory.
	Name string

	// The filename of the specfile, e.g. "pyproject.toml" for
	// Poetry.
	//
	// This field is mandatory.
	Specfile string

	// The filename of the lockfile, e.g. "poetry.lock" for
	// Poetry.
	//
	// This field is mandatory.
	Lockfile string

	// List of filename globs that match against files written in
	// this programming language, e.g. "*.py" for Python. These
	// should not include any slashes, because they may be matched
	// in any subdirectory.
	//
	// FilenamePatterns is used for two things: language backend
	// autodetection (if matching files exist in the project
	// directory, the project is autodetected for this backend,
	// subject to being overridden by another heuristic), and
	// regexp searches for dependency guessing.
	//
	// This field is mandatory.
	FilenamePatterns []string

	// QuirksNone if the language backend conforms to the core
	// abstractions of UPM, and some bitwise disjunction of the
	// Quirks constant values otherwise.
	//
	// This field is optional, and defaults to QuirksNone.
	Quirks Quirks

	// Function that normalizes a package name. This is used to
	// prevent duplicate packages getting added to the specfile.
	// For example, in Python the package names "flask" and
	// "Flask" are the same, so all package names are lowercased
	// before comparison.
	//
	// This field is optional.
	NormalizePackageName func(name PkgName) PkgName

	// Return the path (relative to the project directory) in
	// which packages are installed. The path need not exist.
	GetPackageDir func() string

	// Search for packages using an online index. The query may
	// contain any characters, including whitespace. Return a list
	// of search results, which can be of any length. (It will be
	// truncated by the command-line interface.) If the search
	// fails, terminate the process. If it successfully returns no
	// results, return an empty slice.
	//
	// This field is mandatory.
	Search func(query string) []PkgInfo

	// Retrieve information about a package from an online index.
	// If the package doesn't exist, return a zero struct.
	//
	// This field is mandatory.
	Info func(PkgName) PkgInfo

	// Add packages to the specfile. The map is guaranteed to have
	// at least one package, and all of the packages are
	// guaranteed to not already be in the specfile (according to
	// ListSpecfile). The specfile is *not* guaranteed to exist
	// already. The specs may be empty, in which case default
	// specs should be generated (for example, specifying the
	// latest version or newer). This method must create the
	// specfile if it does not exist already.
	//
	// If QuirksAddRemoveAlsoInstalls, then also lock and install.
	// In this case this method must also create the lockfile if
	// it does not exist already.
	//
	// This field is mandatory.
	Add func(map[PkgName]PkgSpec)

	// Remove packages from the specfile. The map is guaranteed to
	// have at least one package, and all of the packages are
	// guaranteed to already be in the specfile (according to
	// ListSpecfile). The specfile is guaranteed to exist already.
	// This method may not delete the specfile or lockfile.
	//
	// If QuirksAddRemoveAlsoInstalls, then also lock and install.
	// In this case this method must also create the lockfile if
	// it does not exist already.
	//
	// This field is mandatory.
	Remove func(map[PkgName]bool)

	// Generate the lockfile from the specfile. The specfile is
	// guaranteed to already exist. This method must create the
	// lockfile if it does not exist already.
	//
	// If QuirksLockAlsoInstalls, then also install.
	//
	// This field is mandatory, unless QuirksNotReproducible in
	// which case this field *may* not be specified.
	Lock func()

	// Install packages from the lockfile. The specfile and
	// lockfile are guaranteed to already exist, unless
	// QuirksNotReproducible in which case only the specfile is
	// guaranteed to exist.
	//
	// This field is mandatory.
	Install func()

	// List the packages in the specfile. Names and specs should
	// be returned in a format suitable for the Add method. The
	// specfile is guaranteed to exist already.
	//
	// This field is mandatory.
	ListSpecfile func() map[PkgName]PkgSpec

	// List the packages in the lockfile. Names should be returned
	// in a format suitable for the Add method. The lockfile is
	// guaranteed to exist already.
	//
	// This field is mandatory.
	ListLockfile func() map[PkgName]PkgVersion

	// Regexps used to determine if the Guess method really needs
	// to be invoked, or if its previous return value can be
	// re-used.
	//
	// These regexps should match imports, requires, or whatever
	// is analogous for the language. They are run against every
	// file in the project directory and its subdirectories,
	// subject to FilenamePatterns and util.IgnoredPaths. If a
	// regexp has no capture groups, the entire match is used;
	// otherwise, the match of each of its capture groups is used.
	// The list of all matches from all regexps against all files
	// is aggregated and hashed, and this is used to determine if
	// Guess needs to be re-run. So, these regexps should be
	// written so that what they match will change whenever the
	// return value of Guess might change.
	//
	// This field is optional; if it is omitted, then Guess will
	// always be run without recourse to caching.
	GuessRegexps []*regexp.Regexp

	// Return a list of packages that are probably needed as
	// dependencies of the project. It is better to be safe than
	// sorry: only packages which are *definitely* project
	// dependencies should be returned. Names should be returned
	// in a format suitable for the Add method. There is no need
	// to eliminate packages already installed.
	//
	// The second value indicates whether the bare imports search
	// was fully successful. One reason why it might not be
	// successful is if there is a syntax error in the code. It is
	// important to get this right because a syntax error can
	// cause an entire file to be skipped. Then if the error is
	// fixed later, the GuessRegexps may return the same results,
	// causing UPM to re-use the existing Guess return value
	// (which is now wrong).
	//
	// This field is mandatory.
	Guess func() (map[PkgName]bool, bool)
}

// Setup panics if the given language backend does not specify all of
// the mandatory fields. It also assigns some defaults.
//
// Honestly, this is a bit of a hack. We should really not expose the
// struct fields through the API directly, or at least we should have
// a builder function which can perform this normalization and
// validation.
func (b *LanguageBackend) Setup() {
	condition2flag := map[string]bool{
		"missing name":                     b.Name == "",
		"missing specfile":                 b.Specfile == "",
		"missing lockfile":                 b.Lockfile == "",
		"need at least 1 filename pattern": len(b.FilenamePatterns) == 0,
		"missing package dir":              b.GetPackageDir == nil,
		"missing Search":                   b.Search == nil,
		"missing Info":                     b.Info == nil,
		"missing Add":                      b.Add == nil,
		"missing Remove":                   b.Remove == nil,
		// The lock method should be unimplemented if
		// and only if builds are not reproducible.
		"either implement Lock or mark QuirksIsNotReproducible": ((b.Lock == nil) != b.QuirksIsNotReproducible()),
		"missing install":      b.Install == nil,
		"missing ListSpecfile": b.ListSpecfile == nil,
		"missing ListLockfile": b.ListLockfile == nil,
		// If the backend isn't reproducible, then lock is
		// unimplemented. So how could it also do
		// installation?
		"Lock installs, but is not implemented": b.QuirksDoesLockAlsoInstall() && b.QuirksIsNotReproducible(),
		// If you install, then you have to lock.
		"Add and Remove install, so they must also Lock": b.QuirksDoesAddRemoveAlsoInstall() && !b.QuirksDoesAddRemoveAlsoLock(),
	}

	reasons := []string{}
	for reason, flag := range condition2flag {
		if flag {
			reasons = append(reasons, reason)
		}
	}

	if len(reasons) > 0 {
		util.Panicf("language backend %s is incomplete or invalid: %s", b.Name, reasons)
	}

	if b.NormalizePackageName == nil {
		b.NormalizePackageName = func(name PkgName) PkgName {
			return name
		}
	}
}
