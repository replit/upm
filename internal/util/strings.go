package util

import (
	"fmt"
	"regexp"
	"strings"
)

// AuthorInfo represents the author of a package. (Packages may only
// have one author in our world.)
type AuthorInfo struct {

	// Probably "Firstname Lastname", although no given format is
	// actually enforced.
	Name string

	// Probably "something@example.com", although no given format
	// is actually enforced.
	Email string

	// Home page or personal website. Probably
	// "https://example.com/somewhere", although no given format
	// is actually enforced.
	URL string
}

// String returns a string representation of an author. For example,
// "Radon Rosborough <radon.neon@gmail.com>".
func (a AuthorInfo) String() string {
	parts := []string{}
	if a.Name != "" {
		parts = append(parts, a.Name)
	}
	if a.Email != "" || a.URL != "" {
		subparts := []string{}
		if a.Email != "" {
			subparts = append(subparts, a.Email)
		}
		if a.URL != "" {
			subparts = append(subparts, a.URL)
		}
		parts = append(parts,
			fmt.Sprintf("<%s>", strings.Join(subparts, ", ")),
		)
	}
	return strings.Join(parts, " ")
}

// Regexps compiles each provided pattern into a regexp object, and
// returns a slice of them.
func Regexps(patterns []string) []*regexp.Regexp {
	regexps := []*regexp.Regexp{}
	for _, pattern := range patterns {
		regexps = append(regexps, regexp.MustCompile(pattern))
	}
	return regexps
}
