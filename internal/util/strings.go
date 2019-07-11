package util

import (
	"fmt"
	"regexp"
	"strings"
)

type AuthorInfo struct {
	Name  string
	Email string
	URL   string
}

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

func Regexps(patterns []string) []*regexp.Regexp {
	regexps := []*regexp.Regexp{}
	for _, pattern := range patterns {
		regexps = append(regexps, regexp.MustCompile(pattern))
	}
	return regexps
}
