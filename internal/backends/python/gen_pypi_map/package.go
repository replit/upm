package main

import (
	"regexp"
	"strings"
)

var normalizationPattern = regexp.MustCompile(`[-_.]+`)

// normalizePackageName implements NormalizePackageName for the Python
// backends.
// See https://packaging.python.org/en/latest/specifications/name-normalization/
func normalizePackageName(name string) string {
	name = strings.ToLower(name)
	name = normalizationPattern.ReplaceAllString(name, "-")
	return name
}
