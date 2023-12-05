// Package pkg defines methods and heuristics for sanitizing package output.
package pkg

import (
	"strings"

	"github.com/replit/upm/internal/api"
)

func makeLoweredHM(names []string) map[string]bool {
	// Build a hashset. struct{}{} purportedly is of size 0, so this is as good as we get
	set := make(map[string]bool)
	for _, pkg := range names {
		set[strings.ToLower(pkg)] = true
	}
	return set
}

func SortPackages(query string, ignoredPackages []string, packages []api.PkgInfo) []api.PkgInfo {

	ignoredPackageSet := makeLoweredHM(ignoredPackages)

	needle := strings.ToLower(query)
	var exact *api.PkgInfo
	prefixed := []api.PkgInfo{}
	infixed := []api.PkgInfo{}
	filtered := []api.PkgInfo{}

	// Reorder results based on some common heuristics
	for idx, pkg := range packages {
		lower := strings.ToLower(pkg.Name)
		if lower == needle {
			exact = &packages[idx]
		} else if ignoredPackageSet[lower] {
			continue
		} else if strings.HasPrefix(lower, needle) {
			prefixed = append(prefixed, pkg)
		} else if strings.Contains(lower, needle) {
			infixed = append(infixed, pkg)
		} else {
			filtered = append(filtered, pkg)
		}
	}

	curated := []api.PkgInfo{}
	if exact != nil {
		curated = append(curated, *exact)
	}
	curated = append(curated, prefixed...)
	curated = append(curated, infixed...)
	curated = append(curated, filtered...)

	return curated
}
