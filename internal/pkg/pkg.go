// Package pkg defines methods and heuristics for sanitizing package output.
package pkg

import (
	"github.com/replit/upm/internal/api"
)

func makeLoweredHM(normalizePackageName func(api.PkgName) api.PkgName, names []string) map[api.PkgName]bool {
	// Build a hashset. struct{}{} purportedly is of size 0, so this is as good as we get
	set := make(map[api.PkgName]bool)
	for _, pkg := range names {
		normal := normalizePackageName(api.PkgName(pkg))
		set[normal] = true
	}
	return set
}

func SortPrefixSuffix(normalizePackageName func(api.PkgName) api.PkgName, query string, ignoredPackages []string, packages []api.PkgInfo) []api.PkgInfo {
	ignoredPackageSet := makeLoweredHM(normalizePackageName, ignoredPackages)

	needle := normalizePackageName(api.PkgName(query))
	var exact *api.PkgInfo
	prefixed := []api.PkgInfo{}
	infixed := []api.PkgInfo{}
	filtered := []api.PkgInfo{}

	// Reorder results based on some common heuristics
	for idx, pkg := range packages {
		lower := normalizePackageName(api.PkgName(pkg.Name))
		if lower == needle {
			exact = &packages[idx]
		} else if ignoredPackageSet[lower] {
			continue
		} else if lower.HasPrefix(needle) {
			prefixed = append(prefixed, pkg)
		} else if lower.Contains(needle) {
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
