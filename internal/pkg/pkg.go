// Package pkg defines methods and heuristics for sanitizing package output.
package pkg

import (
	"github.com/replit/upm/internal/api"
)

func NormalizePackageNameNoop(name api.PkgName) api.PkgName {
	return name
}

func SortNoop(normalizePackageName func(api.PkgName) api.PkgName) func(query string, packages []api.PkgInfo) []api.PkgInfo {
	return func(query string, packages []api.PkgInfo) []api.PkgInfo {
		return sortNoop(normalizePackageName, query, packages)
	}
}

func sortNoop(normalizePackageName func(api.PkgName) api.PkgName, query string, packages []api.PkgInfo) []api.PkgInfo {
	return packages
}

func SortPrefixSuffix(normalizePackageName func(api.PkgName) api.PkgName) func(query string, packages []api.PkgInfo) []api.PkgInfo {
	return func(query string, packages []api.PkgInfo) []api.PkgInfo {
		return sortPrefixSuffix(normalizePackageName, query, packages)
	}
}

func sortPrefixSuffix(normalizePackageName func(api.PkgName) api.PkgName, query string, packages []api.PkgInfo) []api.PkgInfo {
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
