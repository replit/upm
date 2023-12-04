// Package pkg defines methods and heuristics for sanitizing package output.
package pkg

import (
	"strings"

	"github.com/replit/upm/internal/api"
)

func SortPackages(query string, packages []api.PkgInfo) []api.PkgInfo {

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
