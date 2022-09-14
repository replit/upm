package main

import "fmt"

type PackageCache = map[string]PackageInfo

type PackageInfo struct {
	Name         string   `json:"name,omitempty"`
	Downloads    int      `json:"downloads,string,omitempty"`
	Version      string   `json:"version,omitempty"`
	RequiresDist []string `json:"requires_dist,omitempty"`

	// Specific to the dist we use to get modules from
	Modules []string `json:"modules,omitempty"`
	Error   string   `json:"error,omitempty"`
}

type LegacyPackageInfo struct {
	Pkg       string   `json:"p"`
	Mods      []string `json:"m"`
	Downloads int      `json:"d"`
}

type PackageURL struct {
	Filename    string
	URL         string
	PackageType string
	UploadTime  string
	Size        int64
	MD5         string `json:"md5_digest"`
}

type PackageData struct {
	Info     PackageInfo
	Releases map[string][]PackageURL
}

type PypiErrorType int

type PypiError struct {
	Class        PypiErrorType
	Info         string
	WrappedError error
}

const (
	NoTopLevel PypiErrorType = iota
	UnknownArchive
	UnknownDist
	NoDistributions
	DownloadFailure
	InstallFailure
)

func (e PypiError) Error() string {
	message := [...]string{"No top level module",
		"Unknown archive type:" + e.Info,
		"Unknown distribution type: " + e.Info,
		"No distributions for latest version: " + e.Info,
		"Failed to download: " + e.Info,
		"Failed to install: " + e.Info,
	}[e.Class]
	return fmt.Sprintf("{\"type\": %v, \"message\": \"%v\"}", e.Class, message)
}
