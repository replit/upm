package main

import "fmt"

type PackageCache = map[string]PackageInfo

type PackageInfo struct {
	Name      string `json:"name"`
	Downloads int    `json:"downloads,string"`
	Version   string

	// Specific to the dist we use to get modules from
	Modules []string
	MD5     string `json:"md5_digest"`
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
)

func (e PypiError) Error() string {
	message := [...]string{"No top level module",
		"Unknown archive type:" + e.Info,
		"Unknown distribution type: " + e.Info,
		"No distributions for latest version: " + e.Info,
	}[e.Class]
	return fmt.Sprintf("{\"type\": %v, \"message\": \"%v\"}", e.Class, message)
}
