package main

import (
	"encoding/json"
	"fmt"
)

type PackageCache = map[string]PackageInfo

// PackageInfo is very similar between BigQuery and PyPi,
// save for "downloads" which is an int in one, and an object in another.
type BqPackageInfo struct {
	Name         string   `json:"name,omitempty"`
	Downloads    int      `json:"downloads,string,omitempty"`
	Version      string   `json:"version,omitempty"`
	RequiresDist []string `json:"requires_dist,omitempty"`
}

type DownloadsInfo struct {
	LastDay   int `json:"last_day"`
	LastWeek  int `json:"last_week"`
	LastMonth int `json:"last_month"`
}

type PackageInfo struct {
	Name         string        `json:"name,omitempty"`
	Downloads    DownloadsInfo `json:"downloads,omitempty"`
	Version      string        `json:"version,omitempty"`
	RequiresDist []string      `json:"requires_dist,omitempty"`

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
	Info     PackageInfo `json:"info"`
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
	jsonError := map[string]interface{}{
		"type":    e.Class,
		"message": message,
	}
	if e.WrappedError != nil {
		jsonError["wrapped"] = e.WrappedError.Error()
	}
	encodedError, err := json.Marshal(jsonError)
	if err != nil {
		return fmt.Sprintf("{\"error\": \"%v\"}", err)
	}
	return string(encodedError)
}
