package main

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
