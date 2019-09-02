// Package dart provides backends for Dart (https://dart.dev)
// using the Dart package manager Pub.dev (https://pub.dev)
package dart

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"net/url"
	"os"
	"path"
	"runtime"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
	"gopkg.in/yaml.v2"
)

// TODO: Properly implement package dir
// Blocked on https://github.com/dart-lang/pub/issues/2009
// Workaround inspired by https://github.com/google/pub_cache/blob/master/lib/pub_cache.dart#L17
func dartGetPackageDir() string {
	cacheEnv := os.Getenv("PUB_CACHE")
	if cacheEnv != "" {
		return cacheEnv
	}

	if runtime.GOOS == "windows" {
		return path.Join(os.Getenv("APPDATA"), "Pub", "Cache")
	}

	return path.Join(os.Getenv("HOME"), ".pub-cache")
}

// PubspecYaml represents deps in a pubspec.yaml file.
type dartPubspecYaml struct {
	Name            string                 `yaml:"name"`
	Description     string                 `yaml:"description"`
	Dependencies    map[string]interface{} `yaml:"dependencies"`
	DevDependencies map[string]interface{} `yaml:"dev_dependencies"`
}

// dartListPubspecYaml lists all deps in a pubspec.yaml file
func dartListPubspecYaml() map[api.PkgName]api.PkgSpec {

	var specs dartPubspecYaml
	specs = readSpecFile()

	pkgs := map[api.PkgName]api.PkgSpec{}
	for nameStr, specStr := range specs.Dependencies {
		pkgs[api.PkgName(nameStr)] = api.PkgSpec(dartParseSpec(specStr))
	}
	for nameStr, specStr := range specs.DevDependencies {
		pkgs[api.PkgName(nameStr)] = api.PkgSpec(dartParseSpec(specStr))
	}
	return pkgs
}

// ParseDartSpec parses a Dart version handling special
// cases like Flutter SDK deps where the spec is a map.
func dartParseSpec(spec interface{}) string {
	version, ok := spec.(string)
	// Handle cases like Flutter SDK deps where the spec is a map.
	if !ok {
		version = "Unknown"
	}
	return version
}

// PubspecLock represents deps in a pubspec.lock file.
type dartPubspecLock struct {
	Packages map[string]struct {
		Dependency string `yaml:"dependency"`
		Source     string `yaml:"source"`
		Version    string `yaml:"version"`
	} `yaml:"packages"`
}

// dartListPubspecLock lists all deps in a pubspec.lock file
func dartListPubspecLock() map[api.PkgName]api.PkgVersion {
	contentsB, err := ioutil.ReadFile("pubspec.lock")
	if err != nil {
		util.Die("pubspec.lock: %s", err)
	}
	var cfg dartPubspecLock
	if err := yaml.Unmarshal(contentsB, &cfg); err != nil {
		util.Die("pubspec.lock: %s", err)
	}
	pkgs := map[api.PkgName]api.PkgVersion{}
	for nameStr, data := range cfg.Packages {
		pkgs[api.PkgName(nameStr)] = api.PkgVersion(data.Version)
	}
	return pkgs
}

// pubDevSearchResults represents the data we get from Pub.dev when
// calling /api/search.
type pubDevSearchResults struct {
	Packages []struct {
		Name string `json:"package"`
	} `json:"packages"`
	NextURL string `json:"next"`
}

// dartSearch implements Search for Pub.dev.
func dartSearch(query string) []api.PkgInfo {
	endpoint := "https://pub.dev/api/search"
	queryParams := "?q=" + url.QueryEscape(query)

	resp, err := http.Get(endpoint + queryParams)
	if err != nil {
		util.Die("Pub.dev: %s", err)
	}
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		util.Die("Pub.dev: %s", err)
	}

	var pubDevResults pubDevSearchResults
	if err := json.Unmarshal(body, &pubDevResults); err != nil {
		util.Die("Pub.dev: %s", err)
	}

	results := make([]api.PkgInfo, len(pubDevResults.Packages))
	for i := range pubDevResults.Packages {
		p := pubDevResults.Packages[i]
		results[i] = api.PkgInfo{
			Name: p.Name,
		}
	}
	return results
}

// pubDevInfoResults represents the data we get from Pub.dev when
// calling /api/packages/[package identifier].
type pubDevInfoResults struct {
	Name   string `json:"name"`
	Latest struct {
		ArchiveURL string `json:"archive_url"`
		Pubspec    struct {
			Version     string `json:"version"`
			Author      string `json:"author"`
			Description string `json:"description"`
			Homepage    string `json:"homepage"`
		} `json:"pubspec"`
	} `json:"latest"`
	Version string `json:"version"`
}

// dartInfo implements Info for Pub.dev.
func dartInfo(name api.PkgName) api.PkgInfo {
	endpoint := fmt.Sprintf("%s/%s", "https://pub.dev/api/packages", name)

	resp, err := http.Get(endpoint)
	if err != nil {
		util.Die("Pub.dev: %s", err)
	}
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		util.Die("Pub.dev: %s", err)
	}

	var pubDevResults pubDevInfoResults
	if err := json.Unmarshal(body, &pubDevResults); err != nil {
		util.Die("Pub.dev: %s", err)
	}

	return api.PkgInfo{
		Name:          pubDevResults.Name,
		Description:   pubDevResults.Latest.Pubspec.Description,
		Version:       pubDevResults.Version,
		HomepageURL:   pubDevResults.Latest.Pubspec.Homepage,
		SourceCodeURL: "",
		BugTrackerURL: "",
		Author: util.AuthorInfo{
			Name:  pubDevResults.Latest.Pubspec.Author,
			Email: "",
			URL:   "",
		}.String(),
		License: ""}
}

func createSpecFile() {
	if util.Exists("pubspec.yaml") {
		util.Die("pubspec.yaml already exists")
	}

	pubspec := dartPubspecYaml{
		Name: "MyApp",
	}
	writeSpecFile(pubspec)
}

func writeSpecFile(specs dartPubspecYaml) {
	var data []byte
	var err error
	data, err = yaml.Marshal(&specs)
	if err != nil {
		fmt.Println("Marshal Error")
	}

	ioutil.WriteFile("pubspec.yaml", data, 0666)
}

func readSpecFile() dartPubspecYaml {
	contentsB, err := ioutil.ReadFile("pubspec.yaml")
	if err != nil {
		util.Die("pubspec.yaml: %s", err)
	}

	var specs dartPubspecYaml
	if err := yaml.Unmarshal(contentsB, &specs); err != nil {
		util.Die("pubspec.yaml: %s", err)
	}

	return specs
}

func dartAdd(pkgs map[api.PkgName]api.PkgSpec) {
	if !util.Exists("pubspec.yaml") {
		createSpecFile()
	}

	var specs dartPubspecYaml
	specs = readSpecFile()

	for name, spec := range pkgs {
		arg := string(name)
		if spec != "" {
			specs.Dependencies[arg] = string(spec)
		} else {
			specs.Dependencies[arg] = nil
		}
	}

	writeSpecFile(specs)
}

func dartRemove(pkgs map[api.PkgName]bool) {
	var specs dartPubspecYaml
	specs = readSpecFile()

	for name := range pkgs {
		delete(specs.Dependencies, string(name))
	}

	writeSpecFile(specs)
}

// dartGuess stub.
func dartGuess() (map[api.PkgName]bool, bool) {
	pkgs := map[api.PkgName]bool{}

	return pkgs, true
}

// DartPubBackend is a UPM backend for Dart that uses Pub.dev.
var DartPubBackend = api.LanguageBackend{
	Name:             "dart-pub",
	Specfile:         "pubspec.yaml",
	Lockfile:         "pubspec.lock",
	FilenamePatterns: []string{"*.dart"},
	Quirks:           api.QuirksNone,
	GetPackageDir:    dartGetPackageDir,
	Search:           dartSearch,
	Info:             dartInfo,
	Add:              dartAdd,
	Remove:           dartRemove,
	Lock: func() {
		util.RunCmd([]string{"pub", "get"})
	},
	Install: func() {
		util.RunCmd([]string{"pub", "get"})
	},
	ListSpecfile: dartListPubspecYaml,
	ListLockfile: dartListPubspecLock,
	GuessRegexps: nil,
	Guess:        dartGuess,
}
