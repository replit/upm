// Package dart provides backends for Dart (https://dart.dev)
// using the Dart package manager Pub.dev (https://pub.dev)
package dart

import (
	"encoding/json"
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
	contentsB, err := ioutil.ReadFile("pubspec.yaml")
	if err != nil {
		util.Die("pubspec.yaml: %s", err)
	}

	var cfg dartPubspecYaml
	if err := yaml.Unmarshal(contentsB, &cfg); err != nil {
		util.Die("pubspec.yaml: %s", err)
	}

	pkgs := map[api.PkgName]api.PkgSpec{}
	for nameStr, specStr := range cfg.Dependencies {
		pkgs[api.PkgName(nameStr)] = api.PkgSpec(dartParseSpec(specStr))
	}
	for nameStr, specStr := range cfg.DevDependencies {
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
// doing a search.
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

// dartInfo is an empty stub implementation.
func dartInfo(name api.PkgName) api.PkgInfo {
	return api.PkgInfo{
		Name:          "",
		Description:   "",
		Version:       "",
		HomepageURL:   "",
		SourceCodeURL: "",
		BugTrackerURL: "",
		Author: util.AuthorInfo{
			Name:  "",
			Email: "",
			URL:   "",
		}.String(),
		License: ""}
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
	Add: func(pkgs map[api.PkgName]api.PkgSpec) {
		if !util.Exists("package.json") {
			util.RunCmd([]string{"npm", "init", "-y"})
		}
		cmd := []string{"npm", "install"}
		for name, spec := range pkgs {
			arg := string(name)
			if spec != "" {
				arg += "@" + string(spec)
			}
			cmd = append(cmd, arg)
		}
		util.RunCmd(cmd)
	},
	Remove: func(pkgs map[api.PkgName]bool) {
		cmd := []string{"npm", "uninstall"}
		for name, _ := range pkgs {
			cmd = append(cmd, string(name))
		}
		util.RunCmd(cmd)
	},
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

// dartGuess implements Guess for nodejs-yarn and nodejs-npm.
func dartGuess() (map[api.PkgName]bool, bool) {
	pkgs := map[api.PkgName]bool{}

	return pkgs, true
}
