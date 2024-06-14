// Package python provides backends for Python 2 and 3 using Poetry and pip.
package python

import (
	"encoding/json"
	"fmt"
	"io"
	"os"
	"regexp"
	"strings"

	"github.com/BurntSushi/toml"
	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

var normalizationPattern = regexp.MustCompile(`[-_.]+`)

// this generates a mapping of pypi packages <-> modules
// moduleToPypiPackage pypiPackageToModules are provided
// pypiEntryInfoResponse is a wrapper around pypiEntryInfo
// that matches the format of the REST API
type pypiEntryInfoResponse struct {
	Info pypiEntryInfo `json:"info"`
}

// pypiEntryInfo represents the response we get from the
// PyPI API on doing a single-package lookup.
type pypiEntryInfo struct {
	Author        string   `json:"author"`
	AuthorEmail   string   `json:"author_email"`
	HomePage      string   `json:"home_page"`
	License       string   `json:"license"`
	Name          string   `json:"name"`
	ProjectURL    string   `json:"project_url"`
	PackageURL    string   `json:"package_url"`
	BugTrackerURL string   `json:"bugtrack_url"`
	DocsURL       string   `json:"docs_url"`
	RequiresDist  []string `json:"requires_dist"`
	Summary       string   `json:"summary"`
	Version       string   `json:"version"`
}

type pyprojectPackageCfg struct {
	Include string `json:"include"`
	From    string `json:"from"`
}

type pyprojectTOMLGroup struct {
	Dependencies map[string]interface{} `json:"dependencies"`
}

// pyprojectTOML represents the relevant parts of a pyproject.toml
// file.
type pyprojectTOML struct {
	BuildSystem *struct {
		Requires     []string `json:"requires"`
		BuildBackend string   `json:"build-backend"`
	} `json:"build-system"`
	Project *struct {
		Dependencies []string `json:"dependencies"`
	} `json:"project"`
	Tool *struct {
		Poetry *struct {
			Name string `json:"name"`
			// interface{} because they can be either
			// strings or maps (why?? good lord).
			Dependencies    map[string]interface{}        `json:"dependencies"`
			DevDependencies map[string]interface{}        `json:"dev-dependencies"`
			Packages        []pyprojectPackageCfg         `json:"packages"`
			Group           map[string]pyprojectTOMLGroup `json:"group"`
		} `json:"poetry"`
	} `json:"tool"`
}

func getCommonPackageDir() string {
	// Check if we're already inside an activated
	// virtualenv. If so, just use it.
	if venv := os.Getenv("VIRTUAL_ENV"); venv != "" {
		return venv
	}

	// Take PYTHONUSERBASE into consideration, if set
	if userbase := os.Getenv("PYTHONUSERBASE"); userbase != "" {
		return userbase
	}

	return ""
}
func normalizePackageArgs(args []string) map[api.PkgName]api.PkgCoordinates {
	pkgs := make(map[api.PkgName]api.PkgCoordinates)
	versionComponent := regexp.MustCompile(pep440VersionComponent)
	for _, arg := range args {
		var rawName string
		var name api.PkgName
		var spec api.PkgSpec
		if found := matchPackageAndSpec.FindSubmatch([]byte(arg)); len(found) > 0 {
			rawName = string(found[1])
			name = api.PkgName(rawName)
			spec = api.PkgSpec(string(found[2]))
		} else {
			split := strings.SplitN(arg, " ", 2)
			rawName = split[0]
			name = api.PkgName(rawName)
			if len(split) > 1 {
				specStr := strings.TrimSpace(split[1])

				if specStr != "" {
					if offset := versionComponent.FindIndex([]byte(spec)); len(offset) == 0 {
						spec = api.PkgSpec("==" + specStr)
					} else {
						spec = api.PkgSpec(specStr)
					}
				} else {
					spec = api.PkgSpec(specStr)
				}
			}
		}
		pkgs[normalizePackageName(name)] = api.PkgCoordinates{
			Name: rawName,
			Spec: spec,
		}
	}
	return pkgs
}

// normalizePackageName implements NormalizePackageName for the Python
// backends.
// See https://packaging.python.org/en/latest/specifications/name-normalization/
func normalizePackageName(name api.PkgName) api.PkgName {
	nameStr := string(name)
	nameStr = strings.ToLower(nameStr)
	nameStr = normalizationPattern.ReplaceAllString(nameStr, "-")
	return api.PkgName(nameStr)
}

func info(name api.PkgName) api.PkgInfo {
	res, err := api.HttpClient.Get(fmt.Sprintf("https://pypi.org/pypi/%s/json", string(name)))

	if err != nil {
		util.DieNetwork("HTTP Request failed with error: %s", err)
	}

	defer res.Body.Close()

	if res.StatusCode == 404 {
		return api.PkgInfo{}
	}

	if res.StatusCode != 200 {
		util.DieNetwork("Received status code: %d", res.StatusCode)
	}

	body, err := io.ReadAll(res.Body)
	if err != nil {
		util.DieProtocol("Res body read failed with error: %s", err)
	}

	var output pypiEntryInfoResponse
	if err := json.Unmarshal(body, &output); err != nil {
		util.DieProtocol("PyPI response: %s", err)
	}

	info := api.PkgInfo{
		Name:             output.Info.Name,
		Description:      output.Info.Summary,
		Version:          output.Info.Version,
		HomepageURL:      output.Info.HomePage,
		DocumentationURL: output.Info.DocsURL,
		BugTrackerURL:    output.Info.BugTrackerURL,
		Author: util.AuthorInfo{
			Name:  output.Info.Author,
			Email: output.Info.AuthorEmail,
		}.String(),
		License: output.Info.License,
	}

	deps := []string{}
	for _, line := range output.Info.RequiresDist {
		if strings.Contains(line, "extra ==") {
			continue
		}

		deps = append(deps, strings.Fields(line)[0])
	}
	info.Dependencies = deps

	return info
}

func searchPypi(query string) []api.PkgInfo {
	// Normalize query before looking it up in the overide map
	query = string(normalizePackageName(api.PkgName(query)))
	if renamed, found := moduleToPypiPackageOverride[query]; found {
		query = renamed[0]
	}
	results, err := SearchPypi(query)
	if err != nil {
		util.DieNetwork("failed to search pypi: %s", err.Error())
	}
	// Elide package override from results
	filtered := []api.PkgInfo{}
	for _, info := range results {
		lowered := string(normalizePackageName(api.PkgName(info.Name)))
		if rename, ok := moduleToPypiPackageOverride[lowered]; ok {
			// If rename[0] == lowered, we are noconflicting a package
			// If they are different, we are overriding that package,
			// so the query'd package should not be included in the results
			// set.
			if rename[0] != lowered {
				continue
			}
		}
		filtered = append(filtered, info)
	}
	return filtered
}

var pythonGuessRegexps = util.Regexps([]string{
	// The (?:.|\\\n) subexpression allows us to
	// match match multiple lines if
	// backslash-escapes are used on the newlines.
	`from (?:.|\\\n) import`,
	`import ((?:.|\\\n)*) as`,
	`import ((?:.|\\\n)*)`,
})

func readPyproject() (*pyprojectTOML, error) {
	var cfg pyprojectTOML
	if _, err := toml.DecodeFile("pyproject.toml", &cfg); err != nil {
		return nil, err
	}
	return &cfg, nil
}

// getPython3 returns either "python3" or the value of the UPM_PYTHON3
// environment variable.
func getPython3() string {
	python3 := os.Getenv("UPM_PYTHON3")
	if python3 != "" {
		return python3
	} else {
		return "python3"
	}
}

// PythonPoetryBackend is a UPM backend for Python 3 that uses Poetry.
var PythonPoetryBackend = makePythonPoetryBackend(getPython3())
var PythonPipBackend = makePythonPipBackend(getPython3())
