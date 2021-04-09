// Package dotnet provides a backend for c# using dotnet and nuget.org
package dotnet

import (
	"encoding/json"
	"encoding/xml"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"os"
	"strings"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

const lockFile = "packages.lock.json"
const searchQueryURL = "https://azuresearch-usnc.nuget.org/query"

// an individual package record from .csproj file
type packageReference struct {
	XMLName xml.Name `xml:"PackageReference"`
	Include string   `xml:"Include,attr"`
	Version string   `xml:"Version,attr"`
}

// .csproj file structure
type project struct {
	XMLName  xml.Name           `xml:"Project"`
	Packages []packageReference `xml:"ItemGroup>PackageReference"`
}

// nuget.org info lookup result
type infoResult struct {
	Versions []string `json:"versions"`
}

// nuget .nuspec file package repository data
type repository struct {
	XMLName xml.Name `xml:"repository"`
	Type    string   `xml:"type,attr"`
	URL     string   `xml:"url,attr"`
	Commit  string   `xml:"commit,attr"`
}

// nuget .nuspec file package metadata
type packageMetadata struct {
	XMLName     xml.Name   `xml:"metadata"`
	ID          string     `xml:"id"`
	Version     string     `xml:"version"`
	Title       string     `xml:"title"`
	Author      string     `xml:"author"`
	Description string     `xml:"description"`
	License     string     `xml:"license"`
	Repository  repository `xml:"repository"`
	ProjectURL  string     `xml:"projectUrl"`
}

// nuget .nuspec file data
type nugetPackage struct {
	XMLName  xml.Name        `xml:"package"`
	Metadata packageMetadata `xml:"metadata"`
}

// nuget search service result entry
type searchResultData struct {
	ID          string
	Version     string
	Description string
	ProjectURL  string
}

// nuget search service result record
type searchResult struct {
	TotalHits int
	Data      []searchResultData
}

// removes packages using dotnet command and updates lock file
func removePackages(pkgs map[api.PkgName]bool) {
	for packageName := range pkgs {
		command := []string{"dotnet", "remove", findSpecFile(), "package", string(packageName)}
		util.RunCmd(command)
	}
	lock()
}

// adds packages using dotnet command which automatically updates lock files
func addPackages(pkgs map[api.PkgName]api.PkgSpec, projectName string) {
	for packageName, spec := range pkgs {
		command := []string{"dotnet", "add", "package", string(packageName)}
		if string(spec) != "" {
			command = append(command, "--version", string(spec))
		}
		util.RunCmd(command)
	}
}

// installs all packages using dotnet command
func install() {
	util.RunCmd([]string{"dotnet", "restore"})
}

// generates or updates the lock file using dotnet command
func lock() {
	util.RunCmd([]string{"dotnet", "restore", "--use-lock-file"})
}

// find the first ten projects that match the query string on nuget.org
func search(query string) []api.PkgInfo {
	pkgs := []api.PkgInfo{}
	queryURL := fmt.Sprintf("%s?q=%s&take=10", searchQueryURL, query)

	res, err := http.Get(queryURL)
	if err != nil {
		util.Die("failed to query for packages: %s", err)
	}
	defer res.Body.Close()

	if res.StatusCode != http.StatusOK {
		return pkgs
	}

	body, err := ioutil.ReadAll(res.Body)
	if err != nil {
		util.Die("Could not read response: %s", err)
	}

	var searchResult searchResult
	err = json.Unmarshal(body, &searchResult)
	if err != nil {
		util.Die("Could not unmarshar response data: %", err)
	}

	for _, data := range searchResult.Data {
		util.ProgressMsg(data.ID)
		pkgs = append(pkgs, api.PkgInfo{
			Name:          data.ID,
			Version:       data.Version,
			Description:   data.Description,
			SourceCodeURL: data.ProjectURL,
		})
	}

	return pkgs
}

// looks for the .csproj file in the current directory
func findSpecFile() string {
	files, err := ioutil.ReadDir("./")
	if err != nil {
		util.Die("Can't read current directory")
	}

	for _, f := range files {
		if strings.HasSuffix(f.Name(), ".csproj") {
			return f.Name()
		}
	}

	return ".csproj"
}

// looks up all the versions of the package and gets retails for the latest version from nuget.org
func info(pkgName api.PkgName) api.PkgInfo {
	lowID := strings.ToLower(string(pkgName))
	infoURL := fmt.Sprintf("https://api.nuget.org/v3-flatcontainer/%s/index.json", lowID)

	res, err := http.Get(infoURL)
	if err != nil {
		util.Die("failed to get the versions")
	}
	defer res.Body.Close()
	body, err := ioutil.ReadAll(res.Body)
	if err != nil {
		util.Die("Could not read response\n")
	}
	var infoResult infoResult
	err = json.Unmarshal(body, &infoResult)
	if err != nil {
		util.Die("Could not read json body")
	}
	latestVersion := infoResult.Versions[len(infoResult.Versions)-1]
	util.ProgressMsg(fmt.Sprintf("latest version of %s is %s", pkgName, latestVersion))
	specURL := fmt.Sprintf("https://api.nuget.org/v3-flatcontainer/%s/%s/%s.nuspec", lowID, latestVersion, lowID)
	util.ProgressMsg(fmt.Sprintf("Getting spec from %s", specURL))
	res, err = http.Get(specURL)
	if err != nil {
		util.Die("Failed to get the spec")
	}
	defer res.Body.Close()
	body, err = ioutil.ReadAll(res.Body)
	if err != nil {
		util.Die("Could not read response\n")
	}
	var nugetPackage nugetPackage
	err = xml.Unmarshal(body, &nugetPackage)
	if err != nil {
		util.Die(fmt.Sprintf("Failed to read spec %s", err))
	}

	pkgInfo := api.PkgInfo{
		Name:          nugetPackage.Metadata.ID,
		Version:       nugetPackage.Metadata.Version,
		Description:   nugetPackage.Metadata.Description,
		Author:        nugetPackage.Metadata.Author,
		License:       nugetPackage.Metadata.License,
		SourceCodeURL: nugetPackage.Metadata.Repository.URL,
		HomepageURL:   nugetPackage.Metadata.ProjectURL,
	}
	return pkgInfo
}

// loads the details of the project spec file
func listSpecfile() map[api.PkgName]api.PkgSpec {
	var pkgs map[api.PkgName]api.PkgSpec
	projectFile := findSpecFile()
	if util.Exists(projectFile) {

		specReader, err := os.Open(projectFile)
		if err != nil {
			util.Die("Could not open %s, with error: %q", projectFile, err)
		}
		defer specReader.Close()

		pkgs, err = ReadSpec(specReader)
		if err != nil {
			util.Die("Failed to read spec file %s, with error: %q", projectFile, err)
		}
	}
	return pkgs
}

// reads the spec and builds up packages
func ReadSpec(specReader io.Reader) (map[api.PkgName]api.PkgSpec, error) {
	xmlbytes, err := ioutil.ReadAll(specReader)
	if err != nil {
		return nil, err
	}

	var project project
	err = xml.Unmarshal(xmlbytes, &project)
	if err != nil {
		return nil, fmt.Errorf("Failed to unmarshal file content %q", err)
	}

	pkgs := map[api.PkgName]api.PkgSpec{}
	for _, packageReference := range project.Packages {
		pkgName := packageReference.Include
		pkgVersion := api.PkgVersion(packageReference.Version)
		pkgs[api.PkgName(pkgName)] = api.PkgSpec(pkgVersion)
	}
	return pkgs, nil
}

// loads the details of the lock file
func listLockfile() map[api.PkgName]api.PkgVersion {
	pkgs := map[api.PkgName]api.PkgVersion{}
	if util.Exists(lockFile) {
		specReader, err := os.Open(lockFile)
		if err != nil {
			util.Die("Could not open %s, with error: %q", lockFile, err)
		}
		defer specReader.Close()

		pkgs, err = ReadLock(specReader)
		if err != nil {
			util.Die("Error reading lockFile %s %q", lockFile, err)
		}
	}
	return pkgs
}

// reads the lock file and buils up packages
func ReadLock(lockFileReader io.Reader) (map[api.PkgName]api.PkgVersion, error) {
	jsonBytes, err := ioutil.ReadAll(lockFileReader)
	if err != nil {
		return nil, err
	}
	var rawJson interface{}
	err = json.Unmarshal(jsonBytes, &rawJson)
	if err != nil {
		return nil, fmt.Errorf("Failed to unmarshal lock file data %q", err)
	}

	m := rawJson.(map[string]interface{})
	dependencies := m["dependencies"].(map[string]interface{})
	pkgs := map[api.PkgName]api.PkgVersion{}
	for _, v := range dependencies {
		packages := v.(map[string]interface{})
		for packageName, details := range packages {
			pkgs[api.PkgName(packageName)] = api.PkgVersion(details.(map[string]interface{})["resolved"].(string))
		}
	}

	return pkgs, nil
}

//DotNetBackend is the UPM language backend for C# using dotnet
var DotNetBackend = api.LanguageBackend{
	Name:             "csharp-dotnet",
	Specfile:         findSpecFile(),
	Lockfile:         lockFile,
	FilenamePatterns: []string{"*.cs", "*.csproj"},
	Remove:           removePackages,
	Add:              addPackages,
	Search:           search,
	Info:             info,
	Install:          install,
	Lock:             lock,
	ListSpecfile:     listSpecfile,
	ListLockfile:     listLockfile,
	GetPackageDir: func() string {
		return "bin/"
	},
	Quirks: api.QuirksAddRemoveAlsoLocks |
		api.QuirksAddRemoveAlsoInstalls |
		api.QuirksLockAlsoInstalls,
}
