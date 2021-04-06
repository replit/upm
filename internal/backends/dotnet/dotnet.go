package dotnet

import (
	"encoding/json"
	"encoding/xml"
	"fmt"
	"io/ioutil"
	"net/http"
	"strings"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

const lockFile = "packages.lock.json"

type packageReference struct {
	XMLName xml.Name `xml:"PackageReference"`
	Include string   `xml:"Include,attr"`
	Version string   `xml:"Version,attr"`
}

type propertyGroup struct {
	XMLName         xml.Name `xml:"PropertyGroup"`
	OutputType      string   `xml:"OutputType"`
	TargetFramework string   `xml:"TargetFramework"`
}

type project struct {
	XMLName       xml.Name           `xml:"Project"`
	PropertyGroup propertyGroup      `xml:"PropertyGroup"`
	Packages      []packageReference `xml:"ItemGroup>PackageReference"`
}

type infoResult struct {
	Versions []string `json:"versions"`
}

type repository struct {
	XMLName xml.Name `xml:"repository"`
	Type    string   `xml:"type,attr"`
	URL     string   `xml:"url,attr"`
	Commit  string   `xml:"commit,attr"`
}
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
type nugetPackage struct {
	XMLName  xml.Name        `xml:"package"`
	Metadata packageMetadata `xml:"metadata"`
}

func removePackages(pkgs map[api.PkgName]bool) {}

func addPackages(pkgs map[api.PkgName]api.PkgSpec, projectName string) {
	for packageName, spec := range pkgs {
		command := []string{"dotnet", "add", "package", string(packageName)}
		if string(spec) != "" {
			command = append(command, "--version", string(spec))
		}
		util.RunCmd(command)
	}
}

func search(query string) []api.PkgInfo {
	return []api.PkgInfo{}
}

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

func info(pkgName api.PkgName) api.PkgInfo {
	lowID := strings.ToLower(string(pkgName))
	infoURL := fmt.Sprintf("https://api.nuget.org/v3-flatcontainer/%s/index.json", lowID)
	util.ProgressMsg(fmt.Sprintf("Looking up versions at: %s", infoURL))
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

func listSpecfile() map[api.PkgName]api.PkgSpec {
	pkgs := map[api.PkgName]api.PkgSpec{}
	projectFile := findSpecFile()
	if util.Exists(projectFile) {
		var project project
		var xmlbytes []byte
		var err error

		xmlbytes, err = ioutil.ReadFile(projectFile)
		if err != nil {
			util.Die("error reading spec file: %s", err)
		}
		err = xml.Unmarshal(xmlbytes, &project)
		if err != nil {
			util.Die("error unmarshaling spec file: %s", err)
		}
		util.ProgressMsg(fmt.Sprintf("Project: %s", project))
		for _, packageReference := range project.Packages {
			pkgName := packageReference.Include
			pkgVersion := api.PkgVersion(packageReference.Version)
			pkgs[api.PkgName(pkgName)] = api.PkgSpec(pkgVersion)
		}
	}
	return pkgs
}

func listLockfile() map[api.PkgName]api.PkgVersion {
	pkgs := map[api.PkgName]api.PkgVersion{}
	if util.Exists(lockFile) {
		jsonBytes, err := ioutil.ReadFile(lockFile)
		if err != nil {
			util.Die("error reading lock file: %s", err)
		}
		var rawJson interface{}
		err = json.Unmarshal(jsonBytes, &rawJson)
		if err != nil {
			util.Die("error unmashaling lock file: %s", err)
		}

		m := rawJson.(map[string]interface{})
		dependencies := m["dependencies"].(map[string]interface{})
		for _, v := range dependencies {
			packages := v.(map[string]interface{})
			for packageName, details := range packages {
				pkgs[api.PkgName(packageName)] = api.PkgVersion(details.(map[string]interface{})["resolved"].(string))
			}
		}

	}
	return pkgs
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
	Install:          func() {},
	Lock:             func() {},
	ListSpecfile:     listSpecfile,
	ListLockfile:     listLockfile,
	GetPackageDir: func() string {
		return "bin/"
	},
}
