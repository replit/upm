package dotnet

import (
	"encoding/xml"
	"fmt"
	"io/ioutil"
	"strings"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

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

func removePackages(pkgs map[api.PkgName]bool) {}

func addPackages(pkgs map[api.PkgName]api.PkgSpec, projectName string) {}

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
	return api.PkgInfo{}
}

func listSpecfile() map[api.PkgName]api.PkgSpec {
	pkgs := map[api.PkgName]api.PkgSpec{}
	util.ProgressMsg("looking for project file")
	projectFile := findSpecFile()
	if util.Exists(projectFile) {
		util.ProgressMsg(fmt.Sprintf("Found %s", projectFile))
		var project project
		var xmlbytes []byte
		var err error

		xmlbytes, err = ioutil.ReadFile(projectFile)
		util.ProgressMsg("Read inputfile")
		if err != nil {
			util.Die("error reading spec file: %s", err)
		}
		err = xml.Unmarshal(xmlbytes, &project)
		util.ProgressMsg("unmarshed xml")
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
	return pkgs
}

//DotNetBackend is the UPM language backend for C# using dotnet
var DotNetBackend = api.LanguageBackend{
	Name:             "csharp-dotnet",
	Specfile:         findSpecFile(),
	Lockfile:         "packages.lock.json",
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
