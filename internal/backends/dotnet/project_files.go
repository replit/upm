package dotnet

import (
	"encoding/json"
	"encoding/xml"
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"strings"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

const lockFileName = "packages.lock.json"

// an individual package record from .NET project file
type packageReference struct {
	XMLName xml.Name `xml:"PackageReference"`
	Include string   `xml:"Include,attr"`
	Version string   `xml:"Version,attr"`
}

// .NET project file structure
type project struct {
	XMLName  xml.Name           `xml:"Project"`
	Packages []packageReference `xml:"ItemGroup>PackageReference"`
}

// looks for the .NET project files file in the current directory
func findSpecFile() string {
	files, err := ioutil.ReadDir("./")
	if err != nil {
		util.Die("can't read current directory: %s", err)
	}

	for _, f := range files {
		if strings.HasSuffix(f.Name(), ".csproj") || strings.HasSuffix(f.Name(), ".fsproj") {
			return f.Name()
		}
	}

	return ".csproj"
}

// loads the details of the project spec file
func listSpecfile() map[api.PkgName]api.PkgSpec {
	var pkgs map[api.PkgName]api.PkgSpec
	projectFile := findSpecFile()
	specReader, err := os.Open(projectFile)
	if errors.Is(err, os.ErrNotExist) {
		return pkgs
	}
	if err != nil {
		util.Die("Could not open %s, with error: %q", projectFile, err)
	}
	defer specReader.Close()

	pkgs, err = ReadSpec(specReader)
	if err != nil {
		util.Die("Failed to read spec file %s, with error: %q", projectFile, err)
	}

	return pkgs
}

// ReadSpec reads the spec and builds up packages.
func ReadSpec(specReader io.Reader) (map[api.PkgName]api.PkgSpec, error) {
	xmlbytes, err := ioutil.ReadAll(specReader)
	if err != nil {
		return nil, err
	}

	var project project
	err = xml.Unmarshal(xmlbytes, &project)
	if err != nil {
		return nil, fmt.Errorf("failed to unmarshal file content %q", err)
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

	specReader, err := os.Open(lockFileName)
	if errors.Is(err, os.ErrNotExist) {
		return pkgs
	}
	if err != nil {
		util.Die("Could not open %s, with error: %q", lockFileName, err)
	}
	defer specReader.Close()

	pkgs, err = ReadLock(specReader)
	if err != nil {
		util.Die("error reading lockFile %s: %q", lockFileName, err)
	}

	return pkgs
}

type lockFilePackage struct {
	Type         string
	Resolved     string
	ContentHash  string
	Dependencies map[string]string
}

type lockFile struct {
	Version      int
	Dependencies map[string]map[string]lockFilePackage
}

// ReadLock reads the lock file and buils up packages.
func ReadLock(lockFileReader io.Reader) (map[api.PkgName]api.PkgVersion, error) {
	jsonBytes, err := ioutil.ReadAll(lockFileReader)
	if err != nil {
		return nil, err
	}
	var lockFile lockFile
	err = json.Unmarshal(jsonBytes, &lockFile)
	if err != nil {
		return nil, fmt.Errorf("failed to unmarshal lock file data: %q", err)
	}

	dependencies := lockFile.Dependencies
	pkgs := map[api.PkgName]api.PkgVersion{}
	for _, v := range dependencies {
		for packageName, packageDetails := range v {
			version := packageDetails.Resolved
			if version != "" {
				pkgs[api.PkgName(packageName)] = api.PkgVersion(packageDetails.Resolved)
			}
		}
	}

	return pkgs, nil
}
