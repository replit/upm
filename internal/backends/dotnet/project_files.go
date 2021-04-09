package dotnet

import (
	"encoding/json"
	"encoding/xml"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"strings"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

const lockFile = "packages.lock.json"

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
