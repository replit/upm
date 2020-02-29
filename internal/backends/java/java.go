// Package java provides a backend for Java using maven.
package java

import (
	"encoding/xml"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

type Dependency struct {
	XMLName xml.Name `xml:"dependency"`	
	GroupId string `xml:"groupId"`
	ArtifactId string `xml:"artifactId"`
	Version string `xml:"version"`	
}

type Project struct {
	XMLName xml.Name `xml:"project"`
	ModelVersion string `xml:"modelVersion"`
	GroupId string `xml:"groupId"`
	ArtifactId string `xml:"artifactId"`
	Version string `xml:"version"`
	Dependencies []Dependency `xml:"dependencies>dependency"`
}

var hardcodedProject = Project{
	ModelVersion: "4.0.0",
	GroupId: "co.repl",
	ArtifactId: "artifact",
	Version: "0.0-SNAPSHOT",
	Dependencies: []Dependency{
		{
			GroupId: "com.google.guava",
			ArtifactId: "guava",
			Version: "28.2-jre",
		},
	},
}

// javaPatterns is the FilenamePatterns value for JavaBackend.
var javaPatterns = []string{"*.java"}

// JavaBackend is the UPM language backend for Java using Maven.
var JavaBackend = api.LanguageBackend{
	Name:             "java-maven",
	Specfile:         "pom.xml",
	Lockfile:         "pom.xml",
	FilenamePatterns: javaPatterns,
	Quirks:           api.QuirksAddRemoveAlsoLocks,
	GetPackageDir: func() string {
		return "target/dependency"
	},
	Search: func(query string) []api.PkgInfo {
		var results []api.PkgInfo
		return results
	},
	Info: func(name api.PkgName) api.PkgInfo {
		return api.PkgInfo{}
	},
	Add: func(pkgs map[api.PkgName]api.PkgSpec) {
		marshalled, err := xml.MarshalIndent(hardcodedProject, "", "  ")
		if err != nil {
			util.Die("could not marshal pom: %s", err)
		}

		contentsB := []byte(marshalled)
		util.ProgressMsg("write pom.xml")
		util.TryWriteAtomic("pom.xml", contentsB)
	},
	Remove: func(pkgs map[api.PkgName]bool) {
		marshalled, err := xml.MarshalIndent(hardcodedProject, "", "  ")
		if err != nil {
			util.Die("could not marshal pom: %s", err)
		}

		contentsB := []byte(marshalled)
		util.ProgressMsg("write pom.xml")
		util.TryWriteAtomic("pom.xml", contentsB)
	},
	Install: func() {
		util.RunCmd([]string{"mvn", "dependency:copy-dependencies"})
	},
	ListSpecfile: func() map[api.PkgName]api.PkgSpec {
		pkgs := map[api.PkgName]api.PkgSpec{}
		return pkgs
	},
	ListLockfile: func() map[api.PkgName]api.PkgVersion {
		pkgs := map[api.PkgName]api.PkgVersion{}
		name := api.PkgName("guava")
		version := api.PkgVersion("28.2-jre")
		pkgs[name] = version
		return pkgs
	},
	Lock: func(){},
}
