// Package java provides a backend for Java using maven.
package java

import (
	"encoding/xml"
	"io/ioutil"
	"regexp"

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

var emptyProject = Project{
	ModelVersion: "4.0.0",
	GroupId: "co.repl",
	ArtifactId: "artifact",
	Version: "0.0-SNAPSHOT",
	Dependencies: []Dependency{},
}

var packageRegexp = regexp.MustCompile("^([^:]+):([^:]+):([^:]+)")

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
		var project Project
		if util.Exists("pom.xml") {
			xmlbytes, err := ioutil.ReadFile("pom.xml")
			if err != nil {
				util.Die("error reading pom.xml: %s", err)
			}
			err = xml.Unmarshal(xmlbytes, &project)
			if err != nil {
				util.Die("error unmarshalling pom.xml: %s", err)
			}
		} else {
			project = emptyProject
		}

		newDependencies := []Dependency{}
		for name, _ := range pkgs {
			submatches := packageRegexp.FindStringSubmatch(string(name))
			if nil == submatches {
				util.Die("package name %s does not match groupid:artifactid:version pattern")
			} else {
				dependency := Dependency{
					GroupId: submatches[1],
					ArtifactId: submatches[2],
					Version: submatches[3],
				}
				newDependencies = append(newDependencies, dependency)
			}
		}

		project.Dependencies = append(project.Dependencies, newDependencies...)
		marshalled, err := xml.MarshalIndent(project, "", "  ")
		if err != nil {
			util.Die("could not marshal pom: %s", err)
		}

		contentsB := []byte(marshalled)
		util.ProgressMsg("write pom.xml")
		util.TryWriteAtomic("pom.xml", contentsB)
	},
	Remove: func(pkgs map[api.PkgName]bool) {
		marshalled, err := xml.MarshalIndent(emptyProject, "", "  ")
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
