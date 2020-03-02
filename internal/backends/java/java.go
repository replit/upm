// Package java provides a backend for Java using maven.
package java

import (
	"encoding/xml"
	"io/ioutil"
  "os"
	"regexp"
  "strings"

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

var pkgNameRegexp = regexp.MustCompile("^([^:]+):([^:]+)") // groupid:artifactid

// javaPatterns is the FilenamePatterns value for JavaBackend.
var javaPatterns = []string{"*.java"}

func readProjectOrMakeEmpty(path string) Project {
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
  return project
}

const pomdotxml = "pom.xml"

// JavaBackend is the UPM language backend for Java using Maven.
var JavaBackend = api.LanguageBackend{
	Name:             "java-maven",
	Specfile:         pomdotxml,
	Lockfile:         pomdotxml,
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
		project := readProjectOrMakeEmpty(pomdotxml)
    existingDependencies := map[api.PkgName]api.PkgVersion{}
    for _, dependency := range project.Dependencies {
      pkgName := api.PkgName(strings.Join([]string{dependency.GroupId, dependency.ArtifactId}, ":"))
      pkgVersion := api.PkgVersion(dependency.Version)
      existingDependencies[pkgName] = pkgVersion
    }

		newDependencies := []Dependency{}
		for pkgName, pkgSpec := range pkgs {
			submatches := pkgNameRegexp.FindStringSubmatch(string(pkgName))
			if nil == submatches {
				util.Die("package name %s does not match groupid:artifactid pattern")
			} else {
        if _, ok := existingDependencies[pkgName]; ok {
          // this package is already in the lock file
        } else {
          dependency := Dependency{
            GroupId: submatches[1],
            ArtifactId: submatches[2],
            Version: string(pkgSpec),
          }
          newDependencies = append(newDependencies, dependency)
        }
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
    project := readProjectOrMakeEmpty(pomdotxml)

    dependenciesToKeep := []Dependency{}
    for _, dependency := range(project.Dependencies) {
      pkgName := api.PkgName(strings.Join([]string{dependency.GroupId, dependency.ArtifactId}, ":"))
      if _, ok := pkgs[pkgName]; ok {
        // removing this dependency
      } else {
        dependenciesToKeep = append(dependenciesToKeep, dependency)
      }
    }

    projectWithFilteredDependencies := project
    projectWithFilteredDependencies.Dependencies = dependenciesToKeep

    marshalled, err := xml.MarshalIndent(projectWithFilteredDependencies, "", "  ")
    if err != nil {
      util.Die("error marshalling pom.xml: %s", err)
    }
		contentsB := []byte(marshalled)
		util.ProgressMsg("write pom.xml")
		util.TryWriteAtomic("pom.xml", contentsB)

    os.RemoveAll("target/dependency")
	},
	Install: func() {
		util.RunCmd([]string{"mvn", "dependency:copy-dependencies"})
	},
	ListSpecfile: func() map[api.PkgName]api.PkgSpec {
    project := readProjectOrMakeEmpty(pomdotxml)
		pkgs := map[api.PkgName]api.PkgSpec{}
    for _, dependency := range(project.Dependencies) {
      pkgName := api.PkgName(strings.Join([]string{dependency.GroupId, dependency.ArtifactId}, ":"))
      pkgSpec := api.PkgSpec(dependency.Version)
      pkgs[pkgName] = pkgSpec
    }
		return pkgs
	},
	ListLockfile: func() map[api.PkgName]api.PkgVersion {
    project := readProjectOrMakeEmpty(pomdotxml)
		pkgs := map[api.PkgName]api.PkgVersion{}
    for _, dependency := range(project.Dependencies) {
      pkgName := api.PkgName(strings.Join([]string{dependency.GroupId, dependency.ArtifactId}, ":"))
      pkgVersion := api.PkgVersion(dependency.Version)
      pkgs[pkgName] = pkgVersion
    }
		return pkgs
	},
	Lock: func(){},
}
