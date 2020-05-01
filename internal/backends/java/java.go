// Package java provides a backend for Java using maven.
package java

import (
	"encoding/xml"
	"fmt"
	"io/ioutil"
	"os"
	"regexp"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

type Dependency struct {
	XMLName     xml.Name `xml:"dependency"`
	GroupId     string   `xml:"groupId"`
	ArtifactId  string   `xml:"artifactId"`
	Version     string   `xml:"version"`
	PackageType string   `xml:"type"`
}

type DynamicDependency struct {
	XMLName        xml.Name `xml:"DynamicDependency"`
	GroupId        string   `xml:"groupId"`
	ArtifactId     string   `xml:"artifactId"`
	Version        string   `xml:"version"`
	Classifier     string   `xml:"classifier"`
	RepositoryType string   `xml:"repositoryType"`
}

type PluginConfiguration struct {
	XMLName             xml.Name            `xml:"configuration"`
	DynamicDependencies []DynamicDependency `xml:"dynamicDependencies>DynamicDependency"`
}

type Plugin struct {
	XMLName       xml.Name            `xml:"plugin"`
	GroupId       string              `xml:"groupId"`
	ArtifactId    string              `xml:"artifactId"`
	Version       string              `xml:"version"`
	Configuration PluginConfiguration `xml:"configuration"`
}

type Project struct {
	XMLName      xml.Name     `xml:"project"`
	ModelVersion string       `xml:"modelVersion"`
	GroupId      string       `xml:"groupId"`
	ArtifactId   string       `xml:"artifactId"`
	Version      string       `xml:"version"`
	Dependencies []Dependency `xml:"dependencies>dependency"`
	Plugins      []Plugin     `xml:"build>plugins>plugin"`
}

const initialPomXml = `
<project>
<modelVersion>4.0.0</modelVersion>
<groupId>mygroupid</groupId>
<artifactId>myartifactid</artifactId>
<version>0.0-SNAPSHOT</version>
<build>
<plugins>
<plugin>
    <groupId>de.qaware.maven</groupId>
    <artifactId>go-offline-maven-plugin</artifactId>
    <version>1.2.5</version>
    <configuration>
        <dynamicDependencies>
            <DynamicDependency>
                <groupId>org.apache.maven.surefire</groupId>
                <artifactId>surefire-junit4</artifactId>
                <version>2.20.1</version>
                <repositoryType>PLUGIN</repositoryType>
            </DynamicDependency>
            <DynamicDependency>
                <groupId>com.querydsl</groupId>
                <artifactId>querydsl-apt</artifactId>
                <version>4.2.1</version>
                <classifier>jpa</classifier>
                <repositoryType>MAIN</repositoryType>
            </DynamicDependency>
        </dynamicDependencies>
    </configuration>
</plugin>
</plugins>
</build>
</project>
`

var pkgNameRegexp = regexp.MustCompile("^([^:]+):([^:]+)") // groupid:artifactid

// javaPatterns is the FilenamePatterns value for JavaBackend.
var javaPatterns = []string{"*.java"}

func readProjectOrMakeEmpty(path string) Project {
	var project Project
	var xmlbytes []byte
	if util.Exists("pom.xml") {
		var err error
		xmlbytes, err = ioutil.ReadFile("pom.xml")
		if err != nil {
			util.Die("error reading pom.xml: %s", err)
		}
	} else {
		xmlbytes = []byte(initialPomXml)
	}
	err := xml.Unmarshal(xmlbytes, &project)
	if err != nil {
		util.Die("error unmarshalling pom.xml: %s", err)
	}
	return project
}

const pomdotxml = "pom.xml"

func addPackages(pkgs map[api.PkgName]api.PkgSpec) {
	project := readProjectOrMakeEmpty(pomdotxml)
	existingDependencies := map[api.PkgName]api.PkgVersion{}
	for _, dependency := range project.Dependencies {
		pkgName := api.PkgName(
			fmt.Sprintf("%s:%s", dependency.GroupId, dependency.ArtifactId),
		)
		pkgVersion := api.PkgVersion(dependency.Version)
		existingDependencies[pkgName] = pkgVersion
	}

	newDependencies := []Dependency{}
	for pkgName, pkgSpec := range pkgs {
		submatches := pkgNameRegexp.FindStringSubmatch(string(pkgName))
		if nil == submatches {
			util.Die(
				"package name %s does not match groupid:artifactid pattern",
				pkgName,
			)
		}

		groupId := submatches[1]
		artifactId := submatches[2]
		if _, ok := existingDependencies[pkgName]; ok {
			// this package is already in the lock file
			continue
		}

		var query string
		if pkgSpec == "" {
			query = fmt.Sprintf("g:%s AND a:%s", groupId, artifactId)
		} else {
			query = fmt.Sprintf("g:%s AND a:%s AND v:%s", groupId, artifactId, pkgSpec)
		}
		searchDocs, err := Search(query)
		if err != nil {
			util.Die(
				"error searching maven for latest version of %s:%s: %s",
				groupId,
				artifactId,
				err,
			)
		}
		if len(searchDocs) == 0 {
			if pkgSpec == "" {
				util.Die("did not find a package %s:%s", groupId, artifactId)
			} else {
				util.Die("did not find a package %s:%s:%s", groupId, artifactId, pkgSpec)
			}
		}
		searchDoc := searchDocs[0]

		var versionString string
		if pkgSpec == "" {
			versionString = searchDoc.Version
		} else {
			versionString = string(pkgSpec)
		}

		var packageType string
		if searchDoc.PackageType == "pom" {
			packageType = "pom"
		} else {
			packageType = "jar"
		}

		dependency := Dependency{
			GroupId:     submatches[1],
			ArtifactId:  submatches[2],
			Version:     versionString,
			PackageType: packageType,
		}
		newDependencies = append(newDependencies, dependency)

	}

	project.Dependencies = append(project.Dependencies, newDependencies...)
	marshalled, err := xml.MarshalIndent(project, "", "  ")
	if err != nil {
		util.Die("could not marshal pom: %s", err)
	}

	contentsB := []byte(marshalled)
	util.ProgressMsg("write pom.xml")
	util.TryWriteAtomic("pom.xml", contentsB)
}

func removePackages(pkgs map[api.PkgName]bool) {
	project := readProjectOrMakeEmpty(pomdotxml)

	dependenciesToKeep := []Dependency{}
	for _, dependency := range project.Dependencies {
		pkgName := api.PkgName(
			fmt.Sprintf("%s:%s", dependency.GroupId, dependency.ArtifactId),
		)
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
}

func listSpecfile() map[api.PkgName]api.PkgSpec {
	project := readProjectOrMakeEmpty(pomdotxml)
	pkgs := map[api.PkgName]api.PkgSpec{}
	for _, dependency := range project.Dependencies {
		pkgName := api.PkgName(
			fmt.Sprintf("%s:%s", dependency.GroupId, dependency.ArtifactId),
		)
		pkgSpec := api.PkgSpec(dependency.Version)
		pkgs[pkgName] = pkgSpec
	}
	return pkgs
}

func listLockfile() map[api.PkgName]api.PkgVersion {
	project := readProjectOrMakeEmpty(pomdotxml)
	pkgs := map[api.PkgName]api.PkgVersion{}
	for _, dependency := range project.Dependencies {
		pkgName := api.PkgName(
			fmt.Sprintf("%s:%s", dependency.GroupId, dependency.ArtifactId),
		)
		pkgVersion := api.PkgVersion(dependency.Version)
		pkgs[pkgName] = pkgVersion
	}
	return pkgs
}

func search(query string) []api.PkgInfo {
	searchDocs, err := Search(query)
	if err != nil {
		util.Die("error searching maven %s", err)
	}
	pkgInfos := []api.PkgInfo{}
	for _, searchDoc := range searchDocs {
		pkgInfo := api.PkgInfo{
			Name:    fmt.Sprintf("%s:%s", searchDoc.Group, searchDoc.Artifact),
			Version: searchDoc.Version,
		}
		pkgInfos = append(pkgInfos, pkgInfo)
	}
	return pkgInfos
}

func info(pkgName api.PkgName) api.PkgInfo {
	searchDoc, err := Info(string(pkgName))

	if err != nil {
		util.Die("error searching maven %s", err)
	}

	if searchDoc.Artifact == "" {
		return api.PkgInfo{}
	}

	pkgInfo := api.PkgInfo{
		Name:    fmt.Sprintf("%s:%s", searchDoc.Group, searchDoc.Artifact),
		Version: searchDoc.CurrentVersion,
	}
	return pkgInfo
}

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
	Search: search,
	Info:   info,
	Add:    addPackages,
	Remove: removePackages,
	Install: func() {
		util.RunCmd([]string{
			"mvn",
			"de.qaware.maven:go-offline-maven-plugin:resolve-dependencies",
			"dependency:copy-dependencies",
		})
	},
	ListSpecfile: listSpecfile,
	ListLockfile: listLockfile,
	Lock:         func() {},
}
