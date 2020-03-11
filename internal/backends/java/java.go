// Package nodejs provides backends for Java using Gradle and Maven.
package java

import (
	"encoding/xml"
	"fmt"
	"io/ioutil"
	"os"
	"regexp"
	"strings"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

// Maven

type Dependency struct {
	XMLName    xml.Name `xml:"dependency"`
	GroupId    string   `xml:"groupId"`
	ArtifactId string   `xml:"artifactId"`
	Version    string   `xml:"version"`
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

type MavenProject struct {
	XMLName      xml.Name     `xml:"project"`
	ModelVersion string       `xml:"modelVersion"`
	GroupId      string       `xml:"groupId"`
	ArtifactId   string       `xml:"artifactId"`
	Version      string       `xml:"version"`
	Dependencies []Dependency `xml:"dependencies>dependency"`
	Plugins      []Plugin     `xml:"build>plugins>plugin"`
}

const initialPom = `
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

// javaPatterns is the FilenamePatterns value for JavaBackend.
var javaPatterns = []string{"*.java"}

func readMavenProjectOrMakeEmpty(path string) MavenProject {
	var project MavenProject
	var xmlbytes []byte
	if util.Exists("pom.xml") {
		var err error
		xmlbytes, err = ioutil.ReadFile("pom.xml")
		if err != nil {
			util.Die("error reading pom.xml: %s", err)
		}
	} else {
		xmlbytes = []byte(initialPom)
	}
	err := xml.Unmarshal(xmlbytes, &project)
	if err != nil {
		util.Die("error unmarshalling pom.xml: %s", err)
	}
	return project
}

const pomdotxml = "pom.xml"

var pkgNameRegexp = regexp.MustCompile("^([^:]+):([^:]+)") // groupid:artifactid

func pkgNameToGA(pkgName api.PkgName) (string, string) {
	submatches := pkgNameRegexp.FindStringSubmatch(string(pkgName))
	if nil == submatches {
		util.Die(
			"package name %s does not match groupid:artifactid pattern",
			pkgName,
		)
	}

	groupId := submatches[1]
	artifactId := submatches[2]
	return groupId, artifactId
}

func lookupLatestVersion(groupId, artifactId string) string {
	query := fmt.Sprintf("g:%s AND a:%s", groupId, artifactId)
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
		util.Die("did not find a package %s:%s", groupId, artifactId)
	}
	searchDoc := searchDocs[0]
	return searchDoc.Version
}

func addPackages(
	existingDependencies []Dependency,
	pkgs map[api.PkgName]api.PkgSpec,
) []Dependency {

	existingPkgs := map[api.PkgName]api.PkgVersion{}
	for _, dependency := range existingDependencies {
		pkgName := api.PkgName(
			fmt.Sprintf("%s:%s", dependency.GroupId, dependency.ArtifactId),
		)
		pkgVersion := api.PkgVersion(dependency.Version)
		existingPkgs[pkgName] = pkgVersion
	}

	newDependencies := []Dependency{}
	for pkgName, pkgSpec := range pkgs {
		groupId, artifactId := pkgNameToGA(pkgName)
		if _, ok := existingPkgs[pkgName]; ok {
			// this package is already in the lock file
			continue
		}

		var versionString string
		if pkgSpec == "" {
			versionString = lookupLatestVersion(groupId, artifactId)
		} else {
			versionString = string(pkgSpec)
		}
		dependency := Dependency{
			GroupId:    groupId,
			ArtifactId: artifactId,
			Version:    versionString,
		}
		newDependencies = append(newDependencies, dependency)

	}

	return append(existingDependencies, newDependencies...)
}

func writeMavenProject(project MavenProject) {
	marshalled, err := xml.MarshalIndent(project, "", "  ")
	if err != nil {
		util.Die("could not marshal pom: %s", err)
	}

	contentsB := []byte(marshalled)
	util.ProgressMsg(fmt.Sprintf("write %s", pomdotxml))
	util.TryWriteAtomic(pomdotxml, contentsB)
}

func removePackages(
	existingDependencies []Dependency,
	pkgs map[api.PkgName]bool,
) []Dependency {

	dependenciesToKeep := []Dependency{}
	for _, dependency := range existingDependencies {
		pkgName := api.PkgName(
			fmt.Sprintf("%s:%s", dependency.GroupId, dependency.ArtifactId),
		)
		if _, ok := pkgs[pkgName]; ok {
			// removing this dependency
		} else {
			dependenciesToKeep = append(dependenciesToKeep, dependency)
		}
	}

	return dependenciesToKeep
}

func dependenciesToSpecs(dependencies []Dependency) map[api.PkgName]api.PkgSpec {
	pkgs := map[api.PkgName]api.PkgSpec{}
	for _, dependency := range dependencies {
		pkgName := api.PkgName(
			fmt.Sprintf("%s:%s", dependency.GroupId, dependency.ArtifactId),
		)
		pkgSpec := api.PkgSpec(dependency.Version)
		pkgs[pkgName] = pkgSpec
	}
	return pkgs
}

func dependenciesToVersions(dependencies []Dependency) map[api.PkgName]api.PkgVersion {
	pkgs := map[api.PkgName]api.PkgVersion{}
	for _, dependency := range dependencies {
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

const mavenPackageDir = "target/dependency"

var JavaMavenBackend = api.LanguageBackend{
	Name:             "java-maven",
	Specfile:         pomdotxml,
	Lockfile:         pomdotxml,
	FilenamePatterns: javaPatterns,
	Quirks:           api.QuirksAddRemoveAlsoLocks,
	GetPackageDir:    func() string { return mavenPackageDir },
	Search:           search,
	Info: func(name api.PkgName) api.PkgInfo {
		return api.PkgInfo{Name: string(name)}
	},
	Add: func(pkgs map[api.PkgName]api.PkgSpec) {
		project := readMavenProjectOrMakeEmpty(pomdotxml)
		dependencies := addPackages(project.Dependencies, pkgs)
		updatedProject := project
		updatedProject.Dependencies = dependencies
		writeMavenProject(updatedProject)
	},
	Remove: func(pkgs map[api.PkgName]bool) {
		project := readMavenProjectOrMakeEmpty(pomdotxml)
		dependencies := removePackages(project.Dependencies, pkgs)
		updatedProject := project
		updatedProject.Dependencies = dependencies
		writeMavenProject(updatedProject)
		os.RemoveAll("target/dependency")
	},
	Install: func() {
		util.RunCmd([]string{
			"mvn",
			"de.qaware.maven:go-offline-maven-plugin:resolve-dependencies",
			"dependency:copy-dependencies",
		})
	},
	ListSpecfile: func() map[api.PkgName]api.PkgSpec {
		project := readMavenProjectOrMakeEmpty(pomdotxml)
		return dependenciesToSpecs(project.Dependencies)
	},
	ListLockfile: func() map[api.PkgName]api.PkgVersion {
		project := readMavenProjectOrMakeEmpty(pomdotxml)
		return dependenciesToVersions(project.Dependencies)
	},
	Lock: func() {},
}

const buildDotGradle = "upm.build.gradle"
const gradlePackageDir = "upmdependencies"

const emptyBuildDotGradle = `
plugins {
    id 'java'
}

repositories {
    mavenCentral()
}

dependencies {
}

task copyDependencies(type: Copy) {
    from configurations.default
    into 'upmdependencies'
}
`

var gradleDependencyRegexp = regexp.MustCompile("implementation\\s+'[^']+'")

type GradleProject struct {
	Dependencies []Dependency
}

func readGradleProjectOrMakeEmpty(path string) GradleProject {
	var buildfilebytes []byte
	if util.Exists(path) {
		var err error
		buildfilebytes, err = ioutil.ReadFile(path)
		if err != nil {
			util.Die("error reading %s: %s", path, err)
		}
	} else {
		buildfilebytes = []byte(emptyBuildDotGradle)
	}

	dependencies := []Dependency{}
	for _, gav := range gradleDependencyRegexp.FindAll(buildfilebytes, -1) {
		submatches := pkgNameRegexp.FindStringSubmatch(string(gav))
		if nil == submatches {
			util.Die(
				"package name %s does not match groupid:artifactid pattern",
				gav,
			)
		}

		groupId := submatches[1]
		artifactId := submatches[2]
		version := submatches[3]

		dependency := Dependency{
			GroupId:    groupId,
			ArtifactId: artifactId,
			Version:    version,
		}

		dependencies = append(dependencies, dependency)
	}
	return GradleProject{
		Dependencies: dependencies,
	}
}

const buildDotGradleTemplate = `
plugins {
    id 'java'
}

repositories {
    mavenCentral()
}

dependencies {
%s
}

task copyDependencies(type: Copy) {
    from configurations.default
    into 'upmdependencies'
}
`

func writeGradleProject(project GradleProject) {
	gavs := []string{}
	for _, dependency := range project.Dependencies {
		gav := fmt.Sprintf(
			"    implementation '%s:%s:%s'",
			dependency.GroupId,
			dependency.ArtifactId,
			dependency.Version,
		)
		gavs = append(gavs, gav)
	}
	marshalled := fmt.Sprintf(buildDotGradleTemplate, strings.Join(gavs, "\n"))

	contentsB := []byte(marshalled)
	util.ProgressMsg(fmt.Sprintf("write %s", buildDotGradle))
	util.TryWriteAtomic(buildDotGradle, contentsB)
}

var JavaGradleBackend = api.LanguageBackend{
	Name:             "java-gradle",
	Specfile:         buildDotGradle,
	Lockfile:         buildDotGradle,
	FilenamePatterns: javaPatterns,
	Quirks:           api.QuirksAddRemoveAlsoLocks,
	GetPackageDir:    func() string { return gradlePackageDir },
	Search:           search,
	Info: func(name api.PkgName) api.PkgInfo {
		return api.PkgInfo{Name: string(name)}
	},
	Add: func(pkgs map[api.PkgName]api.PkgSpec) {
		project := readGradleProjectOrMakeEmpty(buildDotGradle)
		dependencies := addPackages(project.Dependencies, pkgs)
		updatedProject := project
		updatedProject.Dependencies = dependencies
		writeGradleProject(updatedProject)
	},
	Remove: func(pkgs map[api.PkgName]bool) {
		project := readGradleProjectOrMakeEmpty(buildDotGradle)
		dependencies := removePackages(project.Dependencies, pkgs)
		updatedProject := project
		updatedProject.Dependencies = dependencies
		writeGradleProject(updatedProject)
		os.RemoveAll(gradlePackageDir)
	},
	Install: func() {
		util.RunCmd([]string{"gradle", "-b", "upm.build.gradle", "copyDependencies"})
	},
	ListSpecfile: func() map[api.PkgName]api.PkgSpec {
		project := readGradleProjectOrMakeEmpty(buildDotGradle)
		return dependenciesToSpecs(project.Dependencies)
	},
	ListLockfile: func() map[api.PkgName]api.PkgVersion {
		project := readGradleProjectOrMakeEmpty(buildDotGradle)
		return dependenciesToVersions(project.Dependencies)
	},
	Lock: func() {},
}
