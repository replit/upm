// Package java provides a backend for Java using maven.
package java

import (
	"fmt"
	"io/ioutil"
	"os"
	"regexp"
	"strings"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

var gaRegexp = regexp.MustCompile("^([^:]+):([^:]+)")         // groupid:artifactid
var gavRegexp = regexp.MustCompile("([^:]+):([^:]+):([^:]+)") // groupid:artifactid:version

type GradleProject struct {
	Dependencies []Dependency
}

const buildFilename = "upm.build.gradle"

const emptyBuildFile = `
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

const buildFileTemplate = `
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

var dependencyRegexp = regexp.MustCompile("implementation\\s+'[^']+'")

func readGradleGradleProjectOrMakeEmpty(path string) GradleProject {
	var buildfilebytes []byte
	if util.Exists(path) {
		var err error
		buildfilebytes, err = ioutil.ReadFile(path)
		if err != nil {
			util.Die("error reading %s: %s", path, err)
		}
	} else {
		buildfilebytes = []byte(emptyBuildFile)
	}

	dependencies := []Dependency{}
	for _, gav := range dependencyRegexp.FindAll(buildfilebytes, -1) {
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

func addGradlePackages(pkgs map[api.PkgName]api.PkgSpec) {
	project := readGradleGradleProjectOrMakeEmpty(buildFilename)
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

		var versionString string
		if pkgSpec == "" {
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
			versionString = searchDoc.Version
		} else {
			versionString = string(pkgSpec)
		}
		dependency := Dependency{
			GroupId:    submatches[1],
			ArtifactId: submatches[2],
			Version:    versionString,
		}
		newDependencies = append(newDependencies, dependency)

	}

	allDependencies := append(project.Dependencies, newDependencies...)
	gavs := []string{}
	for _, dependency := range allDependencies {
		gav := fmt.Sprintf(
			"    implementation '%s:%s:%s'",
			dependency.GroupId,
			dependency.ArtifactId,
			dependency.Version,
		)
		gavs = append(gavs, gav)
	}
	marshalled := fmt.Sprintf(buildFileTemplate, strings.Join(gavs, "\n"))

	contentsB := []byte(marshalled)
	util.ProgressMsg(fmt.Sprintf("write %s", buildFilename))
	util.TryWriteAtomic(buildFilename, contentsB)
}

func removeGradlePackages(pkgs map[api.PkgName]bool) {
	project := readGradleGradleProjectOrMakeEmpty(buildFilename)

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

	gavs := []string{}
	for _, dependency := range dependenciesToKeep {
		gav := fmt.Sprintf(
			"    implementation '%s:%s:%s'",
			dependency.GroupId,
			dependency.ArtifactId,
			dependency.Version,
		)
		gavs = append(gavs, gav)
	}
	marshalled := fmt.Sprintf(buildFileTemplate, strings.Join(gavs, "\n"))

	contentsB := []byte(marshalled)
	util.ProgressMsg(fmt.Sprintf("write %s", buildFilename))
	util.TryWriteAtomic(buildFilename, contentsB)

	os.RemoveAll("upmdependencies")
}

func listGradleSpecfile() map[api.PkgName]api.PkgSpec {
	project := readGradleGradleProjectOrMakeEmpty(buildFilename)
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

func listGradleLockfile() map[api.PkgName]api.PkgVersion {
	project := readGradleGradleProjectOrMakeEmpty(buildFilename)
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

// JavaBackend is the UPM language backend for Java using Maven.
var JavaGradleBackend = api.LanguageBackend{
	Name:             "java-gradle",
	Specfile:         buildFilename,
	Lockfile:         buildFilename,
	FilenamePatterns: javaPatterns,
	Quirks:           api.QuirksAddRemoveAlsoLocks,
	GetPackageDir: func() string {
		return "upmdependencies"
	},
	Search: search,
	Info: func(name api.PkgName) api.PkgInfo {
		return api.PkgInfo{
			Name: string(name),
		}
	},
	Add:    addGradlePackages,
	Remove: removeGradlePackages,
	Install: func() {
		util.RunCmd([]string{"gradle", "-b", "upm.build.gradle", "copyDependencies"})
	},
	ListSpecfile: listGradleSpecfile,
	ListLockfile: listGradleLockfile,
	Lock:         func() {},
}
