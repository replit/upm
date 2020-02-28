// Package java provides a backend for Java using maven.
package java

import (
	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

const hardcodedPom = `
<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
  <modelVersion>4.0.0</modelVersion>
 
  <groupId>com.mycompany.app</groupId>
  <artifactId>my-app</artifactId>
  <version>1.0-SNAPSHOT</version>
 
<dependencies>
	<dependency>
		<groupId>com.google.guava</groupId>
		<artifactId>guava</artifactId>
		<version>28.2-jre</version>
	</dependency>
</dependencies>
</project>
`

// javaPatterns is the FilenamePatterns value for JavaBackend.
var javaPatterns = []string{"*.java"}

// JavaBackend is the UPM language backend for Java using Maven.
var JavaBackend = api.LanguageBackend{
	Name:             "java-maven",
	Specfile:         "pom.xml",
	Lockfile:         "pom.xml",
	FilenamePatterns: javaPatterns,
	// Quirks:           api.QuirksNotReproducible,
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
		contentsB := []byte(hardcodedPom)
		util.ProgressMsg("write pom.xml")
		util.TryWriteAtomic("pom.xml", contentsB)
	},
	Remove: func(pkgs map[api.PkgName]bool) {
		contentsB := []byte(hardcodedPom)
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
