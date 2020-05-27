package clojure

import (
	"encoding/json"
	"errors"
	"fmt"
	"github.com/replit/upm/internal/api"
	"io/ioutil"
	"net/http"
	"os"
	"strings"
)

type JavaPackage struct {
	Group    string
	Artifact string
}

func (pkg JavaPackage) toString() string {
	return pkg.Group + "/" + pkg.Artifact
}

func (self JavaPackage) fromString(s string) error {
	parts := strings.Split(string(name), "/")
	if len(parts) != 2 || len(parts[0]) == 0 || len(parts[1]) == 0 {
		return errors.New("Package name: " + s + " is not of the form <group id>/<artifact id>")
	}
	self.Group = parts[0]
	self.Artifact = parts[1]
	return nil
}

type MPkgInfo struct {
	Id            JavaPackage
	LatestVersion string
	Versions      []string
	Description   string
	Homepage      string
	ScmUrl        string
	IssuesUrl     string
	Author        string
}

type ClojarsSearchOp struct {
	Results []struct {
		JarName     string `json:"jar_name"`
		GroupName   string `json:"group_name"`
		Version     string `json:"version"`
		Description string `json:"description"`
	} `json:"results"`
}

type ClojarsFetchOp struct {
	GroupName string `json:"group_name"`
	JarName string `json:"jar_name"`
	Description string `json:"description"`
	Homepage string `json:"homepage"`
	Version string `json:"latest_version"`
	User string `json:"user"`
}

func GetMPackageInfoClojars(pkg JavaPackage) (MPkgInfo, error) {
	res, err := http.Get("https://clojars.org/api/artifacts/" + pkg.toString())
	if err != nil {
		return MPkgInfo{}, err
	}
	bs, err := ioutil.ReadAll(res.Body)
	var pinfo ClojarsFetchOp
	err = json.Unmarshal(bs, &pinfo)
	return MPkgInfo {
		Id: JavaPackage{
			pinfo.GroupName,
			pinfo.JarName,
		},
		Description: pinfo.Description,
		LatestVersion: pinfo.Version,
		Homepage: pinfo.Homepage,
		Author: pinfo.User,
	}, nil
}

func GetMPackageInfoMaven(pkg JavaPackage) (MPkgInfo, error) {
	return MPkgInfo{}, errors.New("unimplemented")
}

func GetMPackageInfo(pkg JavaPackage) (MPkgInfo, error) {
	cinfo, err := GetMPackageInfoClojars(pkg)
	if err == nil {
		return cinfo, nil
	} else {
		minfo, err := GetMPackageInfoMaven(pkg)
		if err == nil {
			return minfo, nil
		}
		return MPkgInfo{}, errors.New("Couldn't fetch info for package: " + pkg.toString() + " from any sources")
	}
}

func GenPkgInfo(pinfo MPkgInfo) api.PkgInfo {
	return api.PkgInfo{
		Name:             pinfo.Id.Group + "/" + pinfo.Id.Artifact,
		Description:      pinfo.Description,
		Version:          pinfo.LatestVersion,
		HomepageURL:      pinfo.Homepage,
		DocumentationURL: "",
		SourceCodeURL:    "",
		BugTrackerURL:    "",
		Author:           pinfo.Author,
		License:          "",
		Dependencies:     nil,
	}
}

func GetPackageInfo(pkg JavaPackage) (api.PkgInfo, error) {
	pinfo, err := GetMPackageInfo(pkg)
	if err != nil {
		return api.PkgInfo{}, err
	}
	return GenPkgInfo(pinfo), nil
}

func findPackagesClojars(name string) ([]api.PkgInfo, error) {
	s := "https://clojars.org/search?q=" + name + "&format=json"
	res, err := http.Get(s)
	if err != nil {
		return nil, err
	}
	var resp ClojarsSearchOp
	bs, err := ioutil.ReadAll(res.Body)
	err = json.Unmarshal(bs, &resp)
	if err != nil {
		return nil, err
	}

	fres := []api.PkgInfo{}
	for _, p := range resp.Results {
		pinfo, err := GetMPackageInfoClojars(JavaPackage{
			Artifact: p.JarName,
			Group:    p.GroupName,
		})
		if err != nil {
			panic(err)
		}
		fres = append(fres, GenPkgInfo(pinfo))
	}

	return fres, nil
}

func findPackagesMaven(name string) ([]api.PkgInfo, error) {
	return nil, errors.New("unimplemented")
}

func info(name api.PkgName) api.PkgInfo {
	s := string(name)
	parts := strings.Split(s, "/")
	if len(parts) != 2 {
		fmt.Println("Package name must be of the form <group id>/<artifact id>")
		os.Exit(1)
	}
	group, artifact := parts[0], parts[1]
	res, err := GetPackageInfo(JavaPackage{
		group,
		artifact,
	})
	if err != nil {
		fmt.Println(err.Error())
		os.Exit(1)
	}
	return res
}

func Search(name string) []api.PkgInfo {
	cpkg, err := findPackagesClojars(name)
	if err != nil {
		fmt.Println("Error while querying clojars: " + err.Error())
	}
	mpkg, err := findPackagesMaven(name)
	if err != nil {
		fmt.Println("Error while querying maven: " + err.Error())
	}
	return append(cpkg, mpkg...)
}

var DepsBackend = api.LanguageBackend{
	Name:     "clojure-deps",
	Specfile: "deps.edn",
	// Clojure has trace.edn, but it's a write only file that lists the jars used by current project, tools.deps
	// never reads it.
	Lockfile:             "trace.edn",
	FilenamePatterns:     []string{"*.clj", "*.cljc"},
	Quirks:               api.QuirksNotReproducible,
	NormalizePackageName: normalize,
	GetPackageDir: func() string {
		return "."
	},
	Search:       Search,
	Info:         info,
	Add:          addPackages,
	Remove:       removePackages,
	Install:      install,
	ListSpecfile: listSpecfile,
	ListLockfile: listLockfile,
}

func normalize(name api.PkgName) api.PkgName {
	parts := strings.Split(string(name), "/")
	if len(parts) != 2 || len(parts[0]) == 0 || len(parts[1]) == 0 {
		fmt.Println("Package name must be of the form <group id>/<artifact id>")
		os.Exit(1)
	}
	return name
}

func listLockfile() map[api.PkgName]api.PkgVersion {
	return map[api.PkgName]api.PkgVersion{}
}

func listSpecfile() map[api.PkgName]api.PkgSpec {
	return map[api.PkgName]api.PkgSpec{}
}

func install() {

}

func removePackages(m map[api.PkgName]bool) {

}

type DepsFormat struct {
	Deps map[string] map[string]string `edn:"deps"`
}

func addPackages(m map[api.PkgName]api.PkgSpec) {
	var pkg JavaPackage
	for name, spec := range m {
		err := pkg.fromString(string(name))
		if err != nil {
			fmt.Println(err.Error())
			os.Exit(1)
		}
		mPkgInfo, err := GetMPackageInfo(pkg)
		if err != nil {
			fmt.Println(err.Error())
			os.Exit(1)
		}
		version := string(spec)
		if version == "" {
			m[name] = api.PkgSpec(mPkgInfo.LatestVersion)
		} else {
			versionValid := false
			for _, v := range mPkgInfo.Versions {
				if v == version {
					versionValid = true
				}
			}
			if ! versionValid {
				fmt.Println("No artifact with version: \"" + string(spec) + "\" found for package: \"" + string(name) + "\"")
				os.Exit(1)
			}
		}
	}
}
