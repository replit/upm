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

func formatJavaPackage(pkg JavaPackage) string {
	return pkg.Group + "/" + pkg.Artifact
}

type Pkg struct {
	JarName     string `json:"jar_name"`
	GroupName   string `json:"group_name"`
	Version     string `json:"version"`
	Description string `json:"description"`
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

func GetPackageInfoClojars(pkg JavaPackage) (api.PkgInfo, error) {
	res, err := http.Get("https://clojars.org/api/artifacts/" + formatJavaPackage(pkg))
	if err != nil {
		return api.PkgInfo{}, err
	}
	bs, err := ioutil.ReadAll(res.Body)
	var pinfo ClojarsFetchOp
	err = json.Unmarshal(bs, &pinfo)
	return api.PkgInfo{
		Name:             pinfo.GroupName + "/" + pinfo.JarName,
		Description:      pinfo.Description,
		Version:          pinfo.Version,
		HomepageURL:      pinfo.Homepage,
		DocumentationURL: "",
		SourceCodeURL:    "",
		BugTrackerURL:    "",
		Author:           pinfo.User,
		License:          "",
		Dependencies:     nil,
	}, err
}

func GetPackageInfoMaven(pkg JavaPackage) (api.PkgInfo, error) {
	return api.PkgInfo{}, errors.New("unimplemented")
}

func GetPackageInfo(pkg JavaPackage) (api.PkgInfo, error) {
	pinfo, err := GetPackageInfoClojars(pkg)
	if err == nil {
		return pinfo, nil
	} else {
		pinfo, err = GetPackageInfoMaven(pkg)
	}
	if err == nil {
		return pinfo, nil
	}
	return api.PkgInfo{}, errors.New("Couldn't fetch info for package: " + formatJavaPackage(pkg) + " from any sources")
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
		pinfo, err := GetPackageInfoClojars(JavaPackage{
			Artifact: p.JarName,
			Group:    p.GroupName,
		})
		if err != nil {
			panic(err)
		}
		fres = append(fres, pinfo)
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
	//NormalizePackageName: normalize,
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

func addPackages(m map[api.PkgName]api.PkgSpec) {

}
