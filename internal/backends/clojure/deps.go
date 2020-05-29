package clojure

import (
	"encoding/json"
	"errors"
	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
	"io/ioutil"
	"net/http"
	"olympos.io/encoding/edn"
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

func (self *JavaPackage) FromString(s string) error {
	parts := strings.Split(s, "/")
	if len(parts) == 1 && len(s) != 0 {
		self.Artifact = s
		self.Group = s
		return nil
	}
	if len(parts) != 2 || len(parts[0]) == 0 || len(parts[1]) == 0 {
		return errors.New("Package name: " + s + " is not of the form (<group id>/)<artifact id>")
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
	AllVersions []struct {
		Version string `json:"version"`
		Downloads int `json:"downloads"`
	} `json:"recent_versions"`
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
	if err != nil {
		return MPkgInfo{}, err
	}
	versions := []string{}
	for _, v := range pinfo.AllVersions {
		versions = append(versions, v.Version)
	}
	return MPkgInfo {
		Id: JavaPackage{
			pinfo.GroupName,
			pinfo.JarName,
		},
		Description: pinfo.Description,
		LatestVersion: pinfo.Version,
		Versions: versions,
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
	jpkg := JavaPackage{}
	err := jpkg.FromString(string(name))
	if err != nil {
		util.Die("Error while parsing package name", err)
	}
	res, err := GetPackageInfo(jpkg)
	if err != nil {
		util.Die(err.Error())
	}
	return res
}

func Search(name string) []api.PkgInfo {
	cpkg, err := findPackagesClojars(name)
	if err != nil {
		util.Log("Error while querying clojars: " + err.Error())
	}
	mpkg, err := findPackagesMaven(name)
	if err != nil {
		util.Log("Error while querying maven: " + err.Error())
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
	jpkg := JavaPackage{}
	err := jpkg.FromString(string(name))
	if err != nil {
		util.Die("Error while parsing package name", err)
	}
	return name
}

var DefaultEdn = map[edn.Keyword]interface{} {
	edn.Keyword("deps"): map[string]string{},
	edn.Keyword("aliases"): map[edn.Keyword]interface{} {
		edn.Keyword("main"): map[edn.Keyword]interface{} {
			edn.Keyword("main-opts"): []string { "-m", "main" },
		},
	},
}

var DefaultMainClj = `
(ns main)

(defn -main [& args]
  (println "Hello, upm!"))
`

func InitProject() {
	bs, err := edn.MarshalIndent(DefaultEdn, "", "  ")
	if err != nil {
		panic(err)
	}
	ioutil.WriteFile("deps.edn", bs, 0644)

	os.Mkdir("src", 0755)

	if _, err := os.Stat("src/main.clj"); err == nil {
		util.Log("NOTE: src/main.clj already exists, skipping generating the main function, you'll have to set it up yourself")
	} else {
		ioutil.WriteFile("src/main.clj", []byte(DefaultMainClj), 0644)
		util.Log("tools.deps project has been initialized, run with \"clj -Amain\"")
	}
}

func ReadDeps() map[edn.Keyword]interface{} {
	if _, err := os.Stat("deps.edn"); err != nil {
		InitProject()
	}
	var pedn map[edn.Keyword]interface{}
	if bs, err := ioutil.ReadFile("deps.edn"); err == nil {
		err = edn.Unmarshal(bs, &pedn)
		if err != nil {
			util.Die("invalid \"deps.edn\", aborting")
		} else {
			depsMap, ok := pedn[edn.Keyword("deps")].(map[interface{}]interface{})
			if ! ok {
				util.Die("invalid \"deps.edn\", aborting")
			}
			for k, _ := range depsMap {
				v := depsMap[k].(map[interface{}]interface{})
				if _, ok := v[edn.Keyword("mvn/version")]; ! ok {
					util.Die("invalid \"deps.edn\", aborting")
				}
			}
			return pedn
		}
	} else {
		util.Die("error while reading \"deps.edn\", aborting")
	}
	panic("fglng")
}

func WriteDeps(ednMap interface{}) error {
	bs, err := edn.MarshalIndent(ednMap, "", "  ")
	if err != nil {
		return err
	} else {
		ioutil.WriteFile("deps.edn", bs, 0644)
		return nil
	}
	panic("fglng")
}

func addPackages(m map[api.PkgName]api.PkgSpec) {
	pedn := ReadDeps()
	var jpkg JavaPackage
	for name, spec := range m {
		err := jpkg.FromString(string(name))
		if err != nil {
			util.Die(err.Error())
		}
		mPkgInfo, err := GetMPackageInfo(jpkg)
		if err != nil {
			util.Die(err.Error())
		}
		version := string(spec)
		if version == "" {
			util.Log("Using version: " + mPkgInfo.LatestVersion + " for package: " + string(name))
			m[name] = api.PkgSpec(mPkgInfo.LatestVersion)
		} else {
			versionValid := false
			for _, v := range mPkgInfo.Versions {
				if v == version {
					versionValid = true
				}
			}
			if ! versionValid {
				util.Die("No artifact with version: \"" + string(spec) + "\" found for package: \"" + string(name) + "\"")
			}
		}
	}

	depsMap := pedn[edn.Keyword("deps")].(map[interface{}]interface{})
	for name, spec := range m {
		depsMap[edn.Symbol(string(name))] = map[edn.Keyword]string {
			edn.Keyword("mvn/version"): string(spec),
		}
	}

	WriteDeps(pedn)
}

func removePackages(m map[api.PkgName]bool) {
	pedn := ReadDeps()
	var jpkg JavaPackage

	for name, _ := range m {
		err := jpkg.FromString(string(name))
		if err != nil {
			util.Die(err.Error())
		}
	}

	depsMap := pedn[edn.Keyword("deps")].(map[interface{}]interface{})
	for name, _ := range m {
		sname := string(name)
		delete(depsMap, edn.Symbol(sname))
	}

	WriteDeps(pedn)
}

func listSpecfile() map[api.PkgName]api.PkgSpec {
	pedn := ReadDeps()
	depsMap := pedn[edn.Keyword("deps")].(map[interface{}]interface{})

	res := map[api.PkgName]api.PkgSpec{}
	for k, v := range depsMap {
		name := k.(edn.Symbol).String()
		t := v.(map[interface{}]interface{})
		spec := t[edn.Keyword("mvn/version")].(string)
		res[api.PkgName(name)] = api.PkgSpec(spec)
	}
	return res
}

func listLockfile() map[api.PkgName]api.PkgVersion {
	traceEdn := struct {
		Log []struct {
			Lib edn.Symbol `edn:"lib"`
			UsedVersion struct {
				Version string `edn:"mvn/version"`
			} `edn:"use-coord"`
		} `edn:"log"`
	} {}
	bs, err := ioutil.ReadFile("trace.edn")
	if err != nil {
		util.Die("Error while reading trace.edn", err)
	}
	err = edn.Unmarshal(bs, &traceEdn)

	res := map[api.PkgName]api.PkgVersion{}

	for _, entry := range traceEdn.Log {
		res[api.PkgName(string(entry.Lib))] = api.PkgVersion(entry.UsedVersion.Version)
	}

	return res
}

func install() {
	util.Log("installing, this will download the dependencies")
	util.RunCmd([]string {"clj", "--eval", "(System/exit 0)"})
}
