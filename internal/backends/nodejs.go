package backends

import (
	"encoding/json"
	"io/ioutil"
	"regexp"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

type packageJson struct {
	Dependencies    map[string]string `json:"dependencies"`
	DevDependencies map[string]string `json:"devDependencies"`
}

var nodejsBackend = api.LanguageBackend{
	Name:     "nodejs-yarn",
	Specfile: "package.json",
	Lockfile: "yarn.lock",
	Quirks:   api.QuirksNone,
	Detect: func() bool {
		return false
	},
	Search: func([]string) []api.PkgInfo {
		util.NotImplemented()
		return nil
	},
	Info: func(api.PkgName) *api.PkgInfo {
		util.NotImplemented()
		return &api.PkgInfo{}
	},
	Add: func(pkgs map[api.PkgName]api.PkgSpec) {
		cmd := []string{"yarn", "add"}
		for name, spec := range pkgs {
			cmd = append(cmd, string(name)+"@"+string(spec))
		}
		util.RunCmd(cmd)
	},
	Remove: func(pkgs map[api.PkgName]bool) {
		cmd := []string{"yarn", "remove"}
		for name, _ := range pkgs {
			cmd = append(cmd, string(name))
		}
		util.RunCmd(cmd)
	},
	Lock: func() {
		util.RunCmd([]string{"yarn", "upgrade"})
	},
	Install: func() {
		util.RunCmd([]string{"yarn", "install"})
	},
	ListSpecfile: func() map[api.PkgName]api.PkgSpec {
		contentsB, err := ioutil.ReadFile("package.json")
		if err != nil {
			util.Die("package.json: %s", err)
		}
		var cfg packageJson
		if err := json.Unmarshal(contentsB, &cfg); err != nil {
			util.Die("package.json: %s", err)
		}
		pkgs := map[api.PkgName]api.PkgSpec{}
		for nameStr, specStr := range cfg.Dependencies {
			pkgs[api.PkgName(nameStr)] = api.PkgSpec(specStr)
		}
		for nameStr, specStr := range cfg.DevDependencies {
			pkgs[api.PkgName(nameStr)] = api.PkgSpec(specStr)
		}
		return pkgs
	},
	ListLockfile: func() map[api.PkgName]api.PkgVersion {
		contentsB, err := ioutil.ReadFile("yarn.lock")
		if err != nil {
			util.Die("yarn.lock: %s", err)
		}
		contents := string(contentsB)
		r, err := regexp.Compile(`(?m)^"?([^@ \n]+).+:\n  version "(.+)"$`)
		if err != nil {
			panic(err)
		}
		pkgs := map[api.PkgName]api.PkgVersion{}
		for _, match := range r.FindAllStringSubmatch(contents, -1) {
			name := api.PkgName(match[1])
			version := api.PkgVersion(match[2])
			pkgs[name] = version
		}
		return pkgs
	},
	Guess: func() map[api.PkgName]bool {
		util.NotImplemented()
		return nil
	},
}
