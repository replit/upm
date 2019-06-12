package internal

import (
	"github.com/BurntSushi/toml"
	"os"
)

type pyprojectToml struct {
	Tool struct {
		Poetry struct {
			Dependencies map[string]string
		}
	}
}

type poetryLock struct {
	Package []struct {
		Name string
		Version string
	}
}

var languageBackends = []languageBackend{{
	name: "python-poetry",
	specfile: "pyproject.toml",
	lockfile: "poetry.lock",
	detect: func () bool {
		return true
	},
	add: func (pkgs map[pkgName]pkgSpec) {
		if _, err := os.Stat("pyproject.toml"); os.IsNotExist(err) {
			runCmd([]string{"poetry", "init", "--no-interaction"})
		} else if err != nil {
			die("pyproject.toml: %s", err)
		}
		cmd := []string{"poetry", "add"}
		for name, spec := range pkgs {
			cmd = append(cmd, string(name) + string(spec))
		}
		runCmd(cmd)
	},
	remove: func (pkgs map[pkgName]bool) {
		if _, err := os.Stat("pyproject.toml"); os.IsNotExist(err) {
			return
		} else if err != nil {
			die("pyproject.toml: %s", err)
		}
		cmd := []string{"poetry", "remove"}
		for name, _ := range pkgs {
			cmd = append(cmd, string(name))
		}
		runCmd(cmd)
	},
	lock: func () {
		runCmd([]string{"poetry", "lock"})
	},
	install: func () {
		runCmd([]string{"poetry", "install"})
	},
	listSpecfile: func () map[pkgName]pkgSpec {
		var cfg pyprojectToml
		if _, err := toml.DecodeFile("pyproject.toml", &cfg); err != nil {
			if os.IsNotExist(err) {
				return map[pkgName]pkgSpec{}
			}
			die("%s", err.Error())
		}
		pkgs := map[pkgName]pkgSpec{}
		for nameStr, specStr := range cfg.Tool.Poetry.Dependencies {
			if nameStr == "python" {
				continue
			}

			pkgs[pkgName(nameStr)] = pkgSpec(specStr)
		}
		return pkgs
	},
	listLockfile: func () map[pkgName]pkgVersion {
		var cfg poetryLock
		if _, err := toml.DecodeFile("poetry.lock", &cfg); err != nil {
			if os.IsNotExist(err) {
				return map[pkgName]pkgVersion{}
			}
			die("%s", err.Error())
		}
		pkgs := map[pkgName]pkgVersion{}
		for _, pkgObj := range cfg.Package {
			name := pkgName(pkgObj.Name)
			version := pkgVersion(pkgObj.Version)
			pkgs[name] = version
		}
		return pkgs
	},
}}

func getBackend(language string) languageBackend {
	if language != "" {
		for _, languageBackend := range languageBackends {
			if languageBackend.name == language {
				return languageBackend
			}
		}
	}
	for _, languageBackend := range languageBackends {
		if languageBackend.detect() {
			return languageBackend
		}
	}
	die("could not autodetect a language for your project")
	return languageBackend{}
}

func getBackendNames() []string {
	backendNames := []string{}
	for _, languageBackend := range languageBackends {
		backendNames = append(backendNames, languageBackend.name)
	}
	return backendNames
}
