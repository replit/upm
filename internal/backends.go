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
	search: func ([]string) []pkgInfo {
		notImplemented()
		return nil
	},
	info: func (pkgName) pkgInfo {
		notImplemented()
		return pkgInfo{}
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
		// Unfortunately, this doesn't necessarily uninstall
		// packages that have been removed from the lockfile,
		// which happens for example if 'poetry remove' is
		// interrupted. See
		// <https://github.com/sdispater/poetry/issues/648>.
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
	guess: func () map[pkgName]bool {
		notImplemented()
		return nil
	},
}}

// Keep up to date with languageBackend in types.go
func checkBackends() {
	for _, b := range languageBackends {
		if (b.name == "" ||
			b.specfile == "" ||
			b.lockfile == "" ||
			b.detect == nil ||
			b.search == nil ||
			b.info == nil ||
			b.add == nil ||
			b.remove == nil ||
			b.lock == nil ||
			b.install == nil ||
			b.listSpecfile == nil ||
			b.listLockfile == nil ||
			b.guess == nil) {
			panicf("language backend %s is incomplete", b.name)
		}
	}
}

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
