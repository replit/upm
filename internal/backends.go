package internal

import (
	"github.com/BurntSushi/toml"
	"os"
)

type PyprojectToml struct {
	Tool struct {
		Poetry struct {
			Dependencies map[string]string
		}
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
	listSpecfile: func () map[pkgName]pkgSpec {
		var cfg PyprojectToml
		if _, err := toml.DecodeFile("pyproject.toml", &cfg); err != nil {
			if os.IsNotExist(err) {
				return map[pkgName]pkgSpec{}
			}
			die(err.Error())
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
