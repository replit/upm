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
	add: func (pkgSpecs []pkgSpec) {
		if _, err := os.Stat("pyproject.toml"); os.IsNotExist(err) {
			runCmd([]string{"poetry", "init", "--no-interaction"})
		} else if err != nil {
			die("pyproject.toml: %s", err)
		}
		cmd := []string{"poetry", "add"}
		for _, pkgSpec := range pkgSpecs {
			cmd = append(cmd, pkgSpec.pkg + pkgSpec.spec)
		}
		runCmd(cmd)
	},
	listSpecfile: func () []pkgSpec {
		var cfg PyprojectToml
		if _, err := toml.DecodeFile("pyproject.toml", &cfg); err != nil {
			if os.IsNotExist(err) {
				return []pkgSpec{}
			}
			die(err.Error())
		}
		specs := []pkgSpec{}
		for name, spec := range cfg.Tool.Poetry.Dependencies {
			if name == "python" {
				continue
			}
			specs = append(specs, pkgSpec{name, spec})
		}
		return specs
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
