package internal

var languageBackends = []languageBackend{{
	name: "python-poetry",
	specfile: "pyproject.toml",
	lockfile: "poetry.lock",
	detect: func () bool {
		return true
	},
	add: func (pkgSpecs []pkgSpec) {
		cmd := []string{"poetry", "add"}
		for _, pkgSpec := range pkgSpecs {
			cmd = append(cmd, pkgSpec.pkg + pkgSpec.spec)
		}
		runCmd(cmd)
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
