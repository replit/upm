package internal

import (
	"fmt"
	"github.com/BurntSushi/toml"
	"io/ioutil"
	"os"
	"regexp"
	"strings"
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

const elispInstallCode = `
(dolist (dir load-path)
  (when (string-match "elpa/\\(.+\\)-\\([^-]+\\)" dir)
    (princ (format "%s=%s\n"
                   (match-string 1 dir)
                   (match-string 2 dir)))))
`

const elispListSpecfileCode = `
(let* ((bundle (cask-cli--bundle))
       (deps (append (cask-runtime-dependencies bundle)
                     (cask-development-dependencies bundle))))
  (dolist (d deps)
    (let ((fetcher (cask-dependency-fetcher d))
          (url (cask-dependency-url d))
          (files (cask-dependency-files d))
          (ref (cask-dependency-ref d))
          (branch (cask-dependency-branch d)))
      (princ (format "%S=%s%s%s%s\n"
                     (cask-dependency-name d)
                     (if fetcher (format "%S %S" fetcher url) "")
                     (if files (format ":files %S" files) "")
                     (if ref (format ":ref %S" ref) "")
                     (if branch (format ":branch %S" branch) ""))))))
`

var languageBackends = []languageBackend{{
	name: "python-poetry",
	specfile: "pyproject.toml",
	lockfile: "poetry.lock",
	quirks: quirksNone,
	detect: func () bool {
		return false
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
}, {
	name: "elisp-cask",
	specfile: "Cask",
	lockfile: "packages.txt",
	quirks: quirksNotReproducible,
	detect: func () bool {
		return false
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
		contentsB, err := ioutil.ReadFile("Cask")
		var contents string
		if os.IsNotExist(err) {
			contents = `(source gnu)
(source melpa)
(source org)
`
		} else if err != nil {
			die("Cask: %s", err)
		} else {
			contents = string(contentsB)
		}

		// Ensure newline before the stuff we add, for
		// readability.
		if len(contents) > 0 && contents[len(contents)-1] != '\n' {
			contents += "\n"
		}

		for name, spec := range pkgs {
			contents += fmt.Sprintf(`(depends-on "%s"`, name)
			if spec != "" {
				contents += fmt.Sprintf(" %s", spec)
			}
			contents += fmt.Sprint(")\n")
		}

		contentsB = []byte(contents)
		progressMsg("write Cask")
		tryWriteAtomic("Cask", contentsB)
	},
	remove: func (pkgs map[pkgName]bool) {
		contentsB, err := ioutil.ReadFile("Cask")
		if err != nil {
			die("Cask: %s", err)
		}
		contents := string(contentsB)

		for name, _ := range pkgs {
			r, err := regexp.Compile(
				fmt.Sprintf(
					`(?m)^ *\(depends-on +"%s".*\)\n?$`,
					regexp.QuoteMeta(string(name)),
				),
			)
			if err != nil {
				panic(err)
			}
			contents = r.ReplaceAllLiteralString(contents, "")
		}

		contentsB = []byte(contents)
		progressMsg("write Cask")
		tryWriteAtomic("Cask", contentsB)
	},
	install: func () {
		runCmd([]string{"cask", "install"})
		outputB := getCmdOutput(
			[]string{"cask", "eval", elispInstallCode},
		)
		tryWriteAtomic("packages.txt", outputB)
	},
	listSpecfile: func () map[pkgName]pkgSpec {
		outputB := getCmdOutput(
			[]string{"cask", "eval", elispListSpecfileCode},
		)
		pkgs := map[pkgName]pkgSpec{}
		for _, line := range strings.Split(string(outputB), "\n") {
			if line == "" {
				continue
			}
			fields := strings.SplitN(line, "=", 2)
			if len(fields) != 2 {
				die("unexpected output: %s", line)
			}
			name := pkgName(fields[0])
			spec := pkgSpec(fields[1])
			pkgs[name] = spec
		}
		return pkgs
	},
	listLockfile: func () map[pkgName]pkgVersion {
		contentsB, err := ioutil.ReadFile("packages.txt")
		if err != nil {
			die("packages.txt: %s", err)
		}
		contents := string(contentsB)
		r, err := regexp.Compile(`(.+)=(.+)`)
		if err != nil {
			panic(err)
		}
		pkgs := map[pkgName]pkgVersion{}
		for _, match := range r.FindAllStringSubmatch(contents, -1) {
			name := pkgName(match[1])
			version := pkgVersion(match[2])
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
			// The lock method should be unimplemented if
			// and only if builds are not reproducible.
			((b.lock == nil) != quirksIsNotReproducible(b)) ||
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
	if language != "" {
		die("no such language: %s", language)
	} else {
		die("could not autodetect a language for your project")
	}
	return languageBackend{}
}

func getBackendNames() []string {
	backendNames := []string{}
	for _, languageBackend := range languageBackends {
		backendNames = append(backendNames, languageBackend.name)
	}
	return backendNames
}
