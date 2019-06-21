package backends

import (
	"fmt"
	"io/ioutil"
	"os"
	"regexp"
	"strings"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

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

var elispBackend = api.LanguageBackend{
	Name:     "elisp-cask",
	Specfile: "Cask",
	Lockfile: "packages.txt",
	Quirks:   api.QuirksNotReproducible,
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
		contentsB, err := ioutil.ReadFile("Cask")
		var contents string
		if os.IsNotExist(err) {
			contents = `(source gnu)
(source melpa)
(source org)
`
		} else if err != nil {
			util.Die("Cask: %s", err)
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
		util.ProgressMsg("write Cask")
		util.TryWriteAtomic("Cask", contentsB)
	},
	Remove: func(pkgs map[api.PkgName]bool) {
		contentsB, err := ioutil.ReadFile("Cask")
		if err != nil {
			util.Die("Cask: %s", err)
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
		util.ProgressMsg("write Cask")
		util.TryWriteAtomic("Cask", contentsB)
	},
	Install: func() {
		util.RunCmd([]string{"cask", "install"})
		outputB := util.GetCmdOutput(
			[]string{"cask", "eval", elispInstallCode},
		)
		util.TryWriteAtomic("packages.txt", outputB)
	},
	ListSpecfile: func() map[api.PkgName]api.PkgSpec {
		outputB := util.GetCmdOutput(
			[]string{"cask", "eval", elispListSpecfileCode},
		)
		pkgs := map[api.PkgName]api.PkgSpec{}
		for _, line := range strings.Split(string(outputB), "\n") {
			if line == "" {
				continue
			}
			fields := strings.SplitN(line, "=", 2)
			if len(fields) != 2 {
				util.Die("unexpected output: %s", line)
			}
			name := api.PkgName(fields[0])
			spec := api.PkgSpec(fields[1])
			pkgs[name] = spec
		}
		return pkgs
	},
	ListLockfile: func() map[api.PkgName]api.PkgVersion {
		contentsB, err := ioutil.ReadFile("packages.txt")
		if err != nil {
			util.Die("packages.txt: %s", err)
		}
		contents := string(contentsB)
		r, err := regexp.Compile(`(.+)=(.+)`)
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
