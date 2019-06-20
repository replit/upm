package backends

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"regexp"
	"strings"

	"github.com/BurntSushi/toml"
	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

type pypiXMLRPCEntry struct {
	Name    string `json:"name"`
	Summary string `json:"summary"`
	Version string `json:"version"`
}

type pypiXMLRPCInfo struct {
	Author       string   `json:"author"`
	AuthorEmail  string   `json:"author_email"`
	HomePage     string   `json:"home_page"`
	License      string   `json:"license"`
	Name         string   `json:"name"`
	ProjectURL   []string `json:"project_url"`
	RequiresDist []string `json:"requires_dist"`
	Summary      string   `json:"summary"`
	Version      string   `json:"version"`
}

type pyprojectToml struct {
	Tool struct {
		Poetry struct {
			Dependencies    map[string]string `json:"dependencies"`
			DevDependencies map[string]string `json:"dev-dependencies"`
		} `json:"poetry"`
	} `json:"tool"`
}

type poetryLock struct {
	Package []struct {
		Name    string `json:"name"`
		Version string `json:"version"`
	} `json:"package"`
}

type packageJson struct {
	Dependencies    map[string]string `json:"dependencies"`
	DevDependencies map[string]string `json:"devDependencies"`
}

const pythonSearchCode = `
import json
import sys
import xmlrpc.client

query = sys.argv[1]
pypi = xmlrpc.client.ServerProxy("https://pypi.org/pypi")
results = pypi.search({"name": query})
json.dump(results, sys.stdout, indent=2)
print()
`

const pythonInfoCode = `
import json
import sys
import xmlrpc.client

package = sys.argv[1]
pypi = xmlrpc.client.ServerProxy("https://pypi.org/pypi")
releases = pypi.package_releases(package)
if not releases:
    print("{}")
    sys.exit(0)
release, = releases
info = pypi.release_data(package, release)
json.dump(info, sys.stdout, indent=2)
print()
`

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

var languageBackends = []api.LanguageBackend{{
	Name:     "python-poetry",
	Specfile: "pyproject.toml",
	Lockfile: "poetry.lock",
	Quirks:   api.QuirksNone,
	Detect: func() bool {
		return false
	},
	Search: func(queries []string) []api.PkgInfo {
		query := strings.Join(queries, " ")
		outputB := util.GetCmdOutput([]string{
			"python3", "-c", pythonSearchCode, query,
		})
		var outputJson []pypiXMLRPCEntry
		if err := json.Unmarshal(outputB, &outputJson); err != nil {
			util.Die("PyPI response: %s", err)
		}
		results := []api.PkgInfo{}
		for i := range outputJson {
			results = append(results, api.PkgInfo{
				Name:        outputJson[i].Name,
				Description: outputJson[i].Summary,
				Version:     outputJson[i].Version,
			})
		}
		return results
	},
	Info: func(name api.PkgName) *api.PkgInfo {
		outputB := util.GetCmdOutput([]string{
			"python3", "-c", pythonInfoCode, string(name),
		})
		var output pypiXMLRPCInfo
		if err := json.Unmarshal(outputB, &output); err != nil {
			util.Die("PyPI response: %s", err)
		}
		if output.Name == "" {
			return nil
		}
		info := &api.PkgInfo{
			Name:        output.Name,
			Description: output.Summary,
			Version:     output.Version,
			HomepageURL: output.HomePage,
			License:     output.License,
		}
		for _, line := range output.ProjectURL {
			fields := strings.SplitN(line, ", ", 2)
			if len(fields) != 2 {
				continue
			}

			name := fields[0]
			url := fields[1]

			matched, err := regexp.MatchString(`(?i)doc`, name)
			if err != nil {
				panic(err)
			}
			if matched {
				info.DocumentationURL = url
				continue
			}

			matched, err = regexp.MatchString(`(?i)code`, name)
			if err != nil {
				panic(err)
			}
			if matched {
				info.SourceCodeURL = url
				continue
			}

			matched, err = regexp.MatchString(`(?i)track`, name)
			if err != nil {
				panic(err)
			}
			if matched {
				info.BugTrackerURL = url
				continue
			}
		}

		authorParts := []string{}
		if output.Author != "" {
			authorParts = append(authorParts, output.Author)
		}
		if output.AuthorEmail != "" {
			authorParts = append(
				authorParts, fmt.Sprintf(
					"<%s>", output.AuthorEmail,
				),
			)
		}
		info.Author = strings.Join(authorParts, " ")

		deps := []string{}
		for _, line := range output.RequiresDist {
			if strings.Contains(line, "extra ==") {
				continue
			}

			deps = append(deps, strings.Fields(line)[0])
		}
		info.Dependencies = deps

		return info
	},
	Add: func(pkgs map[api.PkgName]api.PkgSpec) {
		if !util.FileExists("pyproject.toml") {
			util.RunCmd([]string{"poetry", "init", "--no-interaction"})
		}
		cmd := []string{"poetry", "add"}
		for name, spec := range pkgs {
			cmd = append(cmd, string(name)+string(spec))
		}
		util.RunCmd(cmd)
	},
	Remove: func(pkgs map[api.PkgName]bool) {
		cmd := []string{"poetry", "remove"}
		for name, _ := range pkgs {
			cmd = append(cmd, string(name))
		}
		util.RunCmd(cmd)
	},
	Lock: func() {
		util.RunCmd([]string{"poetry", "lock"})
	},
	Install: func() {
		// Unfortunately, this doesn't necessarily uninstall
		// packages that have been removed from the lockfile,
		// which happens for example if 'poetry remove' is
		// interrupted. See
		// <https://github.com/sdispater/poetry/issues/648>.
		util.RunCmd([]string{"poetry", "install"})
	},
	ListSpecfile: func() map[api.PkgName]api.PkgSpec {
		var cfg pyprojectToml
		if _, err := toml.DecodeFile("pyproject.toml", &cfg); err != nil {
			util.Die("%s", err.Error())
		}
		pkgs := map[api.PkgName]api.PkgSpec{}
		for nameStr, specStr := range cfg.Tool.Poetry.Dependencies {
			if nameStr == "python" {
				continue
			}

			pkgs[api.PkgName(nameStr)] = api.PkgSpec(specStr)
		}
		for nameStr, specStr := range cfg.Tool.Poetry.DevDependencies {
			if nameStr == "python" {
				continue
			}

			pkgs[api.PkgName(nameStr)] = api.PkgSpec(specStr)
		}
		return pkgs
	},
	ListLockfile: func() map[api.PkgName]api.PkgVersion {
		var cfg poetryLock
		if _, err := toml.DecodeFile("poetry.lock", &cfg); err != nil {
			util.Die("%s", err.Error())
		}
		pkgs := map[api.PkgName]api.PkgVersion{}
		for _, pkgObj := range cfg.Package {
			name := api.PkgName(pkgObj.Name)
			version := api.PkgVersion(pkgObj.Version)
			pkgs[name] = version
		}
		return pkgs
	},
	Guess: func() map[api.PkgName]bool {
		util.NotImplemented()
		return nil
	},
}, {
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
}, {
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
}}

// Keep up to date with languageBackend in types.go
func CheckAll() {
	for _, b := range languageBackends {
		if b.Name == "" ||
			b.Specfile == "" ||
			b.Lockfile == "" ||
			b.Detect == nil ||
			b.Search == nil ||
			b.Info == nil ||
			b.Add == nil ||
			b.Remove == nil ||
			// The lock method should be unimplemented if
			// and only if builds are not reproducible.
			((b.Lock == nil) != api.QuirksIsNotReproducible(b)) ||
			b.Install == nil ||
			b.ListSpecfile == nil ||
			b.ListLockfile == nil ||
			b.Guess == nil {
			util.Panicf("language backend %s is incomplete", b.Name)
		}
	}
}

func GetBackend(language string) api.LanguageBackend {
	if language != "" {
		for _, languageBackend := range languageBackends {
			if languageBackend.Name == language {
				return languageBackend
			}
		}
	}
	for _, languageBackend := range languageBackends {
		if languageBackend.Detect() {
			return languageBackend
		}
	}
	if language != "" {
		util.Die("no such language: %s", language)
	} else {
		util.Die("could not autodetect a language for your project")
	}
	return api.LanguageBackend{}
}

func GetBackendNames() []string {
	backendNames := []string{}
	for _, languageBackend := range languageBackends {
		backendNames = append(backendNames, languageBackend.Name)
	}
	return backendNames
}
