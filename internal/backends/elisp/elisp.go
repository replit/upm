// Package elisp provides a backend for Emacs Lisp using Cask.
package elisp

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

// elispPatterns is the FilenamePatterns value for ElispBackend.
var elispPatterns = []string{"*.el"}

// ElispBackend is the UPM language backend for Emacs Lisp using Cask.
var ElispBackend = api.LanguageBackend{
	Name:             "elisp-cask",
	Specfile:         "Cask",
	Lockfile:         "packages.txt",
	FilenamePatterns: elispPatterns,
	Quirks:           api.QuirksNotReproducible,
	GetPackageDir: func() string {
		return ".cask"
	},
	Search: func(query string) []api.PkgInfo {
		tmpdir, err := ioutil.TempDir("", "elpa")
		if err != nil {
			util.Die("%s", err)
		}
		defer os.RemoveAll(tmpdir)

		// Run script with lexical binding (any header comment
		// in the script would not be respected, so we have to
		// do it this way).
		code := fmt.Sprintf(
			"(eval '(progn %s) t)", util.GetResource("/elisp/elpa-search.el"),
		)
		code = strings.Replace(code, "~", "`", -1)
		outputB := util.GetCmdOutput([]string{
			"emacs", "-Q", "--batch", "--eval", code,
			tmpdir, "search", query,
		})
		var results []api.PkgInfo
		if err := json.Unmarshal(outputB, &results); err != nil {
			util.Die("%s", err)
		}
		return results
	},
	Info: func(name api.PkgName) api.PkgInfo {
		tmpdir, err := ioutil.TempDir("", "elpa")
		if err != nil {
			util.Die("%s", err)
		}
		defer os.RemoveAll(tmpdir)

		// Run script with lexical binding (any header comment
		// in the script would not be respected, so we have to
		// do it this way).
		code := fmt.Sprintf(
			"(eval '(progn %s) t)", util.GetResource("/elisp/elpa-search.el"),
		)
		code = strings.Replace(code, "~", "`", -1)
		outputB := util.GetCmdOutput([]string{
			"emacs", "-Q", "--batch", "--eval", code,
			tmpdir, "info", string(name),
		})
		var info api.PkgInfo
		if err := json.Unmarshal(outputB, &info); err != nil {
			util.Die("%s", err)
		}
		return info
	},
	Add: func(pkgs map[api.PkgName]api.PkgSpec) {
		contentsB, err := ioutil.ReadFile("Cask")
		var contents string
		if os.IsNotExist(err) {
			contents = `(source melpa)
(source gnu)
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
			contents = regexp.MustCompile(
				fmt.Sprintf(
					`(?m)^ *\(depends-on +"%s".*\)\n?$`,
					regexp.QuoteMeta(string(name)),
				),
			).ReplaceAllLiteralString(contents, "")
		}

		contentsB = []byte(contents)
		util.ProgressMsg("write Cask")
		util.TryWriteAtomic("Cask", contentsB)
	},
	Install: func() {
		util.RunCmd([]string{"cask", "install"})
		outputB := util.GetCmdOutput(
			[]string{"cask", "eval", util.GetResource(
				"/elisp/cask-list-installed.el",
			)},
		)
		util.ProgressMsg("write packages.txt")
		util.TryWriteAtomic("packages.txt", outputB)
	},
	ListSpecfile: func() map[api.PkgName]api.PkgSpec {
		outputB := util.GetCmdOutput(
			[]string{"cask", "eval", util.GetResource(
				"/elisp/cask-list-specfile.el",
			)},
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
		r := regexp.MustCompile(`(.+)=(.+)`)
		pkgs := map[api.PkgName]api.PkgVersion{}
		for _, match := range r.FindAllStringSubmatch(contents, -1) {
			name := api.PkgName(match[1])
			version := api.PkgVersion(match[2])
			pkgs[name] = version
		}
		return pkgs
	},
	GuessRegexps: util.Regexps([]string{
		`\(\s*require\s*'\s*([^)[:space:]]+)[^)]*\)`,
	}),
	Guess: func() (map[api.PkgName]bool, bool) {
		r := regexp.MustCompile(
			`\(\s*require\s*'\s*([^)[:space:]]+)[^)]*\)`,
		)
		required := map[string]bool{}
		for _, match := range util.SearchRecursive(r, elispPatterns) {
			required[match[1]] = true
		}

		if len(required) == 0 {
			return map[api.PkgName]bool{}, true
		}

		r = regexp.MustCompile(
			`\(\s*provide\s*'\s*([^)[:space:]]+)[^)]*\)`,
		)
		provided := map[string]bool{}
		for _, match := range util.SearchRecursive(r, elispPatterns) {
			provided[match[1]] = true
		}

		tempdir, err := ioutil.TempDir("", "epkgs")
		if err != nil {
			util.Die("%s", err)
		}
		defer os.RemoveAll(tempdir)

		url := "https://github.com/emacsmirror/epkgs/raw/master/epkg.sqlite"
		epkgs := filepath.Join(tempdir, "epkgs.sqlite")
		util.DownloadFile(epkgs, url)

		clauses := []string{}
		for feature := range required {
			if strings.ContainsAny(feature, `\'`) {
				continue
			}
			if provided[feature] {
				continue
			}
			clauses = append(clauses, fmt.Sprintf("feature = '%s'", feature))
		}
		if len(clauses) == 0 {
			return map[api.PkgName]bool{}, true
		}
		where := strings.Join(clauses, " OR ")
		query := fmt.Sprintf("SELECT package FROM provided PR WHERE (%s) "+
			"AND NOT EXISTS (SELECT 1 FROM builtin_libraries B "+
			"WHERE PR.feature = B.feature) "+
			"AND NOT EXISTS (SELECT 1 FROM packages PK "+
			"WHERE PR.package = PK.name AND PK.class = 'builtin');",
			where,
		)
		output := string(util.GetCmdOutput([]string{"sqlite3", epkgs, query}))

		r = regexp.MustCompile(`"(.+?)"`)
		names := map[api.PkgName]bool{}
		for _, match := range r.FindAllStringSubmatch(output, -1) {
			names[api.PkgName(match[1])] = true
		}
		return names, true
	},
}
