package backends

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"regexp"
	"strings"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

const elispSearchInfoCode = `
(require 'cl-lib)
(require 'json)
(require 'map)
(require 'package)
(require 'subr-x)

;; Give MELPA priority as it has more up-to-date versions.
(setq package-archives '((melpa . "https://melpa.org/packages/")
                         (gnu . "https://elpa.gnu.org/packages/")
                         (org . "https://orgmode.org/elpa/")))

(defun upm-convert-package-desc (desc)
  "Convert package descriptor DESC to alist.
The JSON representation of the alist can be unmarshaled directly
into a PkgInfo struct in Go."
  (let ((extras (package-desc-extras desc)))
    ~((name . ,(symbol-name (package-desc-name desc)))
      (description . ,(package-desc-summary desc))
      (version . ,(package-version-join (package-desc-version desc)))
      (homepageURL . ,(alist-get :url extras))
      (author . ,(when-let ((mnt (alist-get :maintainer extras)))
                   (let ((parts nil))
                     (when-let ((email (cdr mnt)))
                       (push (format "<%s>" email) parts))
                     (when-let ((name (car mnt)))
                       (push name parts))
                     (when parts
                       (string-join parts " ")))))
      (dependencies . ,(cl-remove-if
                        (lambda (dep)
                          (string= dep "emacs"))
                        (mapcar
                         (lambda (link)
                           (symbol-name (car link)))
                         (package-desc-reqs desc)))))))

(defun upm-package-info (package)
  "Given PACKAGE string, return alist of metadata for it, or nil."
  (when-let ((descs (alist-get (intern package) package-archive-contents)))
    ;; If the same package is available from multiple repositories,
    ;; prefer the one from the repository which is listed first in
    ;; ~package-archives' (which package.el puts at the *end* of the
    ;; ~package-desc' list).
    (upm-convert-package-desc
     (car (last descs)))))

(defvar upm-num-archives-fetched 0
  "Number of package.el archives which have been fetched so far.")

(defun upm-download-callback (status archive-id action arg)
  "Callback for ~url-retrieve' on a package.el archive.
ARCHIVE-ID is a symbol (e.g. ~gnu', ~melpa', ...)."
  (cl-loop for (event data) on status by #'cddr
           do (when (eq event :error)
                (signal (car data) (cdr data))))
  (let* ((archives-dir (expand-file-name "archives" package-user-dir))
         (archive-dir (expand-file-name
                       (symbol-name archive-id) archives-dir))
         (json-encoding-pretty-print t))
    (make-directory archive-dir 'parents)
    (delete-region (point-min) url-http-end-of-headers)
    (write-file (expand-file-name "archive-contents" archive-dir))
    ;; No race condition, Elisp does not have preemptive
    ;; multithreading.
    (when (>= (cl-incf upm-num-archives-fetched) (length package-archives))
      (package-read-all-archive-contents)
      (pcase action
        ("search"
         (let ((queries (mapcar
                         #'regexp-quote (split-string arg nil 'omit-nulls))))
           (thread-last package-archive-contents
             (map-keys)
             (mapcar #'symbol-name)
             (cl-remove-if-not (lambda (package)
                                 (cl-every (lambda (query)
                                             (string-match-p query package))
                                           queries)))
             (funcall (lambda (packages)
                        (cl-sort packages #'< :key #'length)))
             (mapcar #'upm-package-info)
             (json-encode)
             (princ))
           (terpri)))
        ("info"
         (princ
          (json-encode (upm-package-info arg)))
         (terpri))
        (_ (error "No such action: %S" action))))))

(cl-destructuring-bind (dir action arg) command-line-args-left
  (setq command-line-args-left nil)
  (setq package-user-dir dir)
  (dolist (link package-archives)
    (url-retrieve
     (concat (cdr link) "archive-contents")
     #'upm-download-callback
     (list (car link) action arg)
     'silent)))

(while (< upm-num-archives-fetched (length package-archives))
  ;; 50ms is small enough to be imperceptible to the user.
  (accept-process-output nil 0.05))
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

var elispBackend = api.LanguageBackend{
	Name:     "elisp-cask",
	Specfile: "Cask",
	Lockfile: "packages.txt",
	Quirks:   api.QuirksNotReproducible,
	Detect: func() bool {
		return false
	},
	Search: func(queries []string) []api.PkgInfo {
		tmpdir, err := ioutil.TempDir("", "elpa")
		if err != nil {
			util.Die("%s", err)
		}
		defer os.RemoveAll(tmpdir)

		code := fmt.Sprintf("(progn %s)", elispSearchInfoCode)
		code = strings.Replace(code, "~", "`", -1)
		outputB := util.GetCmdOutput([]string{
			"emacs", "-Q", "--batch", "--eval", code,
			tmpdir, "search", strings.Join(queries, " "),
		})
		var results []api.PkgInfo
		if err := json.Unmarshal(outputB, &results); err != nil {
			util.Die("%s", err)
		}
		return results
	},
	Info: func(name api.PkgName) *api.PkgInfo {
		tmpdir, err := ioutil.TempDir("", "elpa")
		if err != nil {
			util.Die("%s", err)
		}
		defer os.RemoveAll(tmpdir)

		code := fmt.Sprintf("(progn %s)", elispSearchInfoCode)
		code = strings.Replace(code, "~", "`", -1)
		outputB := util.GetCmdOutput([]string{
			"emacs", "-Q", "--batch", "--eval", code,
			tmpdir, "info", string(name),
		})
		var info api.PkgInfo
		if err := json.Unmarshal(outputB, &info); err != nil {
			util.Die("%s", err)
		}
		if info.Name == "" {
			return nil
		}
		return &info
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
