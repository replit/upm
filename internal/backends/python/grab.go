package python

import (
	"context"
	"os"
	"strings"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
	"github.com/smacker/go-tree-sitter/python"
	"gopkg.in/DataDog/dd-trace-go.v1/ddtrace/tracer"
)

var importsQuery = `
(module
  [(import_statement
     name: [(dotted_name) @import
            (aliased_import
              name: (dotted_name) @import)])

   (import_from_statement
     module_name: (dotted_name) @import)]

  .

  (comment)? @pragma)
`

var pyPathGlobs = []string{"*.py"}

var pyIgnorePathSegments = map[string]bool{
	"__pycache__": true,
	"venv":        true,
	".pythonlibs": true,
	".git":        true,
}

var internalModules = map[string]bool{
	"__future__":      true,
	"__main__":        true,
	"_thread":         true,
	"abc":             true,
	"aifc":            true,
	"argparse":        true,
	"array":           true,
	"ast":             true,
	"asyncio":         true,
	"atexit":          true,
	"audioop":         true,
	"base64":          true,
	"bdb":             true,
	"binascii":        true,
	"bisect":          true,
	"builtins":        true,
	"bz2":             true,
	"calendar":        true,
	"cgi":             true,
	"cgitb":           true,
	"chunk":           true,
	"cmath":           true,
	"cmd":             true,
	"code":            true,
	"codecs":          true,
	"codeop":          true,
	"collections":     true,
	"colorsys":        true,
	"compileall":      true,
	"concurrent":      true,
	"configparser":    true,
	"contextlib":      true,
	"contextvars":     true,
	"copy":            true,
	"copyreg":         true,
	"cProfile":        true,
	"crypt":           true,
	"csv":             true,
	"ctypes":          true,
	"curses":          true,
	"dataclasses":     true,
	"datetime":        true,
	"dbm":             true,
	"decimal":         true,
	"difflib":         true,
	"dis":             true,
	"doctest":         true,
	"email":           true,
	"encodings":       true,
	"ensurepip":       true,
	"enum":            true,
	"errno":           true,
	"faulthandler":    true,
	"fcntl":           true,
	"filecmp":         true,
	"fileinput":       true,
	"fnmatch":         true,
	"fractions":       true,
	"ftplib":          true,
	"functools":       true,
	"gc":              true,
	"getopt":          true,
	"getpass":         true,
	"gettext":         true,
	"glob":            true,
	"graphlib":        true,
	"grp":             true,
	"gzip":            true,
	"hashlib":         true,
	"heapq":           true,
	"hmac":            true,
	"html":            true,
	"http":            true,
	"idlelib":         true,
	"imaplib":         true,
	"imghdr":          true,
	"importlib":       true,
	"inspect":         true,
	"io":              true,
	"ipaddress":       true,
	"itertools":       true,
	"json":            true,
	"keyword":         true,
	"lib2to3":         true,
	"linecache":       true,
	"locale":          true,
	"logging":         true,
	"lzma":            true,
	"mailbox":         true,
	"mailcap":         true,
	"marshal":         true,
	"math":            true,
	"mimetypes":       true,
	"mmap":            true,
	"modulefinder":    true,
	"msilib":          true,
	"msicrt":          true,
	"multiprocessing": true,
	"netrc":           true,
	"nis":             true,
	"nntplib":         true,
	"numbers":         true,
	"operator":        true,
	"optparse":        true,
	"os":              true,
	"ossaudiodev":     true,
	"parser":          true,
	"pathlib":         true,
	"pdb":             true,
	"pickle":          true,
	"pickletools":     true,
	"pipes":           true,
	"pkgutil":         true,
	"platform":        true,
	"plistlib":        true,
	"poplib":          true,
	"posix":           true,
	"pprint":          true,
	"profile":         true,
	"pstats":          true,
	"pty":             true,
	"pwd":             true,
	"py_compile":      true,
	"pyclbr":          true,
	"pydoc":           true,
	"queue":           true,
	"quopri":          true,
	"random":          true,
	"re":              true,
	"readline":        true,
	"reprlib":         true,
	"resource":        true,
	"rlcompleter":     true,
	"runpy":           true,
	"sched":           true,
	"secrets":         true,
	"select":          true,
	"selectors":       true,
	"shelve":          true,
	"shlex":           true,
	"shutil":          true,
	"signal":          true,
	"site":            true,
	"sitecustomize":   true,
	"smtplib":         true,
	"sndhdr":          true,
	"socket":          true,
	"socketserver":    true,
	"spwd":            true,
	"sqlite3":         true,
	"ssl":             true,
	"stat":            true,
	"statistics":      true,
	"string":          true,
	"stringprep":      true,
	"struct":          true,
	"subprocess":      true,
	"sunau":           true,
	"symtable":        true,
	"sys":             true,
	"sysconfig":       true,
	"syslog":          true,
	"tabnanny":        true,
	"tarfile":         true,
	"telnetlib":       true,
	"tempfile":        true,
	"termios":         true,
	"test":            true,
	"textwrap":        true,
	"threading":       true,
	"time":            true,
	"timeit":          true,
	"tkinter":         true,
	"token":           true,
	"tokenize":        true,
	"tomllib":         true,
	"trace":           true,
	"traceback":       true,
	"tracemalloc":     true,
	"tty":             true,
	"turtle":          true,
	"turtledemo":      true,
	"types":           true,
	"typing":          true,
	"unicodedata":     true,
	"unittest":        true,
	"urllib":          true,
	"usercustomize":   true,
	"uu":              true,
	"uuid":            true,
	"venv":            true,
	"warnings":        true,
	"wave":            true,
	"weakref":         true,
	"webbrowser":      true,
	"winreg":          true,
	"winsound":        true,
	"wsgiref":         true,
	"xdrlib":          true,
	"xml":             true,
	"xmlrpc":          true,
	"zipapp":          true,
	"zipfile":         true,
	"zipimport":       true,
	"zlib":            true,
	"zoneinfo":        true,
}

func guess(ctx context.Context, python string) (map[api.PkgName]bool, bool) {
	span, ctx := tracer.StartSpanFromContext(ctx, "python.grab.guess")
	defer span.Finish()
	cwd, err := os.Getwd()
	if err != nil {
		util.Die("couldn't get working directory: %s", err)
	}

	foundImportPaths, err := findImports(ctx, cwd)
	if err != nil {
		util.Die("couldn't guess imports: %s", err)
	}

	return filterImports(ctx, foundImportPaths)
}

func findImports(ctx context.Context, dir string) (map[string]bool, error) {
	span, ctx := tracer.StartSpanFromContext(ctx, "python.grab.findImports")
	defer span.Finish()
	py := python.GetLanguage()
	pkgs, err := util.GuessWithTreeSitter(ctx, dir, py, importsQuery, pyPathGlobs, pyIgnorePathSegments)

	if err != nil {
		return nil, err
	}

	foundImportPaths := map[string]bool{}
	for _, pkg := range pkgs {
		foundImportPaths[pkg] = true
	}

	return foundImportPaths, nil
}

func filterImports(ctx context.Context, foundPkgs map[string]bool) (map[api.PkgName]bool, bool) {
	//nolint:ineffassign,wastedassign,staticcheck
	span, ctx := tracer.StartSpanFromContext(ctx, "python.grab.filterImports")
	defer span.Finish()
	// filter out internal modules
	for pkg := range foundPkgs {
		mod := getTopLevelModuleName(pkg)
		if internalModules[mod] {
			delete(foundPkgs, pkg)
		}
	}

	pypiMap, err := NewPypiMap()
	if err != nil {
		util.Die(err.Error())
	}
	defer pypiMap.Close()

	pkgs := map[api.PkgName]bool{}

	for fullModname := range foundPkgs {
		// try and look it up in Pypi
		var pkg string
		var ok bool

		modNameParts := strings.Split(fullModname, ".")
		for len(modNameParts) > 0 {
			testModName := strings.Join(modNameParts, ".")

			// test overrides
			pkg, ok = moduleToPypiPackageOverride[testModName]
			if ok {
				break
			}

			// test pypi
			pkg, ok = pypiMap.ModuleToPackage(testModName)
			if ok {
				break
			}

			// loop with everything except the deepest submodule
			modNameParts = modNameParts[:len(modNameParts)-1]
		}

		if ok {
			name := api.PkgName(pkg)
			pkgs[normalizePackageName(name)] = true
		}
	}

	return pkgs, true
}
