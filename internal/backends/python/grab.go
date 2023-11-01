package python

import (
	"strings"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
	"github.com/smacker/go-tree-sitter/python"
)

var importsQuery = `
(import_statement
  name: [(dotted_name) @import
         (aliased_import
           name: (dotted_name) @import)])

(import_from_statement
  module_name: (dotted_name) @import)
`

var pyPathGlobs = []string{"**/*.py"}

var pyIgnoreGlobs = []string{
	"**/__pycache__/**",
	"**/venv/**",
	"**/.pythonlibs/**",
}

var internalModules = []string{
	"__future__",
	"__main__",
	"_thread",
	"abc",
	"aifc",
	"argparse",
	"array",
	"ast",
	"asyncio",
	"atexit",
	"audioop",
	"base64",
	"bdb",
	"binascii",
	"bisect",
	"builtins",
	"bz2",
	"calendar",
	"cgi",
	"cgitb",
	"chunk",
	"cmath",
	"cmd",
	"code",
	"codecs",
	"codeop",
	"collections",
	"colorsys",
	"compileall",
	"concurrent",
	"configparser",
	"contextlib",
	"contextvars",
	"copy",
	"copyreg",
	"cProfile",
	"crypt",
	"csv",
	"ctypes",
	"curses",
	"dataclasses",
	"datetime",
	"dbm",
	"decimal",
	"difflib",
	"dis",
	"doctest",
	"email",
	"encodings",
	"ensurepip",
	"enum",
	"errno",
	"faulthandler",
	"fcntl",
	"filecmp",
	"fileinput",
	"fnmatch",
	"fractions",
	"ftplib",
	"functools",
	"gc",
	"getopt",
	"getpass",
	"gettext",
	"glob",
	"graphlib",
	"grp",
	"gzip",
	"hashlib",
	"heapq",
	"hmac",
	"html",
	"http",
	"idlelib",
	"imaplib",
	"imghdr",
	"importlib",
	"inspect",
	"io",
	"ipaddress",
	"itertools",
	"json",
	"keyword",
	"lib2to3",
	"linecache",
	"locale",
	"logging",
	"lzma",
	"mailbox",
	"mailcap",
	"marshal",
	"math",
	"mimetypes",
	"mmap",
	"modulefinder",
	"msilib",
	"msicrt",
	"multiprocessing",
	"netrc",
	"nis",
	"nntplib",
	"numbers",
	"operator",
	"optparse",
	"os",
	"ossaudiodev",
	"parser",
	"pathlib",
	"pdb",
	"pickle",
	"pickletools",
	"pipes",
	"pkgutil",
	"platform",
	"plistlib",
	"poplib",
	"posix",
	"pprint",
	"profile",
	"pstats",
	"pty",
	"pwd",
	"py_compile",
	"pyclbr",
	"pydoc",
	"queue",
	"quopri",
	"random",
	"re",
	"readline",
	"reprlib",
	"resource",
	"rlcompleter",
	"runpy",
	"sched",
	"secrets",
	"select",
	"selectors",
	"shelve",
	"shlex",
	"shutil",
	"signal",
	"site",
	"sitecustomize",
	"smtplib",
	"sndhdr",
	"socket",
	"socketserver",
	"spwd",
	"sqlite3",
	"ssl",
	"stat",
	"statistics",
	"string",
	"stringprep",
	"struct",
	"subprocess",
	"sunau",
	"symtable",
	"sys",
	"sysconfig",
	"syslog",
	"tabnanny",
	"tarfile",
	"telnetlib",
	"tempfile",
	"termios",
	"test",
	"textwrap",
	"threading",
	"time",
	"timeit",
	"tkinter",
	"token",
	"tokenize",
	"tomllib",
	"trace",
	"traceback",
	"tracemalloc",
	"tty",
	"turtle",
	"turtledemo",
	"types",
	"typing",
	"unicodedata",
	"unittest",
	"urllib",
	"usercustomize",
	"uu",
	"uuid",
	"venv",
	"warnings",
	"wave",
	"weakref",
	"webbrowser",
	"winreg",
	"winsound",
	"wsgiref",
	"xdrlib",
	"xml",
	"xmlrpc",
	"zipapp",
	"zipfile",
	"zipimport",
	"zlib",
	"zoneinfo",
}

func guess(python string) (map[api.PkgName]bool, bool) {
	pypiMap, err := NewPypiMap()
	if err != nil {
		util.Die(err.Error())
	}
	defer pypiMap.Close()

	availMods := map[string]bool{}

	if knownPkgs, err := listSpecfile(); err == nil {
		for pkgName := range knownPkgs {
			mods, ok := pypiMap.PackageToModules(string(pkgName))
			if ok {
				for _, mod := range mods {
					availMods[mod] = true
				}
			}
		}
	}

	pkgs := map[api.PkgName]bool{}

	for fullModname, pragmas := range output.Imports {
		modname := getTopLevelModuleName(fullModname)
		// provided by an existing package or perhaps by the system
		if availMods[modname] {
			continue
		}

		// If this module has a package pragma, use that
		if pragmas.Package != "" {
			name := api.PkgName(pragmas.Package)
			pkgs[normalizePackageName(name)] = true

		} else {
			// Otherwise, try and look it up in Pypi
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
	}

	return pkgs, output.Success
}

func findImports(dir string) (map[string]bool, error) {
	py := python.GetLanguage()
	pkgs, err := util.GuessWithTreeSitter(dir, py, importsQuery, pyPathGlobs, pyIgnoreGlobs)

	if err != nil {
		return nil, err
	}

	foundImportPaths := map[string]bool{}
	for _, pkg := range pkgs {
		foundImportPaths[pkg] = true
	}

	return foundImportPaths, nil
}

func filterImports(found map[string]bool) (map[api.PkgName]bool, bool) {
	for pkg := range found {
		mod := strings.Split(pkg, ".")[0]
	}

	pypiMap, err := NewPypiMap()
	if err != nil {
		util.Die(err.Error())
	}
	defer pypiMap.Close()

	pkgs := map[api.PkgName]bool{}

	for fullModname := range found {
		modname := getTopLevelModuleName(fullModname)
		// provided by an existing package or perhaps by the system
		if pypiMap.ModuleExists(modname) {
			continue
		}

		// Otherwise, try and look it up in Pypi
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
