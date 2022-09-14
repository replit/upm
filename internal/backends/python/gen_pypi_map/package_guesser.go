package main

import (
	"sort"
	"strings"
)

func GuessPackage(module string, packages []PackageInfo, downloadStats map[string]int) (PackageInfo, string, bool) {
	// Never try and guess packages in the python stdlib
	if stdlibMods[module] {
		return PackageInfo{}, "", false
	}

	// If no packages provide this module, give up
	if len(packages) == 0 {
		return PackageInfo{}, "", false
	}

	// If there is only one package that provides this module, use that
	if len(packages) == 1 {
		return packages[0], "only one", true
	}

	// There are at least two packages that provide this module
	///////////////////////////////////////////////////////////

	// Got through all the matches, if any package name is almost an exact match to the
	// module name, use that
	var nameMatches []PackageInfo = nil
	for _, candidate := range packages {

		if strings.Replace(strings.ToLower(candidate.Name), "-", "_", -1) == strings.ToLower(module) ||
			strings.Replace(strings.ToLower(candidate.Name), "-", "", -1) == strings.ToLower(module) ||
			strings.Replace(strings.ToLower(candidate.Name), "_", "", -1) == strings.ToLower(module) {
			nameMatches = append(nameMatches, candidate)
		}
	}

	if len(nameMatches) > 0 {
		// Pick the most popular match
		sort.Slice(nameMatches, func(a, b int) bool {
			return downloadStats[nameMatches[a].Name] > downloadStats[nameMatches[b].Name]
		})

		return nameMatches[0], "name match", true
	}

	candidates := removeParentPackages(packages)

	if len(candidates) > 0 {
		packages = candidates
	}

	if len(packages) == 1 {
		return packages[0], "only one after trimming", true
	}

	// Sort the packages by downloads
	sort.Slice(packages, func(a, b int) bool {
		return downloadStats[strings.ToLower(packages[a].Name)] >
			downloadStats[strings.ToLower(packages[b].Name)]
	})

	// If the most downloaded package that provides this module has been
	// downloaded fewer then 100 times, skip the module
	if downloadStats[strings.ToLower(packages[0].Name)] < 100 {
		return PackageInfo{}, "", false
	}

	return packages[0], "most popular of remaining", true
}

/*
Go through all input packages and look through their dependencies (RequiresDist).
If they depend on one or more of the other packages in this list, they are removed
from the candidate list (returned).
*/
func removeParentPackages(packages []PackageInfo) []PackageInfo {
	candidateSet := make(map[string]PackageInfo)
	for _, pkg := range packages {
		candidateSet[pkg.Name] = pkg
	}

	for _, pkg := range packages {
		found := false
		for _, dep := range pkg.RequiresDist {
			for _, otherPkg := range packages {
				if strings.EqualFold(dep, otherPkg.Name) {
					// we found otherPkg is depended on by pkg
					// but does otherPkg also depend on pkg?
					// if so, it's a mutual dependency, and we don't count it
					innerFound := false
					for _, otherPkgDep := range otherPkg.RequiresDist {
						if strings.EqualFold(otherPkgDep, pkg.Name) {
							innerFound = true
							break
						}
					}
					if !innerFound {
						found = true
						break
					}
				}
			}
			if found {
				break
			}
		}
		if found {
			delete(candidateSet, pkg.Name)
		}
	}

	var candidates []PackageInfo
	for _, pkg := range candidateSet {
		candidates = append(candidates, pkg)
	}

	return candidates
}

// pythonStdlibModules this build is built from
// https://docs.python.org/3/py-modindex.htm as we never want to guess a
// standard library module is provided by a remote package.
var stdlibMods = map[string]bool{
	"__future__":      true,
	"__main__":        true,
	"_dummy_thread":   true,
	"_thread":         true,
	"abc":             true,
	"aifc":            true,
	"argparse":        true,
	"array":           true,
	"ast":             true,
	"asynchat":        true,
	"asyncio":         true,
	"asyncore":        true,
	"atexit":          true,
	"audioop":         true,
	"base64":          true,
	"bdb":             true,
	"binascii":        true,
	"binhex":          true,
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
	"distutils":       true,
	"doctest":         true,
	"dummy_threading": true,
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
	"formatter":       true,
	"fractions":       true,
	"ftplib":          true,
	"functools":       true,
	"gc":              true,
	"getopt":          true,
	"getpass":         true,
	"gettext":         true,
	"glob":            true,
	"grp":             true,
	"gzip":            true,
	"hashlib":         true,
	"heapq":           true,
	"hmac":            true,
	"html":            true,
	"http":            true,
	"imaplib":         true,
	"imghdr":          true,
	"imp":             true,
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
	"msvcrt":          true,
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
	"smtpd":           true,
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
	"symbol":          true,
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
}
