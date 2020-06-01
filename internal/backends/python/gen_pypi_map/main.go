// This command generates go source holding a mapping of:
// packages -> modules
// and
// modules -> most likely package
//
// these are provided as the maps pypiPackageToModules and moduleToPypiPackage
// respectively.
package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"sort"
	"strconv"
	"strings"
)

type mapEntry struct {
	Pkg       string   `json:"p"`
	Mods      []string `json:"m"`
	Downloads int      `json:"d"`
}

type downloadSort []*mapEntry

func (dl downloadSort) Len() int {
	return len(dl)
}

func (dl downloadSort) Less(i, j int) bool {
	return dl[i].Downloads > dl[j].Downloads
}

func (dl downloadSort) Swap(i, j int) {
	tmp := dl[i]
	dl[i] = dl[j]
	dl[j] = tmp
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

func main() {
	from := flag.String("from", "", "a json file to generate the map from")
	pkg := flag.String("pkg", "", "the pkg name for the output source")
	out := flag.String("out", "", "the destination file for the generated code")
	flag.Parse()

	outgo, err := os.Create(*out)
	if err != nil {
		panic(err)
	}

	injson, err := os.Open(*from)
	if err != nil {
		panic(err)
	}

	dec := json.NewDecoder(injson)

	pkgs := []*mapEntry{}
	mods := map[string][]*mapEntry{}
	guessable := map[string]bool{}

	for dec.More() {
		var m mapEntry

		err := dec.Decode(&m)
		if err != nil {
			panic(err)
		}

		pkgs = append(pkgs, &m)

		for _, mod := range m.Mods {
			if _, ok := mods[mod]; ok {
				mods[mod] = append(mods[mod], &m)
			} else {
				mods[mod] = []*mapEntry{&m}
			}
		}
	}

	for _, pklist := range mods {
		sort.Sort(downloadSort(pklist))
	}

	fmt.Fprintf(outgo, "package %s\n", *pkg)

	fmt.Fprintf(outgo, `
// moduleToPypiPackage holds a map of all known modules to their corresponding
// best matching package. This helps us guess which packages should be installed
// for the given imports.
var moduleToPypiPackage = map[string]string{
`)

	addMap := func(mod, pkg, comment string) {
		if stdlibMods[mod] {
			return
		}

		guessable[mod] = true

		fmt.Fprintf(outgo, "\t")
		fmt.Fprintf(outgo, `%#v: %#v, // %s`, mod, pkg, comment)
		fmt.Fprintf(outgo, "\n")
	}

nextpkg:
	for mod, pkgs := range mods {
		if len(pkgs) == 0 {
			continue nextpkg
		}

		for _, candidate := range pkgs {
			if strings.Replace(strings.ToLower(candidate.Pkg), "-", "_", -1) ==
				strings.ToLower(mod) {
				addMap(mod, candidate.Pkg, "exact match")
				continue nextpkg
			}
		}

		if pkgs[0].Downloads < 100 {
			continue nextpkg
		}

		if len(pkgs) == 1 {
			addMap(
				mod,
				pkgs[0].Pkg,
				"only one pkg matched dls: "+
					strconv.Itoa(pkgs[0].Downloads),
			)

			continue nextpkg
		}

		// if the top package is 10x more popular than the next, we'll go with
		// it. We've added a cost for every module as well, this seems to get
		// the best results
		if pkgs[0].Downloads/len(pkgs[0].Mods) >
			pkgs[1].Downloads*10/len(pkgs[1].Mods) {
			addMap(
				mod,
				pkgs[0].Pkg,
				"high download stats dls: "+
					strconv.Itoa(pkgs[0].Downloads)+
					" second best: "+
					strconv.Itoa(pkgs[1].Downloads),
			)
			continue nextpkg
		}
	}

	fmt.Fprintf(outgo, "}\n")

	fmt.Fprintf(outgo, `
// pypiPackageToModules holds a map of every known python package to the modules
// it provides. This helps prevent us from installing packages for modules which
// are already provided by installed packages. The list of modules is limited to
// those which could potentially be guessed.
//
// The module names are comma separated because go's compiler seems to vomit
// when you create too many slices.

var pypiPackageToModules = map[string]string{
		`)

	for _, pkg := range pkgs {
		guessableMods := []string{}

		for _, mod := range pkg.Mods {
			if guessable[mod] {
				guessableMods = append(guessableMods, mod)
			}
		}

		if len(guessableMods) == 0 {
			continue
		}

		fmt.Fprintf(outgo, "\t")

		// sadly putting these in slices kills the go compiler. Would be nice to
		// find some other way to intern these though.
		fmt.Fprintf(outgo, `%#v: %#v,`, pkg.Pkg, strings.Join(guessableMods, ","))
		fmt.Fprintf(outgo, "\n")
	}

	fmt.Fprintf(outgo, "}\n")

	err = outgo.Close()
	if err != nil {
		panic(err)
	}

	output, err := exec.Command("gofmt", "-w", "-s", *out).CombinedOutput()
	if err != nil {
		fmt.Println(string(output))
		panic(err)
	}
}
