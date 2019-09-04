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
			if strings.ReplaceAll(strings.ToLower(candidate.Pkg), "-", "_") ==
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
