// This command generates fo source holding a mapping of:
// Ruby requires -> RubyGem names
// Based on the conventions from https://guides.rubygems.org/name-your-gem/

package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"os/exec"
)

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

	gemMap := map[string]string{}
	dec := json.NewDecoder(injson)
	if err := dec.Decode(&gemMap); err != nil {
		panic(err)
	}

	fmt.Fprintf(outgo, "package %s\n", *pkg)
	fmt.Fprintf(outgo, `
var requireToGemMapCached = map[string]string{}

func requireToGemMap() map[string]string {
	if len(requireToGemMapCached) == 0 {
		requireToGemMapCached = map[string]string{
`)

	for require, gem := range gemMap {
		fmt.Fprintf(outgo, "\t%#v: %#v,\n", require, gem)
	}

	fmt.Fprintf(outgo, `
		}
	}
	return requireToGemMapCached
}
    `)

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
