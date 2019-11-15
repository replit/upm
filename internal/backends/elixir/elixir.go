package elixir

import (
	"bufio"
	"encoding/json"
	"fmt"
	"net/http"
	"os"
	"strings"
	"time"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

const hexAPIBaseURL = "https://hex.pm/api"

type hexPkg struct {
	Name        string          `json:"name"`
	HTMLURL     string          `json:"html_url"`
	DocsHTMLURL string          `json:"docs_html_url"`
	Releases    []hexPkgRelease `json:"releases"`
	Meta        hexPkgMeta      `json:"meta"`
}

type hexPkgRelease struct {
	Version string `json:"version"`
}

type hexPkgMeta struct {
	Description string            `json:"description"`
	Licenses    []string          `json:"licenses"`
	Links       map[string]string `json:"links"`
}

// ElixirMixBackend is a UPM backend for Elixir's Mix build tool: https://hexdocs.pm/mix/
var ElixirMixBackend = api.LanguageBackend{
	Name:             "elixir",
	Specfile:         "mix.exs",
	Lockfile:         "mix.lock",
	FilenamePatterns: []string{"*.ex", "*.exs"},
	GetPackageDir: func() string {
		return "deps"
	},
	Search: func(query string) []api.PkgInfo {
		req, err := http.NewRequest(http.MethodGet, hexAPIBaseURL+"/packages", nil)
		if err != nil {
			util.Die("request lol")
		}
		req.URL.Query().Add("search", query)
		c := http.Client{
			Timeout: 30 * time.Second,
		}
		resp, err := c.Do(req)
		if err != nil {
			util.Die("client lol")
		}
		defer resp.Body.Close()

		hexPkgs := []hexPkg{}
		dec := json.NewDecoder(resp.Body)
		err = dec.Decode(&hexPkgs)
		if err != nil {
			util.Die(err.Error())
		}

		pkgInfos := []api.PkgInfo{}
		for _, hexPkg := range hexPkgs {
			pkgInfo := api.PkgInfo{
				Name:             hexPkg.Name,
				Description:      hexPkg.Meta.Description,
				HomepageURL:      hexPkg.HTMLURL,
				DocumentationURL: hexPkg.DocsHTMLURL,
			}
			for i, license := range hexPkg.Meta.Licenses {
				pkgInfo.License = pkgInfo.License + license
				if i < len(hexPkg.Meta.Licenses)-1 {
					pkgInfo.License += ", "
				}
			}
			pkgInfos = append(pkgInfos, pkgInfo)
		}

		return pkgInfos
	},
	Info: func(name api.PkgName) api.PkgInfo {
		url := fmt.Sprintf("%s/packages/%s", hexAPIBaseURL, name)
		req, err := http.NewRequest(http.MethodGet, url, nil)
		if err != nil {
			util.Die("request lol")
		}
		c := http.Client{
			Timeout: 30 * time.Second,
		}
		resp, err := c.Do(req)
		if err != nil {
			util.Die("client lol")
		}
		defer resp.Body.Close()

		hexPkg := hexPkg{}
		dec := json.NewDecoder(resp.Body)
		err = dec.Decode(&hexPkg)
		if err != nil {
			util.Die(err.Error())
		}

		pkgInfo := api.PkgInfo{
			Name:             hexPkg.Name,
			Description:      hexPkg.Meta.Description,
			HomepageURL:      hexPkg.HTMLURL,
			DocumentationURL: hexPkg.DocsHTMLURL,
		}
		for i, license := range hexPkg.Meta.Licenses {
			pkgInfo.License = pkgInfo.License + license
			if i < len(hexPkg.Meta.Licenses)-1 {
				pkgInfo.License += ", "
			}
		}

		return pkgInfo
	},
	Add: func(map[api.PkgName]api.PkgSpec) {
		// TODO: something like https://github.com/bryanstearns/mix_deps_add
	},
	Remove:  func(map[api.PkgName]bool) {},
	Lock:    func() {},
	Install: func() {},
	ListSpecfile: func() map[api.PkgName]api.PkgSpec {
		mf := mixFile{"mix.exs"}
		mf.listDeps()
		return map[api.PkgName]api.PkgSpec{}
	},
	ListLockfile: func() map[api.PkgName]api.PkgVersion {
		return map[api.PkgName]api.PkgVersion{}
	},
	Guess: func() (map[api.PkgName]bool, bool) {
		return map[api.PkgName]bool{}, false
	},
}

type mixFile struct {
	path string
}

func (m *mixFile) listDeps() []api.PkgSpec {
	f, err := os.Open(m.path)
	if err != nil {
		util.Die("mix file: %v", err)
	}
	defer f.Close()

	// we're looking for the `defp deps do` section
	insideDeps := false
	scanner := bufio.NewScanner(f)
outer:
	for scanner.Scan() {
		line := scanner.Text()
		line = strings.Trim(line, " \t")

		if insideDeps {
			if line == "end" {
				insideDeps = false
				continue
			}
			fmt.Println(line)
		} else {
			split := strings.Split(line, " ")
			if len(split) != 3 {
				continue
			}

			expected := []string{"defp", "deps()", "do"}
			for i := range expected {
				if split[i] != expected[i] {
					continue outer
				}
			}

			insideDeps = true
		}
	}

	return []api.PkgSpec{}
}
