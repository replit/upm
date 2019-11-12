// Package wasm provides a backend for Wasm using wapm.
package wasm

import (
	"context"
	"strings"

	"github.com/BurntSushi/toml"
	graphql "github.com/machinebox/graphql"
	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

type wapmPackage struct {
	Name string `json:"name"`
}

type wapmUser struct {
	Username string `json:"username"`
}

type wapmPackageVersion struct {
	Version     string      `json:"version"`
	Description string      `json:"description"`
	Homepage    string      `json:"homepage"`
	Repository  string      `json:"repository"`
	License     string      `json:"license"`
	PublishedBy wapmUser    `json:"publishedBy"`
	Package     wapmPackage `json:"package"`
}

type infoQuery struct {
	PackageVersion wapmPackageVersion `json:"packageVersion"`
}

type searchQuery struct {
	Search struct {
		Edges []struct {
			Node wapmPackageVersion `json:"node"`
		} `json:"edges"`
	} `json:"search"`
}

// wapmTOML represents the relevant parts of a wapm.toml
// file.
type wapmTOML struct {
	Package struct {
		Name string `json:"name"`
	} `json:"package"`
	Dependencies map[string]string `json:"dependencies"`
}

// wapmLock represents the relevant parts of a wapm.lock file, in
// TOML format.
type wapmLock struct {
	Modules map[string]map[string]interface{} `json:"modules"`
	//            ^ package  ^ version
}

// WasmBackend is a UPM language backend for Wasm using WAPM.
var WasmBackend = api.LanguageBackend{
	Name:             "wasm-wapm",
	Specfile:         "wapm.toml",
	Lockfile:         "wapm.lock",
	FilenamePatterns: []string{"*.wasm"},
	Quirks:           api.QuirksAddRemoveAlsoLocks,
	GetPackageDir: func() string {
		path := string(util.GetCmdOutput([]string{"wapm", "bin"}))
		path = strings.TrimSuffix(path, "\n")
		path = strings.TrimSuffix(path, "/.bin")
		if path == "" {
			return "wapm_packages"
		}
		return path
	},
	Search: func(query string) []api.PkgInfo {
		client := graphql.NewClient("https://registry.wapm.io/graphql")
		req := graphql.NewRequest(`
        query searchPackages($query: String!) {
            search(query: $query) {
                edges {
                    node {
                        ...on PackageVersion {
                            package {
                                name
                            }
                            version
                            description
                            homepage
                            repository
                            license
                            publishedBy {
                                username
                            }
                        }
                    }
                }
            }
        }
        `)
		// set any variables
		req.Var("query", string(query))
		ctx := context.Background()

		var search searchQuery
		if err := client.Run(ctx, req, &search); err != nil {
			util.Die("WAPM response: %s", err)
		}

		results := []api.PkgInfo{}
		for _, edge := range search.Search.Edges {
			results = append(results, api.PkgInfo{
				Name:        edge.Node.Package.Name,
				Description: edge.Node.Description,
				Version:     edge.Node.Version,
				HomepageURL: edge.Node.Homepage,
				Author:      edge.Node.PublishedBy.Username,
				License:     edge.Node.License,
			})
		}
		return results
	},
	Info: func(name api.PkgName) api.PkgInfo {
		client := graphql.NewClient("https://registry.wapm.io/graphql")
		req := graphql.NewRequest(`
        query getPackage($name: String!) {
            packageVersion: getPackageVersion(name: $name) {
                package {
                    name
                }
                version
                description
                homepage
                repository
                license
                publishedBy {
                    username
                }
            }
        }
        `)
		// set any variables
		req.Var("name", string(name))
		ctx := context.Background()

		var output infoQuery
		if err := client.Run(ctx, req, &output); err != nil {
			util.Die("WAPM response: %s", err)
		}

		return api.PkgInfo{
			Name:        output.PackageVersion.Package.Name,
			Description: output.PackageVersion.Description,
			Version:     output.PackageVersion.Version,
			HomepageURL: output.PackageVersion.Homepage,
			Author:      output.PackageVersion.PublishedBy.Username,
			License:     output.PackageVersion.License,
		}
	},
	Add: func(pkgs map[api.PkgName]api.PkgSpec) {
		if !util.Exists("wapm.lock") {
			util.RunCmd([]string{"wapm", "init", "-y"})
		}
		cmd := []string{"wapm", "add"}
		for name, spec := range pkgs {
			arg := string(name)
			if spec != "" {
				arg += "@" + string(spec)
			}
			cmd = append(cmd, arg)
		}
		util.RunCmd(cmd)
		// We run install after adding the package, to generate the lockfile
		util.RunCmd([]string{"wapm", "install"})
	},
	Remove: func(pkgs map[api.PkgName]bool) {
		cmd := []string{"wapm", "remove"}
		for name := range pkgs {
			cmd = append(cmd, string(name))
		}
		util.RunCmd(cmd)
	},
	Lock: func() {
		util.RunCmd([]string{"wapm", "install"})
	},
	Install: func() {
		util.RunCmd([]string{"wapm", "install"})
	},
	ListSpecfile: func() map[api.PkgName]api.PkgSpec {
		var cfg wapmTOML
		if _, err := toml.DecodeFile("wapm.toml", &cfg); err != nil {
			util.Die("%s", err.Error())
		}
		pkgs := map[api.PkgName]api.PkgSpec{}
		for nameStr, specStr := range cfg.Dependencies {
			if specStr == "" {
				continue
			}
			pkgs[api.PkgName(nameStr)] = api.PkgSpec(specStr)
		}

		return pkgs
	},
	ListLockfile: func() map[api.PkgName]api.PkgVersion {
		var cfg wapmLock
		if _, err := toml.DecodeFile("wapm.lock", &cfg); err != nil {
			util.Die("%s", err.Error())
		}
		pkgs := map[api.PkgName]api.PkgVersion{}
		for pkgName, pkgObj := range cfg.Modules {
			for pkgVersion := range pkgObj {
				name := api.PkgName(pkgName)
				version := api.PkgVersion(pkgVersion)
				pkgs[name] = version
			}
		}
		return pkgs
	},
	Guess: func() (map[api.PkgName]bool, bool) {
		util.NotImplemented()
		panic("unreachable code")
	},
}
