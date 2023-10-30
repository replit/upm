package nodejs

import (
	"os"
	"strings"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
	"github.com/smacker/go-tree-sitter/javascript"
	"github.com/smacker/go-tree-sitter/typescript/tsx"
	"github.com/smacker/go-tree-sitter/typescript/typescript"
)

var internalModules = []string{
	"assert",
	"async_hooks",
	"buffer",
	"child_process",
	"cluster",
	"console",
	"constants",
	"crypto",
	"dgram",
	"dns",
	"domain",
	"events",
	"fs",
	"http",
	"http2",
	"https",
	"inspector",
	"module",
	"net",
	"os",
	"path",
	"perf_hooks",
	"process",
	"punycode",
	"querystring",
	"readline",
	"repl",
	"stream",
	"string_decoder",
	"sys",
	"timers",
	"tls",
	"trace_events",
	"tty",
	"url",
	"util",
	"v8",
	"vm",
	"worker_threads",
	"zlib",
}

// nodejsGuess implements Guess for nodejs-yarn, nodejs-pnpm and nodejs-npm.
func nodejsGuess() (map[api.PkgName]bool, bool) {
	cwd, err := os.Getwd()
	if err != nil {
		util.Die("couldn't get working directory: %s", err)
	}

	dir := os.DirFS(cwd)

	importsQuery := `
(import_statement
  source: (string) @import)

((call_expression
     function: [(identifier) @function
		            (import)]
     arguments: (arguments . (string) @import .))
 (#eq? @function "require"))
`

	js := javascript.GetLanguage()
	jsPkgs, err := util.GuessWithTreeSitter(dir, js, importsQuery, jsPathGlobs, []string{})
	if err != nil {
		util.Die("couldn't guess imports: %s", err)
	}

	ts := typescript.GetLanguage()
	tsPkgs, err := util.GuessWithTreeSitter(dir, ts, importsQuery, tsPathGlobs, []string{})
	if err != nil {
		util.Die("couldn't guess imports: %s", err)
	}

	tsx := tsx.GetLanguage()
	tsxPkgs, err := util.GuessWithTreeSitter(dir, tsx, importsQuery, tsxPathGlobs, []string{})
	if err != nil {
		util.Die("couldn't guess imports: %s", err)
	}

	modules := append([]string{}, jsPkgs...)
	modules = append(modules, tsPkgs...)
	modules = append(modules, tsxPkgs...)

	return findImports(modules), true
}

func findImports(names []string) map[api.PkgName]bool {
	pkgs := map[api.PkgName]bool{}

	for _, mod := range names {
		if mod == "" {
			continue
		}

		if pkgs[api.PkgName(mod)] {
			continue
		}

		// Since Node.js 16, you can prefix the import path with `node:` to denote that the
		// module is a core module.
		if strings.HasPrefix(mod, "node:") {
			continue
		}

		if strings.HasPrefix(mod, "bun:") {
			continue
		}

		// Skip empty imports
		if mod == "" {
			continue
		}

		// Skip absolute imports
		if mod[0] == '/' {
			continue
		}

		// Skip relative imports
		if mod[0] == '.' {
			continue
		}

		// Skip external files, don't import from http or https
		if strings.HasPrefix(mod, "http:") || strings.HasPrefix(mod, "https:") {
			continue
		}

		// Skip script loaders
		if strings.Contains(mod, "!") {
			continue
		}

		// Handle scoped modules or internal modules
		if mod[0] == '@' {
			parts := strings.Split(mod, "/")
			if len(parts) < 2 {
				continue
			}
			mod = strings.Join(parts[:2], "/")
		} else {
			parts := strings.Split(mod, "/")
			mod = parts[0]

			isInternalMod := false
			for _, internal := range internalModules {
				if internal == mod {
					isInternalMod = true
					break
				}
			}
			if isInternalMod {
				continue
			}
		}

		pkgs[api.PkgName(mod)] = true
	}

	return pkgs
}
