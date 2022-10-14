package nodejs

import (
	"io/ioutil"
	"log"
	"path/filepath"
	"strings"

	"github.com/amasad/esparse/ast"
	"github.com/amasad/esparse/logging"
	"github.com/amasad/esparse/parser"
	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
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

func getExt(path string) string {
	extension := ""
	if lastDot := strings.LastIndexByte(path, '.'); lastDot >= 0 {
		extension = path[lastDot:]
	}
	return extension
}

type parseResult struct {
	ast ast.AST
	ok  bool
}

func parseFile(source logging.Source, results chan parseResult) {
	parseOptions := parser.ParseOptions{
		IsBundling:   true,
		MangleSyntax: false,
	}

	// Always parse jsx
	parseOptions.JSX.Parse = true
	// TS parsing strips unused imports, that becomes
	// inconsistent with the regex-based import searching used to
	// generate the guess hash, so we disable it
	parseOptions.TS.Parse = false

	logo, _ := logging.NewDeferLog()

	ast, ok := parser.Parse(logo, source, parseOptions)

	results <- parseResult{ast, ok}
}

func guessBareImports() map[api.PkgName]bool {
	pkgs := map[api.PkgName]bool{}
	results := make(chan parseResult)
	numParsedFiles := 0
	var visitDir func(dirName string)

	visitDir = func(dirName string) {
		for _, ignoredPath := range util.IgnoredPaths {
			if ignoredPath == filepath.Base(dirName) {
				return
			}
		}

		files, err := ioutil.ReadDir(dirName)
		if err != nil {
			log.Fatalln(err)
		}

		for i, file := range files {
			absPath := filepath.Join(dirName, file.Name())
			if file.IsDir() {
				visitDir(absPath)
				continue
			}

			extension := getExt(absPath)
			if extension != ".js" &&
				extension != ".jsx" &&
				extension != ".tsx" &&
				extension != ".ts" {
				continue
			}

			contents, err := ioutil.ReadFile(absPath)
			if err != nil {
				log.Fatalln(err)
			}

			source := logging.Source{
				Index:        uint32(i),
				AbsolutePath: absPath,
				PrettyPath:   absPath,
				Contents:     string(contents),
			}
			numParsedFiles++
			go parseFile(source, results)
		}

	}

	dir, err := filepath.Abs(".")
	if err != nil {
		log.Fatalln(err)
	}

	visitDir(dir)

	for i := 0; i < numParsedFiles; i++ {
		result := <-results
		if !result.ok {
			continue
		}

		for _, importPath := range result.ast.ImportPaths {
			mod := importPath.Path.Text

			// Since Node.js 16, you can prefix the import path with `node:` to denote that the
			// module is a core module.
			if strings.HasPrefix(mod, "node:") {
				continue
			}

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

			// Handle scoped modules
			if mod[0] == '@' {
				parts := strings.Split(mod, "/")
				if len(parts) < 2 {
					continue
				}
				mod = strings.Join(parts[:2], "/")
			} else {
				parts := strings.Split(mod, "/")
				mod = parts[0]
			}

			pkgs[api.PkgName(mod)] = true
		}
	}

	return pkgs
}
