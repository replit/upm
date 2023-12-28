package util

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path"
	"regexp"

	sitter "github.com/smacker/go-tree-sitter"
	"gopkg.in/DataDog/dd-trace-go.v1/ddtrace/tracer"
)

type queryImportsResult struct {
	path    string
	imports map[string]importPragma
	err     error
}

type importPragma struct {
	Package string
}

// GuessWithTreeSitter guesses the imports of a directory using tree-sitter.
// For every file in dir that matches a pattern in searchGlobPatterns, but
// not in ignoreGlobPatterns, it will parse the file using lang and queryImports.
// When there's a capture tagged as `@import`, it reports the capture as an import.
// If there's a capture tagged as `@pragma` that's on the same line as an import,
// it will include the pragma in the results.
func GuessWithTreeSitter(ctx context.Context, root string, lang *sitter.Language, queryImports string, searchGlobPatterns []string, ignorePathSegments map[string]bool) ([]string, error) {
	//nolint:ineffassign,wastedassign,staticcheck
	span, ctx := tracer.StartSpanFromContext(ctx, "GuessWithTreeSitter")
	defer span.Finish()
	dirFS := os.DirFS(root)

	pathsToSearch := []string{}
	err := fs.WalkDir(dirFS, ".", func(dir string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}

		if ignorePathSegments[d.Name()] {
			return fs.SkipDir
		}

		for _, pattern := range searchGlobPatterns {
			globSearchPaths, err := fs.Glob(os.DirFS(dir), pattern)
			if err != nil {
				return err
			}

			for _, gPath := range globSearchPaths {
				pathsToSearch = append(pathsToSearch, path.Join(dir, gPath))
			}
		}

		return nil
	})
	if err != nil {
		return nil, err
	}

	query, err := sitter.NewQuery([]byte(queryImports), lang)
	if err != nil {
		return nil, err
	}

	results := make(chan queryImportsResult)
	for _, filePath := range pathsToSearch {
		filePath2 := filePath
		go func() {
			results <- queryFile(lang, query, filePath2)
		}()
	}

	imports := []string{}
	failed := false
	for numParsedFiles := 0; numParsedFiles < len(pathsToSearch); numParsedFiles++ {
		result := <-results

		if result.err != nil {
			fmt.Printf("error parsing file %s: %v\n", result.path, result.err)
			failed = true
		}

		for importPath, pragma := range result.imports {
			if pragma.Package != "" {
				imports = append(imports, pragma.Package)
			} else {
				imports = append(imports, importPath)
			}
		}
	}

	if failed {
		err = errors.New("failed to parse some files")
	}

	return imports, err
}

func queryFile(lang *sitter.Language, query *sitter.Query, file string) queryImportsResult {
	qc := sitter.NewQueryCursor()

	contents, err := os.ReadFile(file)
	if err != nil {
		return queryImportsResult{file, nil, err}
	}

	node, err := sitter.ParseCtx(context.Background(), contents, lang)
	if err != nil {
		return queryImportsResult{file, nil, err}
	}

	qc.Exec(query, node)

	importPaths := map[string]importPragma{}
	for {
		match, ok := qc.NextMatch()
		if !ok {
			if match != nil {
				err = fmt.Errorf("error querying %s for import paths", file)
			}
			break
		}

		match = qc.FilterPredicates(match, contents)
		if len(match.Captures) == 0 {
			continue
		}

		var importPath string
		var importPathLine uint32
		pragma := importPragma{}
		for _, capture := range match.Captures {
			switch query.CaptureNameForId(capture.Index) {
			case "import":
				importPath = capture.Node.Content(contents)
				importPathLine = capture.Node.EndPoint().Row

			case "pragma":
				// only capture pragma at the end of the line with the import path
				if capture.Node.StartPoint().Row == importPathLine {
					pragma = parsePragma(capture.Node.Content(contents))
				}
			}
		}

		if importPath == "" {
			// shouldn't happen, with the way the query is written and handled.
			err = errors.New("failed to parse import path")
			break
		}

		importPaths[importPath] = pragma
	}

	return queryImportsResult{file, importPaths, err}
}

var pragmaRegex = regexp.MustCompile(`upm (?:package\((?P<package>.*)\))`)

func parsePragma(pragma string) importPragma {
	caps := pragmaRegex.FindStringSubmatch(pragma)

	if len(caps) == 0 {
		return importPragma{}
	}

	return importPragma{
		Package: caps[pragmaRegex.SubexpIndex("package")],
	}
}
