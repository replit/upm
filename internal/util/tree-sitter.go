package util

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path"

	sitter "github.com/smacker/go-tree-sitter"
)

type queryImportsResult struct {
	path    string
	imports []string
	err     error
}

func GuessWithTreeSitter(dir string, lang *sitter.Language, queryImports string, searchGlobPatterns, ignoreGlobPatterns []string) ([]string, error) {
	dirFS := os.DirFS(dir)

	ignoredPaths := map[string]bool{}
	for _, pattern := range ignoreGlobPatterns {
		globIgnorePaths, err := fs.Glob(dirFS, pattern)
		if err != nil {
			return nil, err
		}

		for _, gPath := range globIgnorePaths {
			ignoredPaths[gPath] = true
		}
	}

	pathsToSearch := []string{}
	for _, pattern := range searchGlobPatterns {
		globSearchPaths, err := fs.Glob(dirFS, pattern)
		if err != nil {
			return nil, err
		}

		for _, gPath := range globSearchPaths {
			if !ignoredPaths[gPath] {
				pathsToSearch = append(pathsToSearch, path.Join(dir, gPath))
			}
		}
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

		imports = append(imports, result.imports...)
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

	importPaths := []string{}
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
		for _, capture := range match.Captures {
			if query.CaptureNameForId(capture.Index) == "import" {
				importPath = capture.Node.Content(contents)
				break
			}
		}

		if importPath == "" {
			// shouldn't happen, with the way the query is written and handled.
			err = errors.New("failed to parse import path")
			break
		}

		importPaths = append(importPaths, importPath)
	}

	return queryImportsResult{file, importPaths, err}
}
