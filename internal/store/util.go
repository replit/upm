package store

import (
	"crypto/md5"
	"encoding/hex"
	"io/ioutil"
	"os"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

// hashFile computes the MD5 hash of the contents of the given file.
// It returns the empty string if the file does not exist.
func hashFile(filename string) hash {
	bytes, err := ioutil.ReadFile(filename)
	if os.IsNotExist(err) {
		return ""
	} else if err != nil {
		util.Die("%s: %s", filename, err)
	}
	sum := md5.Sum(bytes)
	return hash(hex.EncodeToString(sum[:]))
}

// hashImports computes the MD5 hash of the matches of b.GuessRegexps
// against b.FilenamePatterns within the project. It is guaranteed to
// be deterministic as long as the project files do not change in such
// a way as to change what any of the regexps match against. If there
// are no regexp matches, then as a special case the empty string is
// returned.
func hashImports(b api.LanguageBackend) hash {
	bytes := []byte{}
	for _, r := range b.GuessRegexps {
		// Rely on lexical ordering of filepath.Walk to
		// guarantee a consistent hash.
		for _, match := range util.SearchRecursive(r, b.FilenamePatterns) {
			if len(match) == 1 {
				bytes = append(bytes, match[0]...)
			} else {
				for _, part := range match[1:] {
					bytes = append(bytes, part...)
				}
			}
		}
	}
	if len(bytes) == 0 {
		return ""
	}
	sum := md5.Sum(bytes)
	return hash(hex.EncodeToString(sum[:]))
}
