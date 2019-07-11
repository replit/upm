package store

import (
	"crypto/md5"
	"encoding/hex"
	"io/ioutil"
	"os"

	"github.com/replit/upm/internal/api"
	"github.com/replit/upm/internal/util"
)

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
	sum := md5.Sum(bytes)
	return hash(hex.EncodeToString(sum[:]))
}
