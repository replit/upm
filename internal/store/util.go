package store

import (
	"crypto/md5"
	"encoding/hex"
	"io/ioutil"
	"os"

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
