package util

import (
	"bytes"
	"io/ioutil"
	"os"

	"github.com/natefinch/atomic"
)

func TryWriteAtomic(filename string, contents []byte) {
	if err1 := atomic.WriteFile(filename, bytes.NewReader(contents)); err1 != nil {
		if err2 := ioutil.WriteFile(filename, contents, 0666); err2 != nil {
			Die("%s: %s; on non-atomic retry: %s", filename, err1, err2)
		}
	}
}

func FileExists(filename string) bool {
	if _, err := os.Stat(filename); os.IsNotExist(err) {
		return false
	} else if err != nil {
		Die("%s: %s", filename, err)
		return false
	} else {
		return true
	}
}
