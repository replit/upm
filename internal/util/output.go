package util

import (
	"fmt"
	"os"
)

func Die(format string, a ...interface{}) {
	fmt.Fprintf(os.Stderr, format+"\n", a...)
	os.Exit(1)
}

func Panicf(format string, a ...interface{}) {
	panic(fmt.Sprintf(format, a...))
}

func NotImplemented() {
	Die("not yet implemented")
}
