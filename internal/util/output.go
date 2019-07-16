package util

import (
	"fmt"
	"os"
)

// Die is like fmt.Printf, but writes to stderr, adds a newline, and
// terminates the process.
func Die(format string, a ...interface{}) {
	fmt.Fprintf(os.Stderr, format+"\n", a...)
	os.Exit(1)
}

// Panicf is a composition of fmt.Sprintf and panic.
func Panicf(format string, a ...interface{}) {
	panic(fmt.Sprintf(format, a...))
}

// NotImplemented terminates the process, indicating that the
// operation is not implemented. This should be used in language
// backends that do not implement all of UPM's API.
func NotImplemented() {
	Die("not yet implemented")
}
