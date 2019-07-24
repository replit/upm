package util

import (
	"fmt"
	"os"

	"github.com/replit/upm/internal/config"
)

// Log is like fmt.Println, but writes to stderr and is inhibited by
// --quiet.
func Log(a ...interface{}) {
	if !config.Quiet {
		fmt.Fprintln(os.Stderr, a...)
	}
}

// ProgressMsg prints the given message to stderr with a prefix. The
// message is inhibited in --quiet mode, however.
func ProgressMsg(msg string) {
	Log("-->", msg)
}

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
