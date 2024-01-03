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
func die(code int, format string, a ...interface{}) {
	fmt.Fprintf(os.Stderr, format+"\n", a...)
	os.Exit(code)
}

func DieIO(format string, a ...interface{}) {
	die(10, format, a...)
}

func DieOverwrite(format string, a ...interface{}) {
	die(11, format, a...)
}

func DieNetwork(format string, a ...interface{}) {
	die(12, format, a...)
}

func DieProtocol(format string, a ...interface{}) {
	die(13, format, a...)
}

func DieConsistency(format string, a ...interface{}) {
	die(14, format, a...)
}

func DieInitializationError(format string, a ...interface{}) {
	die(15, format, a...)
}

func DieUnimplemented(format string, a ...interface{}) {
	die(15, format, a...)
}

func DieSubprocess(format string, a ...interface{}) {
	die(16, format, a...)
}

// Panicf is a composition of fmt.Sprintf and panic.
func Panicf(format string, a ...interface{}) {
	panic(fmt.Sprintf(format, a...))
}

// NotImplemented terminates the process, indicating that the
// operation is not implemented. This should be used in language
// backends that do not implement all of UPM's API.
func NotImplemented() {
	DieUnimplemented("not yet implemented")
}
