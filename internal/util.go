package internal

import (
	"fmt"
	"github.com/kballard/go-shellquote"
	"os"
	"os/exec"
)

func die(format string, a ...interface{}) {
	fmt.Fprintf(os.Stderr, format + "\n", a...)
	os.Exit(1)
}

func notImplemented() {
	die("not yet implemented")
}

func runCmd(cmd []string) {
	fmt.Println("-->", shellquote.Join(cmd...))
	command := exec.Command(cmd[0], cmd[1:]...)
	command.Stdout = os.Stderr
	command.Stderr = os.Stderr
	if err := command.Run(); err != nil {
		die("%s", err)
	}
}
