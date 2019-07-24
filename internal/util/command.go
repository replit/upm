package util

import (
	"os"
	"os/exec"
	"strings"

	"github.com/kballard/go-shellquote"
)

// quoteCmd escapes shell characters in a command. Additionally, it
// replaces long or multiline arguments with a placeholder.
func quoteCmd(cmd []string) string {
	cleanedCmd := make([]string, len(cmd))
	copy(cleanedCmd, cmd)
	for i := range cmd {
		if strings.ContainsRune(cmd[i], '\n') || len(cmd[i]) > 50 {
			cleanedCmd[i] = "<secret sauce>"
		}
	}
	return shellquote.Join(cleanedCmd...)
}

// RunCmd prints and runs the given command, exiting the process on
// error or command failure. Stdout and stderr go to the terminal.
func RunCmd(cmd []string) {
	ProgressMsg(quoteCmd(cmd))
	command := exec.Command(cmd[0], cmd[1:]...)
	command.Stdout = os.Stderr
	command.Stderr = os.Stderr
	if err := command.Run(); err != nil {
		Die("%s", err)
	}
}

// GetCmdOutput prints and runs the given command, returning its
// stdout as a string. Stderr goes to the terminal. GetCmdOutput exits
// the process on error or command failure.
func GetCmdOutput(cmd []string) []byte {
	ProgressMsg(quoteCmd(cmd))
	command := exec.Command(cmd[0], cmd[1:]...)
	command.Stderr = os.Stderr
	output, err := command.Output()
	if err != nil {
		Die("%s", err)
	}
	return output
}
