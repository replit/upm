package util

import (
	"fmt"
	"os"
	"os/exec"
	"strings"

	"github.com/kballard/go-shellquote"
	"github.com/replit/upm/internal/config"
)

func ProgressMsg(msg string) {
	if !config.Quiet {
		fmt.Println("-->", msg)
	}
}

func quoteCmd(cmd []string) string {
	cleanedCmd := make([]string, len(cmd))
	copy(cleanedCmd, cmd)
	for i := range cmd {
		if strings.ContainsRune(cmd[i], '\n') {
			cleanedCmd[i] = "<secret sauce>"
		}
	}
	return shellquote.Join(cleanedCmd...)
}

func RunCmd(cmd []string) {
	ProgressMsg(quoteCmd(cmd))
	command := exec.Command(cmd[0], cmd[1:]...)
	command.Stdout = os.Stderr
	command.Stderr = os.Stderr
	if err := command.Run(); err != nil {
		Die("%s", err)
	}
}

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
