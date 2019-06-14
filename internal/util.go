package internal

import (
	"crypto/md5"
	"encoding/hex"
	"fmt"
	"github.com/kballard/go-shellquote"
	"io/ioutil"
	"os"
	"os/exec"
	"strings"
)

func die(format string, a ...interface{}) {
	fmt.Fprintf(os.Stderr, format + "\n", a...)
	os.Exit(1)
}

func panicf(format string, a ...interface{}) {
	panic(fmt.Sprintf(format, a...))
}

func notImplemented() {
	panic("not yet implemented")
}

func progressMsg(msg string) {
	fmt.Println("-->", msg)
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

func runCmd(cmd []string) {
	progressMsg(quoteCmd(cmd))
	command := exec.Command(cmd[0], cmd[1:]...)
	command.Stdout = os.Stderr
	command.Stderr = os.Stderr
	if err := command.Run(); err != nil {
		die("%s", err)
	}
}

func getCmdOutput(cmd []string) []byte {
	progressMsg(quoteCmd(cmd))
	command := exec.Command(cmd[0], cmd[1:]...)
	command.Stderr = os.Stderr
	output, err := command.Output()
	if err != nil {
		die("%s", err)
	}
	return output
}

func hashFile(filename string) hash {
	bytes, err := ioutil.ReadFile(filename)
	if os.IsNotExist(err) {
		return ""
	} else if err != nil {
		die("%s: %s", filename, err)
	}
	sum := md5.Sum(bytes)
	return hash(hex.EncodeToString(sum[:]))
}

func quirksIsNotReproducible(b languageBackend) bool {
	return (b.quirks & quirksNotReproducible) != 0
}

func quirksIsReproducible(b languageBackend) bool {
	return (b.quirks & quirksNotReproducible) == 0
}
