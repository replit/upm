package internal

import (
	"crypto/md5"
	"encoding/hex"
	"fmt"
	"github.com/kballard/go-shellquote"
	"io/ioutil"
	"os"
	"os/exec"
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

func runCmd(cmd []string) {
	progressMsg(shellquote.Join(cmd...))
	command := exec.Command(cmd[0], cmd[1:]...)
	command.Stdout = os.Stderr
	command.Stderr = os.Stderr
	if err := command.Run(); err != nil {
		die("%s", err)
	}
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
