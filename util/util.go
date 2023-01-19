package util

import (
	"io/ioutil"
	. "mpc/core"
	"os"
	"os/exec"
	"time"
)

func ReadOrBurst(file string) string {
	text, e := ioutil.ReadFile(file)
	if e != nil {
		Fatal(e.Error())
	}
	return string(text)
}

func OkOrBurst(e *Error) {
	if e != nil {
		Fatal(e.String())
	}
}

func Stdout(s string) {
	os.Stdout.Write([]byte(s))
}

func Fatal(s string) {
	os.Stderr.Write([]byte(s))
	os.Exit(0)
}

func Fasm(s string, name string) error {
	f, oserr := os.CreateTemp("", "mpc_*")
	if oserr != nil {
		return oserr
	}
	defer os.Remove(f.Name())
	_, oserr = f.WriteString(s)
	if oserr != nil {
		return oserr
	}
	cmd := exec.Command("fasm", f.Name(), name)
	_, oserr = cmd.Output()
	if oserr != nil {
		return oserr
	}
	return nil
}

func ExecWithTimeout(cmdstr string) error {
	cmd := exec.Command(cmdstr)
	if err := cmd.Start(); err != nil {
		return err
	}
	timer := time.AfterFunc(1*time.Second, func() {
		cmd.Process.Kill()
	})
	err := cmd.Wait()
	timer.Stop()
	return err
}
