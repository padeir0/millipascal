package util

import (
	"mpc/frontend/errors"
	"io/ioutil"
	"os"
)

func ReadOrBurst(file string) string {
	text, e := ioutil.ReadFile(file)
	if e != nil {
		Fatal(e.Error())
	}
	return string(text)
}

func OkOrBurst(e *errors.CompilerError) {
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
