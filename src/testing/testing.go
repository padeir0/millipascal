package testing

import (
	"strings"

	. "mpc/core"
	et "mpc/core/errorkind"
	"mpc/pipelines"

	"fmt"
	"os"
	"os/exec"
	"time"
)

// files are tested by running them and
// seeing if they exit normally
//
// the test data is located directly in the name
// of the file:
// 	module_name.E001.mp
// 	            ^ error code
// 	module_name.mp
// 	           ^ no error code (file must exit normally)

type TestResult struct {
	File    string
	Message string
	Ok      bool
}

func (res *TestResult) String() string {
	if res.Ok {
		return "\u001b[34mok\u001b[0m"
	}
	return "\u001b[31mfail\u001b[0m"
}

type Stage func(filename string, outname string) (outfile string, err *Error)

func S_Lexer(filename string, outname string) (string, *Error) {
	_, err := pipelines.Lexemes(filename)
	return "", err
}

func S_Parser(filename string, outname string) (string, *Error) {
	_, err := pipelines.Ast(filename)
	return "", err
}

func S_Typechecker(filename string, outname string) (string, *Error) {
	_, err := pipelines.Mod(filename)
	return "", err
}

func S_PirGeneration(filename string, outname string) (string, *Error) {
	_, err := pipelines.Pir(filename)
	return "", err
}

func S_MirGeneration(filename string, outname string) (string, *Error) {
	_, err := pipelines.Mir(filename)
	return "", err
}

func S_FasmGeneration(filename string, outname string) (string, *Error) {
	_, err := pipelines.Fasm(filename, outname)
	return "", err
}

func S_Compile(filename string, outname string) (string, *Error) {
	return pipelines.Compile(filename, outname)
}

func S_Format(filename string, outname string) (string, *Error) {
	_, err := pipelines.ModFmt(filename)
	return "", err
}

func Test(file string, st Stage) TestResult {
	defer recoverIfFatal()
	expectedErr := extractError(file)

	outfile, err := st(file, "")

	if err != nil {
		if err.Code == et.InternalCompilerError {
			return TestResult{
				File:    file,
				Ok:      false,
				Message: err.Message,
			}
		}
		return compareError(file, err, expectedErr)
	}

	if outfile != "" {
		defer os.Remove("./" + outfile)

		oserror := execWithTimeout("./" + outfile)
		if oserror != nil {
			return newResult(file, ProcessFileError(oserror))
		}
	}

	return TestResult{
		File: file,
		Ok:   true,
	}
}

func execWithTimeout(cmdstr string) error {
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

func recoverIfFatal() {
	if r := recover(); r != nil {
		fmt.Printf("\u001b[31m fatal error: %v\t\u001b[0m", r)
	}
}

func extractError(file string) string {
	pathlist := strings.Split(file, "/")
	name := pathlist[len(pathlist)-1]
	sections := strings.Split(name, ".")
	if len(sections) < 3 {
		return ""
	}
	err := sections[len(sections)-2]
	return err
}

func compareError(file string, err *Error, expectedErr string) TestResult {
	if err != nil && expectedErr == "" {
		msg := "expected no errors, instead found: " +
			err.ErrCode()
		return TestResult{
			File:    file,
			Message: msg,
			Ok:      false,
		}
	} else if err == nil && expectedErr != "" {
		msg := "expected error " + expectedErr +
			", instead found nothing"
		return TestResult{
			File:    file,
			Message: msg,
			Ok:      false,
		}
	} else if err != nil && expectedErr != "" {
		actual := err.ErrCode()
		if actual != expectedErr {
			msg := "expected error " + expectedErr +
				", instead found " + actual
			return TestResult{
				File:    file,
				Message: msg,
				Ok:      false,
			}
		}
	}
	return TestResult{
		File: file,
		Ok:   true,
	}
}

func newResult(file string, e *Error) TestResult {
	return TestResult{
		File:    file,
		Ok:      false,
		Message: e.Message,
	}
}
