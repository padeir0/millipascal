package testing

import (
	"strings"

	"mpc/backend"
	. "mpc/core"
	et "mpc/core/errorkind"
	"mpc/frontend"
	lexer "mpc/frontend/lexer"
	parser "mpc/frontend/parser"
	. "mpc/util"

	"fmt"
	"os"
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

func Lex(file string) TestResult {
	defer recoverIfFatal()
	expectedErr := extractError(file)
	s := ReadOrBurst(file)
	st := lexer.NewLexer(s)
	_, err := lexer.ReadAll(st)
	return compareError(file, err, expectedErr)
}

func Parse(file string) TestResult {
	defer recoverIfFatal()
	expectedErr := extractError(file)
	s := ReadOrBurst(file)
	_, err := parser.Parse(s)
	return compareError(file, err, expectedErr)
}

func Hir(file string) TestResult {
	defer recoverIfFatal()
	expectedErr := extractError(file)
	_, err := frontend.All(file)
	return compareError(file, err, expectedErr)
}

func Mir(file string) TestResult {
	defer recoverIfFatal()
	expectedErr := extractError(file)
	M, err := frontend.All(file)
	if err != nil {
		if err.Type == et.InternalCompilerError {
			return TestResult{
				File:    file,
				Ok:      false,
				Message: err.Debug,
			}
		}
		return compareError(file, err, expectedErr)
	}
	_, err = backend.Mir(M)
	if err != nil {
		return TestResult{
			File:    file,
			Ok:      false,
			Message: err.Debug,
		}
	}
	return Hir(file)
}

const testfilename = "./dont_use_this_name_EACDEFG1234"

func All(file string) TestResult {
	defer recoverIfFatal()
	expectedErr := extractError(file)
	M, err := frontend.All(file)
	if err != nil {
		if err.Type == et.InternalCompilerError {
			return TestResult{
				File:    file,
				Ok:      false,
				Message: err.Debug,
			}
		}
		return compareError(file, err, expectedErr)
	}

	s, err := backend.Generate(M)
	if err != nil {
		return TestResult{
			File:    file,
			Ok:      false,
			Message: err.Debug,
		}
	}
	oserror := Fasm(s, testfilename)
	if oserror != nil {
		return newResult(file, oserror)
	}
	defer os.Remove(testfilename)

	oserror = ExecWithTimeout(testfilename)
	if oserror != nil {
		return newResult(file, oserror)
	}
	return TestResult{
		File: file,
		Ok:   true,
	}
}

func recoverIfFatal() {
	if r := recover(); r != nil {
		fmt.Printf("\u001b[31m fatal error: %v\t\u001b[0m", r)
	}
}

type Tester func(file string) TestResult

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

func newResult(file string, e error) TestResult {
	return TestResult{
		File:    file,
		Ok:      false,
		Message: e.Error(),
	}
}
