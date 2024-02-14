package pipelines

import (
	"io/ioutil"
	"os"
	"os/exec"

	"mpc/pir"
	amd64 "mpc/pir/backends/linuxamd64/fasm"
	mir "mpc/pir/backends/linuxamd64/mir"
	mirchecker "mpc/pir/backends/linuxamd64/mir/checker"
	resalloc "mpc/pir/backends/linuxamd64/resalloc"
	pirchecker "mpc/pir/checker"

	. "mpc/core"
	mod "mpc/core/module"

	"mpc/lexer"
	"mpc/linearization"
	"mpc/parser"
	"mpc/resolution"
	"mpc/typechecker"

	"fmt"
)

// processes a single file and returns all tokens
// or an error
func Lexemes(file string) ([]*mod.Node, *Error) {
	s, err := getFile(file)
	if err != nil {
		return nil, err
	}
	st := lexer.NewLexer(file, s)
	return st.ReadAll()
}

// processes a single file and returns it's AST
// or an error
func Ast(file string) (*mod.Node, *Error) {
	s, err := getFile(file)
	if err != nil {
		return nil, err
	}
	return parser.Parse(file, s)
}

// processes a file and all it's dependencies
// returns a typed Module or an error
func Mod(file string) (*mod.Module, *Error) {
	m, err := resolution.Resolve(file, false)
	if err != nil {
		return nil, err
	}

	err = typechecker.Check(m)
	if err != nil {
		return nil, err
	}
	return m, nil
}

// processes a file and all it's dependencies
// returns a typed Module or an error
func ModFmt(file string) (*mod.Module, *Error) {
	m, err := resolution.Resolve(file, true)
	if err != nil {
		return nil, err
	}

	err = typechecker.Check(m)
	if err != nil {
		return nil, err
	}
	return m, nil
}

// processes a file and all it's dependencies
// generates PIR or an error
func Pir(file string) (*pir.Program, *Error) {
	m, err := Mod(file)
	if err != nil {
		return nil, err
	}

	p, err := linearization.Generate(m)
	if err != nil {
		return nil, err
	}

	err = ProcessPirError(pirchecker.Check(p))
	if err != nil {
		return nil, err
	}

	return p, nil
}

var NumRegisters = len(amd64.Registers)

// processes a file and all it's dependencies
// generates MIR or an error
func Mir(file string) (*mir.Program, *Error) {
	p, err := Pir(file)
	if err != nil {
		return nil, err
	}
	mirP := resalloc.Allocate(p, NumRegisters)
	err = ProcessPirError(mirchecker.Check(mirP))
	if err != nil {
		fmt.Println(mirP)
		return nil, err
	}
	return mirP, nil
}

// processes a file and all it's dependencies
// generates Fasm program or an error
func Fasm(file string, outname string) (*amd64.FasmProgram, *Error) {
	mirP, err := Mir(file)
	if err != nil {
		return nil, err
	}
	out := amd64.Generate(mirP, outname)
	return out, nil
}

// processes a Millipascal program and saves a binary
// into disk
func Compile(file string, outname string) (string, *Error) {
	fp, err := Fasm(file, outname)
	if err != nil {
		return "", err
	}
	ioerr := genBinary(fp)
	if ioerr != nil {
		return "", ProcessFileError(ioerr)
	}
	return fp.Name, nil
}

func genBinary(fp *amd64.FasmProgram) error {
	f, oserr := os.CreateTemp("", "mpc_*")
	if oserr != nil {
		return oserr
	}
	defer os.Remove(f.Name())
	_, oserr = f.WriteString(fp.Contents)
	if oserr != nil {
		return oserr
	}
	cmd := exec.Command("fasm", f.Name(), "./"+fp.Name)
	_, oserr = cmd.Output()
	if oserr != nil {
		return oserr
	}
	return nil
}

func getFile(file string) (string, *Error) {
	text, e := ioutil.ReadFile(file)
	if e != nil {
		return "", ProcessFileError(e)
	}
	return string(text), nil
}
