package pipelines

import (
	"errors"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"

	"mpc/asmproc"
	gen "mpc/backend0/gen"
	mir "mpc/backend0/mir"
	mirchecker "mpc/backend0/mir/checker"
	resalloc "mpc/backend0/resalloc"
	"mpc/core/asm"
	fasm "mpc/fasm"

	"mpc/core/pir"
	pirchecker "mpc/core/pir/checker"

	. "mpc/core"
	mod "mpc/core/module"

	"mpc/constexpr"
	"mpc/lexer"
	"mpc/linearization"
	"mpc/parser"
	"mpc/resolution"
	"mpc/typechecker"
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
	err = constexpr.EvalConstExprs(m)
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
	err = constexpr.EvalConstExprs(m)
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

	err = typechecker.CheckMain(m)
	if err != nil {
		return nil, err
	}

	err = asmproc.GenAsmProcs(m)
	if err != nil {
		return nil, err
	}

	p, err := linearization.Generate(m)
	if err != nil {
		return nil, err
	}

	err = pirchecker.Check(p)
	if err != nil {
		fmt.Println(p)
		return nil, err
	}

	return p, nil
}

var NumRegisters = len(gen.Registers)

// processes a file and all it's dependencies
// generates MIR or an error
func Mir(file string) (*mir.Program, *Error) {
	p, err := Pir(file)
	if err != nil {
		return nil, err
	}
	mirP := resalloc.Allocate(p, NumRegisters)
	err = mirchecker.Check(mirP)
	if err != nil {
		return nil, err
	}
	return mirP, nil
}

// processes a file and all it's dependencies
// generates Asm program or an error
func Asm(file string, outname string) (*asm.Program, *Error) {
	mirP, err := Mir(file)
	if err != nil {
		return nil, err
	}
	out := gen.Generate(mirP)
	if outname != "" {
		out.FileName = outname
	} else {
		out.FileName = mirP.Name
	}
	return out, nil
}

func Fasm(file string) (string, *Error) {
	p, err := Asm(file, "")
	if err != nil {
		return "", err
	}
	return fasm.Generate(p), nil
}

// processes a Millipascal program and saves a binary
// into disk
func Compile(file string, outname string) (string, *Error) {
	fp, err := Asm(file, outname)
	if err != nil {
		return "", err
	}
	ioerr := genBinary(fp)
	if ioerr != nil {
		return "", ProcessFileError(ioerr)
	}
	return fp.FileName, nil
}

func genBinary(fp *asm.Program) error {
	f, oserr := os.CreateTemp("", "mpc_*")
	if oserr != nil {
		return oserr
	}
	defer os.Remove(f.Name())
	_, oserr = f.WriteString(fasm.Generate(fp))
	if oserr != nil {
		return oserr
	}
	name := "./" + fp.FileName
	cmd := exec.Command("fasm", f.Name(), name)

	s, oserr := cmd.CombinedOutput()
	if oserr != nil {
		return errors.New(string(s) + "\n" + oserr.Error())
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
