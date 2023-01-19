package frontend

import (
	"io/ioutil"
	. "mpc/core"
	et "mpc/core/errorkind"
	hir "mpc/core/hir"
	hirchecker "mpc/core/hir/checker"
	ir "mpc/core/module"
	gen "mpc/frontend/gen"
	lexer "mpc/frontend/lexer"
	nameresolution "mpc/frontend/nameresolution"
	parser "mpc/frontend/parser"
	resolver "mpc/frontend/resolver"
	typechecker "mpc/frontend/typechecker"
)

func Lex(file string) ([]*ir.Node, *Error) {
	s, err := getFile(file)
	if err != nil {
		return nil, err
	}
	st := lexer.NewLexer(s)
	return lexer.ReadAll(st)
}

func Parse(file string) (*ir.Node, *Error) {
	s, err := getFile(file)
	if err != nil {
		return nil, err
	}
	return parser.Parse(s)
}

func All(file string) (*hir.Program, *Error) {
	m, err := resolver.Resolve(file)
	if err != nil {
		return nil, err
	}
	p, err := module(m)
	if err != nil {
		return nil, err
	}

	err = hirchecker.Check(p)
	if err != nil {
		return nil, err
	}
	return p, nil
}

func getFile(file string) (string, *Error) {
	text, e := ioutil.ReadFile(file)
	if e != nil {
		return "", processFileError(e)
	}
	return string(text), nil
}

func processFileError(e error) *Error {
	return &Error{
		Type:  et.FileError,
		Debug: e.Error(),
	}
}

func module(M *ir.Module) (*hir.Program, *Error) {
	var err *Error

	err = nameresolution.ResolveNames(M)
	if err != nil {
		return nil, err
	}

	err = typechecker.Check(M)
	if err != nil {
		return nil, err
	}

	p, err := gen.Generate(M)
	if err != nil {
		return nil, err
	}

	return p, nil
}
