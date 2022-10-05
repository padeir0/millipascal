package frontend

import (
	"mpc/frontend/ir"
	"mpc/frontend/errors"
	et "mpc/frontend/enums/errType"
	parser "mpc/frontend/parser"
	lexer "mpc/frontend/lexer"
	resolver "mpc/frontend/resolver"
	nameresolution "mpc/frontend/nameresolution"
	typechecker "mpc/frontend/typechecker"
	gen "mpc/frontend/gen"
	"mpc/frontend/irchecker"
	"io/ioutil"
)

func Lex(file string) ([]*ir.Node, *errors.CompilerError) {
	s, err := getFile(file)
	if err != nil {
		return nil, err
	}
	st := lexer.NewLexer(s)
	return lexer.ReadAll(st)
}

func Parse(file string) (*ir.Node, *errors.CompilerError) {
	s, err := getFile(file)
	if err != nil {
		return nil, err
	}
	return parser.Parse(s)
}

func All(file string) (*ir.Module, *errors.CompilerError) {
	m, err := resolver.Resolve(file)
	if err != nil {
		return nil, err
	}
	err = module(m)
	if err != nil {
		return nil, err
	}
	// TODO: have a separate HIR checker and a separate MIR checker
	err = irchecker.Check(m, false)
	if err != nil {
		return nil, err
	}
	return m, nil
}

func getFile(file string) (string, *errors.CompilerError) {
	text, e := ioutil.ReadFile(file)
	if e != nil {
		return "", processFileError(e)
	}
	return string(text), nil
}

func processFileError(e error) *errors.CompilerError {
	return &errors.CompilerError{
		Stage: errors.Resolver,
		Type:  et.FileError,
		Debug: e.Error(),
	}
}

func module(M *ir.Module) *errors.CompilerError {
	var err *errors.CompilerError

	err = nameresolution.ResolveNames(M)
	if err != nil {
		return err
	}

	err = typechecker.Check(M)
	if err != nil {
		return err
	}

	gen.Generate(M)
	if err != nil {
		return err
	}

	return nil
}
