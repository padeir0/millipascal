package frontend

import (
	"mpc/frontend/ast"
	"mpc/frontend/errors"
	et "mpc/frontend/ErrType"
	parser "mpc/frontend/parser"
	lexer "mpc/frontend/lexer"
	resolver "mpc/frontend/resolver"
	nameresolution "mpc/frontend/nameresolution"
	typechecker "mpc/frontend/typechecker"
	"io/ioutil"
)

func Lex(file string) ([]*ast.Node, *errors.CompilerError) {
	s, err := getFile(file)
	if err != nil {
		return nil, err
	}
	st := lexer.NewLexer(s)
	return lexer.ReadAll(st)
}

func Parse(file string) (*ast.Node, *errors.CompilerError) {
	s, err := getFile(file)
	if err != nil {
		return nil, err
	}
	return parser.Parse(s)
}

func All(file string) (*ast.Module, *errors.CompilerError) {
	m, err := resolver.Resolve(file)
	if err != nil {
		return nil, err
	}
	err = module(m)
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

func module(M *ast.Module) *errors.CompilerError {
	var err *errors.CompilerError

	err = nameresolution.ResolveNames(M)
	if err != nil {
		return err
	}

	err = typechecker.Check(M)
	if err != nil {
		return err
	}

	return nil
}
