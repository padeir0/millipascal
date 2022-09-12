package resolver

import (
	"mpc/frontend/ast"
	"mpc/frontend/errors"
	et "mpc/frontend/enums/errType"
	"mpc/frontend/parser"
	"mpc/frontend/lexer"
	"io/ioutil"
	"strings"
)

func Resolve(filePath string) (*ast.Module, *errors.CompilerError) {
	n, err := openAndParse(filePath)
	if err != nil {
		return nil, err
	}
	name, err := extractName(filePath)
	if err != nil {
		return nil, err
	}
	return newModule(filePath, name, n), nil
}

func extractName(filePath string) (string, *errors.CompilerError) {
	path := strings.Split(filePath, "/")
	name := strings.Split(path[len(path)-1], ".")
	if lexer.IsValidIdentifier(name[0]) {
		return name[0], nil
	}
	return "", &errors.CompilerError{
		Stage: errors.Resolver,
		Type:  et.InvalidFileName,
		Debug: filePath + " : " + name[0],
	}
}

func invalidModuleName(filePath string) *errors.CompilerError {
	return &errors.CompilerError{
		Stage: errors.Resolver,
		Type:  et.InvalidFileName,
		Debug: filePath,
	}
}

func newModule(modID string, name string, root *ast.Node) *ast.Module {
	return &ast.Module{
		ID:   modID,
		Name: name,
		Root: root,
		Globals: map[string]*ast.Symbol{},
	}
}

func openAndParse(file string) (*ast.Node, *errors.CompilerError) {
	text, e := ioutil.ReadFile(file)
	if e != nil {
		return nil, processFileError(e)
	}

	n, err := parser.Parse(string(text))
	if err != nil {
		return nil, err
	}
	return n, nil
}

func processFileError(e error) *errors.CompilerError {
	return &errors.CompilerError{
		Stage: errors.Resolver,
		Type:  et.FileError,
		Debug: e.Error(),
	}
}
