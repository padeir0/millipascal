package resolver

import (
	"io/ioutil"
	. "mpc/core"
	et "mpc/core/errorkind"
	ir "mpc/core/module"
	T "mpc/core/module/lexkind"
	"mpc/lexer"
	msg "mpc/messages"
	"mpc/parser"
	"strings"
)

func Resolve(filePath string) (*ir.Module, *Error) {
	name, err := extractName(filePath)
	if err != nil {
		return nil, err
	}

	s, ioerr := newState(filePath)
	if ioerr != nil {
		return nil, processFileError(ioerr)
	}

	m, err := resolveModule(s, name)
	if err != nil {
		return nil, err
	}
	err = checkDependencyCycles(m)
	if err != nil {
		return nil, err
	}
	return m, nil
}

type state struct {
	Modules       map[string]*ir.Module
	BaseFolder    string
	FilesInFolder []string

	RefNode   *ir.Node // for errors
	RefModule *ir.Module
}

func newState(fullPath string) (*state, error) {
	folder := getFolder(fullPath)
	files, err := ioutil.ReadDir(folder)
	if err != nil {
		return nil, err
	}
	filenames := make([]string, len(files))
	for i, file := range files {
		filenames[i] = file.Name()
	}
	return &state{
		Modules:       map[string]*ir.Module{},
		BaseFolder:    folder,
		FilesInFolder: filenames,
	}, nil
}

func resolveModule(s *state, modID string) (*ir.Module, *Error) {
	mod, ok := s.Modules[modID]
	if ok {
		return mod, nil
	}
	fileName, err := findFile(s, modID)
	if err != nil {
		return nil, err
	}
	n, err := openAndParse(s, fileName)
	if err != nil {
		return nil, err
	}
	mod = newModule(s.BaseFolder, modID, fileName, n)
	s.Modules[modID] = mod

	err = resolveDependencies(s, n.Leaves[0], mod)
	if err != nil {
		return nil, err
	}
	return mod, nil
}

func resolveDependencies(s *state, coupling *ir.Node, mod *ir.Module) *Error {
	if coupling.Lex != T.COUPLINGS {
		panic("resolver: resolveDependencies: bad node")
	}
	s.RefModule = mod
	for _, n := range coupling.Leaves {
		switch n.Lex {
		case T.IMPORT:
			err := multiImport(s, n, mod)
			if err != nil {
				return err
			}
		case T.FROM:
			err := fromImport(s, n, mod)
			if err != nil {
				return err
			}
		case T.EXPORT:
			// must run after the symbol resolution
		}
	}
	return nil
}

func fromImport(s *state, n *ir.Node, dependentMod *ir.Module) *Error {
	modID := n.Leaves[0].Text
	s.RefNode = n.Leaves[0]

	mod, err := resolveModule(s, modID)
	if err != nil {
		exc := Excerpt{
			Location: place(n, dependentMod),
			Message:  err.Debug,
		}
		err.Info = append(err.Info, exc)
		return err
	}
	addDependency(dependentMod, mod, n)
	return nil
}

func multiImport(s *state, n *ir.Node, dependentMod *ir.Module) *Error {
	if n.Lex != T.IMPORT {
		panic("resolver: singleImport: bad node")
	}

	for _, n := range n.Leaves {
		s.RefNode = n

		mod, err := resolveModule(s, n.Text)
		if err != nil {
			exc := Excerpt{
				Location: place(n, dependentMod),
				Message:  err.Debug,
			}
			err.Info = append(err.Info, exc)
			return err
		}

		addDependency(dependentMod, mod, n)

		if err != nil {
			return err
		}
	}
	return nil
}

func addDependency(parent *ir.Module, dependency *ir.Module, source *ir.Node) {
	_, ok := parent.Dependencies[dependency.Name]
	if ok {
		panic("resolver: addDependency: what the fuck man")
	}
	parent.Dependencies[dependency.Name] = &ir.Dependency{M: dependency, Source: source}
}

func getFolder(fullpath string) string {
	path := strings.Split(fullpath, "/")
	if len(path) == 1 {
		return ""
	}
	folder := strings.Join(path[:len(path)-1], "/")
	return folder
}

func place(n *ir.Node, mod *ir.Module) *SourceLocation {
	return &SourceLocation{
		Line: n.Line, Col: n.Col,
		File: mod.FullPath,
	}
}

func extractName(filePath string) (string, *Error) {
	path := strings.Split(filePath, "/")
	name := strings.Split(path[len(path)-1], ".")
	if lexer.IsValidIdentifier(name[0]) {
		return name[0], nil
	}
	return "", &Error{
		Type:  et.InvalidFileName,
		Debug: filePath + " : " + name[0],
	}
}

func invalidModuleName(filePath string) *Error {
	return &Error{
		Type:  et.InvalidFileName,
		Debug: filePath,
	}
}

func newModule(basePath, modID, filename string, root *ir.Node) *ir.Module {
	return &ir.Module{
		BasePath:     basePath,
		Name:         modID,
		FullPath:     basePath + "/" + filename,
		Root:         root,
		Dependencies: map[string]*ir.Dependency{},
		Exported:     map[string]*ir.Symbol{},
		Globals:      map[string]*ir.Symbol{},
	}
}

func openAndParse(s *state, filename string) (*ir.Node, *Error) {
	text, e := ioutil.ReadFile(s.BaseFolder + "/" + filename)
	if e != nil {
		return nil, processFileError(e)
	}

	n, err := parser.Parse(string(text))
	if err != nil {
		return nil, err
	}
	return n, nil
}

func findFile(s *state, modID string) (string, *Error) {
	found := []string{}
	for _, filename := range s.FilesInFolder {
		if strings.HasPrefix(filename, modID+".") {
			found = append(found, filename)
		}
	}
	if len(found) > 1 {
		return "", msg.AmbiguousFilesInFolder(s.RefModule, s.RefNode, found, modID)
	}
	if len(found) == 0 {
		return "", msg.ModuleNotFound(s.RefModule, s.RefNode, s.BaseFolder, modID)
	}
	return found[0], nil
}

func processFileError(e error) *Error {
	return &Error{
		Type:  et.FileError,
		Debug: e.Error(),
	}
}

func checkDependencyCycles(M *ir.Module) *Error {
	for _, dep := range M.Dependencies {
		prev := []*ir.Dependency{}
		err := checkDepCycle(M, dep, prev)
		if err != nil {
			return err
		}
	}
	return nil
}

func checkDepCycle(M *ir.Module, d *ir.Dependency, prev []*ir.Dependency) *Error {
	if depHasVisited(prev, d) {
		return msg.ErrorInvalidDependencyCycle(M, prev, d)
	}
	prev = append(prev, d)
	top := len(prev)
	for _, dep := range d.M.Dependencies {
		err := checkDepCycle(M, dep, prev[0:top])
		if err != nil {
			return err
		}
	}
	return nil
}

func depHasVisited(visited []*ir.Dependency, b *ir.Dependency) bool {
	for _, v := range visited {
		if b.M == v.M {
			return true
		}
	}
	return false
}
