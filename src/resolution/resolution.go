package resolution

import (
	"fmt"
	"io/ioutil"
	"strings"

	. "mpc/core"
	et "mpc/core/errorkind"
	ir "mpc/core/module"
	GK "mpc/core/module/globalkind"
	lex "mpc/core/module/lexkind"
	"mpc/format"
	"mpc/lexer"
	msg "mpc/messages"
	"mpc/parser"
)

// _fmt set to true will format every file from AST before parsing again
func Resolve(filePath string, _fmt bool) (*ir.Module, *Error) {
	name, err := extractName(filePath)
	if err != nil {
		return nil, err
	}

	s, ioerr := newState(filePath, _fmt)
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
	err = resolveNames(m)
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

	_fmt bool
}

func newState(fullPath string, _fmt bool) (*state, error) {
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
		_fmt:          _fmt,
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
	if coupling.Lex != lex.COUPLINGS {
		panic("resolver: resolveDependencies: bad node")
	}
	s.RefModule = mod
	for _, n := range coupling.Leaves {
		switch n.Lex {
		case lex.IMPORT:
			err := multiImport(s, n, mod)
			if err != nil {
				return err
			}
		case lex.FROM:
			err := fromImport(s, n, mod)
			if err != nil {
				return err
			}
		case lex.EXPORT:
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
		err.Location = place(n, dependentMod)
		return err
	}

	return addDependency(dependentMod, mod, n, modID)
}

func multiImport(s *state, n *ir.Node, dependentMod *ir.Module) *Error {
	if n.Lex != lex.IMPORT {
		panic("resolver: singleImport: bad node")
	}

	items := n.Leaves[0]
	if items.Lex == lex.ALL {
		return msg.CantImportAll(dependentMod, items)
	}

	for _, n := range items.Leaves {
		s.RefNode = n

		impName := ""
		name := ""
		if n.Lex == lex.AS {
			impName = n.Leaves[0].Text
			name = n.Leaves[1].Text
		} else {
			impName = n.Text
			name = n.Text
		}
		mod, err := resolveModule(s, impName)
		if err != nil {
			err.Location = place(n, dependentMod)
			return err
		}

		err = addDependency(dependentMod, mod, n, name)
		if err != nil {
			return err
		}
	}
	return nil
}

func addDependency(parent *ir.Module, dependency *ir.Module, source *ir.Node, name string) *Error {
	_, ok := parent.Dependencies[name]
	if ok {
		return msg.ErrorNameAlreadyDefined(parent, source, name)
	}
	parent.Dependencies[name] = &ir.Dependency{M: dependency, Source: source}
	return nil
}

func getFolder(fullpath string) string {
	path := strings.Split(fullpath, "/")
	if len(path) == 1 {
		return ""
	}
	folder := strings.Join(path[:len(path)-1], "/")
	return folder
}

func place(n *ir.Node, mod *ir.Module) *Location {
	return &Location{
		Range: n.Range,
		File:  mod.FullPath,
	}
}

func extractName(filePath string) (string, *Error) {
	path := strings.Split(filePath, "/")
	name := strings.Split(path[len(path)-1], ".")
	if lexer.IsValidIdentifier(name[0]) {
		return name[0], nil
	}
	return "", &Error{
		Code:    et.InvalidFileName,
		Message: filePath + " : " + name[0],
	}
}

func invalidModuleName(filePath string) *Error {
	return &Error{
		Code:    et.InvalidFileName,
		Message: filePath,
	}
}

func newModule(basePath, modID, filename string, root *ir.Node) *ir.Module {
	return &ir.Module{
		BasePath:     basePath,
		Name:         modID,
		FullPath:     basePath + "/" + filename,
		Root:         root,
		Dependencies: map[string]*ir.Dependency{},
		Exported:     map[string]*ir.Global{},
		Globals:      map[string]*ir.Global{},
	}
}

func openAndParse(s *state, filename string) (*ir.Node, *Error) {
	path := s.BaseFolder + "/" + filename
	text, e := ioutil.ReadFile(path)
	if e != nil {
		return nil, processFileError(e)
	}

	n, err := parser.Parse(path, string(text))
	if err != nil {
		return nil, err
	}

	if s._fmt {
		text := format.Format(n)
		n, err = parser.Parse(path, string(text))
		if err != nil {
			return nil, err
		}
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
		Code:    et.FileError,
		Message: e.Error(),
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

func resolveNames(M *ir.Module) *Error {
	err := resolve(M)
	if err != nil {
		return err
	}
	M.ResetVisited()
	return nil
}

func resolve(M *ir.Module) *Error {
	if M.Visited {
		return nil
	}
	M.Visited = true
	for _, dep := range M.Dependencies {
		err := resolve(dep.M)
		if err != nil {
			return err
		}
	}

	err := createImportedSymbols(M)
	if err != nil {
		return err
	}

	err = createGlobals(M)
	if err != nil {
		return err
	}

	err = resolveGlobalDepGraph(M)
	if err != nil {
		return err
	}

	err = checkGlobalCycles(M)
	if err != nil {
		return err
	}

	err = checkExports(M)
	if err != nil {
		return err
	}

	return nil
}

func createGlobals(M *ir.Module) *Error {
	symbols := M.Root.Leaves[1]
	for _, symbol := range symbols.Leaves {
		err := declareSymbol(M, symbol)
		if err != nil {
			return err
		}
	}
	return nil
}

func importSymbols(M *ir.Module, n *ir.Node) *Error {
	items := n.Leaves[0]
	for _, m := range items.Leaves {
		impName := ""
		name := ""
		if m.Lex == lex.AS {
			impName = m.Leaves[0].Text
			name = m.Leaves[1].Text
		} else {
			impName = m.Text
			name = m.Text
		}
		err := defineModSymbol(M, m, impName, name)
		if err != nil {
			return err
		}
	}
	return nil
}

func defineModSymbol(M *ir.Module, n *ir.Node, impName, name string) *Error {
	sy := &ir.Global{
		Name:       impName,
		Kind:       GK.Module,
		ModuleName: M.Name,
		N:          n,
	}
	_, ok := M.Globals[name]
	if ok {
		return msg.ErrorNameAlreadyDefined(M, n, name)
	}
	M.Globals[name] = sy
	return nil
}

func createImportedSymbols(M *ir.Module) *Error {
	coupling := M.Root.Leaves[0]
	for _, n := range coupling.Leaves {
		switch n.Lex {
		case lex.IMPORT:
			err := importSymbols(M, n)
			if err != nil {
				return err
			}
		case lex.FROM:
			err := fromImportSymbols(M, n)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func fromImportSymbols(M *ir.Module, n *ir.Node) *Error {
	mod := n.Leaves[0].Text
	dep, ok := M.Dependencies[mod]
	if !ok {
		panic("dependency should have been found")
	}
	items := n.Leaves[1]
	if items.Lex == lex.ALL {
		for name, sy := range dep.M.Exported {
			err := defineExternalSymbol(M, items, sy, name, name)
			if err != nil {
				return err
			}
		}
	} else {
		for _, item := range items.Leaves {
			impName := ""
			name := ""
			if item.Lex == lex.AS {
				impName = item.Leaves[0].Text
				name = item.Leaves[1].Text
			} else {
				impName = item.Text
				name = item.Text
			}
			sy, ok := dep.M.Exported[impName]
			if !ok {
				fmt.Println(":", impName, name)
				return msg.NameNotExported(M, item)
			}
			err := defineExternalSymbol(M, item, sy, sy.Name, name)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func defineExternalSymbol(M *ir.Module, n *ir.Node, sy *ir.Global, impName, name string) *Error {
	newSy := &ir.Global{
		Kind:       sy.Kind,
		Name:       impName,
		N:          sy.N,
		External:   true,
		ModuleName: sy.ModuleName,
		Proc:       sy.Proc,
		Data:       sy.Data,
		Struct:     sy.Struct,
		Const:      sy.Const,
	}
	_, ok := M.Globals[name]
	if ok {
		return msg.ErrorNameAlreadyDefined(M, n, name)
	}
	M.Globals[name] = newSy
	return nil
}

func checkExports(M *ir.Module) *Error {
	// check if there are duplicated exports
	// look at global symbols and check if they exist
	// insert into the export list
	type export struct {
		impName string
		node    *ir.Node
	}
	coupling := M.Root.Leaves[0]
	exported := map[string]export{}
	for _, exp := range coupling.Leaves {
		if exp.Lex == lex.EXPORT {
			items := exp.Leaves[0]
			if items.Lex == lex.ALL {
				for name, sy := range M.Globals {
					if !sy.External {
						_, ok := exported[name]
						if ok {
							return msg.ErrorDuplicatedExport(M, items)
						}
						exported[name] = export{
							node:    items,
							impName: name,
						}
					}
				}
			} else {
				for _, item := range items.Leaves {
					impName := ""
					name := ""
					if item.Lex == lex.AS {
						impName = item.Leaves[0].Text
						name = item.Leaves[1].Text
					} else {
						impName = item.Text
						name = item.Text
					}
					_, ok := exported[name]
					if ok {
						return msg.ErrorDuplicatedExport(M, item)
					}
					exported[name] = export{
						impName: impName,
						node:    item,
					}
				}
			}
		}
	}
	for name, exp := range exported {
		sy, ok := M.Globals[exp.impName]
		if !ok {
			return msg.ErrorExportingUndefName(M, exp.node)
		}
		M.Exported[name] = sy
	}
	return nil
}

func declareSymbol(M *ir.Module, n *ir.Node) *Error {
	switch n.Lex {
	case lex.PROC:
		return declProcSymbol(M, n)
	case lex.DATA:
		return declMemSymbol(M, n)
	case lex.CONST:
		return declConstSymbol(M, n)
	case lex.STRUCT:
		return declStructSymbol(M, n)
	default:
		panic("impossible")
	}
}

func declProcSymbol(M *ir.Module, n *ir.Node) *Error {
	sy := getProcSymbol(M, n)
	_, ok := M.Globals[sy.Name]
	if ok {
		return msg.ErrorNameAlreadyDefined(M, n, sy.Name)
	}
	M.Globals[sy.Name] = sy
	return nil
}

func declMemSymbol(M *ir.Module, n *ir.Node) *Error {
	leaf := n.Leaves[0]
	switch leaf.Lex {
	case lex.BEGIN:
		for _, single := range leaf.Leaves {
			err := setMemSymbol(M, single)
			if err != nil {
				return err
			}
		}
		return nil
	case lex.SINGLE:
		return setMemSymbol(M, leaf)
	default:
		panic("impossible")
	}
}

func declConstSymbol(M *ir.Module, n *ir.Node) *Error {
	leaf := n.Leaves[0]
	switch leaf.Lex {
	case lex.BEGIN:
		for _, single := range leaf.Leaves {
			err := setConstSymbol(M, single)
			if err != nil {
				return err
			}
		}
		return nil
	case lex.SINGLE:
		return setConstSymbol(M, leaf)
	default:
		panic("impossible")
	}
}

func getProcSymbol(M *ir.Module, n *ir.Node) *ir.Global {
	name := n.Leaves[0].Text
	return &ir.Global{
		Kind:       GK.Proc,
		Name:       name,
		ModuleName: M.Name,
		Proc: &ir.Proc{
			Name:   name,
			ArgMap: map[string]ir.PositionalLocal{},
			Vars:   map[string]ir.PositionalLocal{},
			N:      n,
		},
		N: n,
	}
}

func setMemSymbol(M *ir.Module, n *ir.Node) *Error {
	name := n.Leaves[0].Text
	mem := &ir.Data{
		Name: name,
		Init: n.Leaves[2],
	}
	sy := &ir.Global{
		Kind:       GK.Data,
		Name:       name,
		ModuleName: M.Name,
		Data:       mem,
		N:          n,
	}
	_, ok := M.Globals[sy.Name]
	if ok {
		return msg.ErrorNameAlreadyDefined(M, n, sy.Name)
	}
	M.Globals[sy.Name] = sy
	return nil
}

func setConstSymbol(M *ir.Module, n *ir.Node) *Error {
	name := n.Leaves[0].Text
	sy := &ir.Global{
		Kind:       GK.Const,
		Name:       name,
		ModuleName: M.Name,
		N:          n,
		Const: &ir.Const{
			Value: nil,
		},
	}
	_, ok := M.Globals[sy.Name]
	if ok {
		return msg.ErrorNameAlreadyDefined(M, n, sy.Name)
	}
	M.Globals[sy.Name] = sy
	return nil
}

func declStructSymbol(M *ir.Module, n *ir.Node) *Error {
	id := n.Leaves[0].Text
	sy := &ir.Global{
		Kind:       GK.Struct,
		Name:       id,
		ModuleName: M.Name,
		N:          n,
		Struct: &ir.Struct{
			Fields:   []ir.Field{},
			FieldMap: map[string]int{},
		},
	}
	fields := n.Leaves[2]
	i := 0
	for _, field := range fields.Leaves {
		idList := field.Leaves[0]
		offset := field.Leaves[2]
		if len(idList.Leaves) > 1 && offset != nil {
			return msg.ErrorOffsetInMultipleFields(M, field)
		}
		for _, id := range idList.Leaves {
			if _, ok := sy.Struct.FieldMap[id.Text]; ok {
				return msg.ErrorNameAlreadyDefined(M, id, id.Text)
			}
			field := ir.Field{
				Name:   id.Text,
				Refs:   ir.Refs{},
				Offset: offset,
			}
			sy.Struct.Fields = append(sy.Struct.Fields, field)
			sy.Struct.FieldMap[id.Text] = i
			i++
		}
	}
	_, ok := M.Globals[sy.Name]
	if ok {
		return msg.ErrorNameAlreadyDefined(M, n, sy.Name)
	}
	M.Globals[sy.Name] = sy
	return nil
}

func resolveGlobalDepGraph(M *ir.Module) *Error {
	for _, sy := range M.Globals {
		if sy.Kind == GK.Const {
			err := resDepExpr(M, ir.FromSymbol(sy), sy.N.Leaves[2])
			if err != nil {
				return err
			}
		}
		if sy.Kind == GK.Data {
			contents := sy.N.Leaves[2]
			var err *Error
			switch contents.Lex {
			case lex.STRING_LIT:
			case lex.BLOB:
				err = resBlobExpr(M, sy, contents)
			default:
				err = resDepExpr(M, ir.FromSymbol(sy), contents)
			}
			if err != nil {
				return err
			}
		}
		if sy.Kind == GK.Struct {
			err := resStruct(M, sy)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func resBaseType(M *ir.Module, sy *ir.Global, n *ir.Node) *Error {
	if n.Lex == lex.IDENTIFIER {
		return resDepID(M, ir.FromSymbol(sy), n, true)
	}
	return nil
}

func resStruct(M *ir.Module, sy *ir.Global) *Error {
	fields := sy.N.Leaves[2]
	for _, field := range fields.Leaves {
		idList := field.Leaves[0]
		expr := field.Leaves[2]
		if expr != nil {
			for _, id := range idList.Leaves {
				field := sy.Struct.FieldMap[id.Text]
				sf := ir.SyField{
					Sy:    sy,
					Field: field,
				}
				err := resDepExpr(M, sf, expr)
				if err != nil {
					return err
				}
			}
		}
	}
	return nil
}

func resBlobExpr(M *ir.Module, sy *ir.Global, n *ir.Node) *Error {
	blobContents := n.Leaves[1]
	for _, leaf := range blobContents.Leaves {
		err := resDepExpr(M, ir.FromSymbol(sy), leaf)
		if err != nil {
			return err
		}
	}
	return nil
}

func resDepExpr(M *ir.Module, sy ir.SyField, n *ir.Node) *Error {
	switch n.Lex {
	case lex.STRING_LIT:
		return msg.CannotUseStringInExpr(M, n)
	case lex.CALL, lex.AT:
		return msg.NonConstExpr(M, n)
	case lex.I64_LIT, lex.I32_LIT, lex.I16_LIT, lex.I8_LIT,
		lex.U64_LIT, lex.U32_LIT, lex.U16_LIT, lex.U8_LIT,
		lex.FALSE, lex.TRUE, lex.PTR_LIT, lex.CHAR_LIT:
		return nil // nothing to resolve here
	case lex.SIZEOF:
		return resSizeof(M, sy, n)
	case lex.DOUBLECOLON:
		return resExternalID(M, n, true)
	case lex.DOT:
		panic("unimplemented")
	case lex.IDENTIFIER:
		return resDepID(M, sy, n, true)
	case lex.NEG, lex.BITWISENOT, lex.NOT:
		return resDepExpr(M, sy, n.Leaves[0])
	case lex.PLUS, lex.MINUS, lex.MULTIPLICATION,
		lex.DIVISION, lex.REMAINDER, lex.BITWISEAND,
		lex.BITWISEXOR, lex.BITWISEOR, lex.SHIFTLEFT,
		lex.SHIFTRIGHT, lex.EQUALS, lex.DIFFERENT,
		lex.MORE, lex.MOREEQ, lex.LESS, lex.LESSEQ,
		lex.AND, lex.OR:
		err := resDepExpr(M, sy, n.Leaves[0])
		if err != nil {
			return err
		}
		return resDepExpr(M, sy, n.Leaves[1])
	case lex.COLON:
		return resDepExpr(M, sy, n.Leaves[1])
	}
	return nil
}

func resSizeof(M *ir.Module, sy ir.SyField, n *ir.Node) *Error {
	op := n.Leaves[0]
	dot := n.Leaves[1]
	if dot == nil {
		switch op.Lex {
		case lex.IDENTIFIER:
			return resDepID(M, sy, op, false)
		case lex.DOUBLECOLON:
			return resExternalID(M, op, false)
		default:
			return nil
		}
	}
	// if dot is not nil, we don't care, fields have static sizes,
	// ie, they have a basic type size.
	return nil
}

func resDotExpr(M *ir.Module, sy *ir.SyField, n *ir.Node) *Error {
	field := n.Leaves[0]
	expr := n.Leaves[1]

	if expr.Lex == lex.IDENTIFIER {
		tsy := M.GetSymbol(expr.Text)
		if tsy == nil {
			return msg.ErrorNameNotDefined(M, expr)
		}
		if tsy.Kind != GK.Struct {
			return msg.ErrorExpectedStruct(M, expr)
		}
		if !tsy.External {
			fieldIndex, ok := tsy.Struct.FieldMap[field.Text]
			if !ok {
				return msg.ErrorNameNotDefined(M, field)
			}
			sy.LinkField(tsy, fieldIndex)
		}
		return nil
	}
	return nil
}

func getSymbolFromExpr(M *ir.Module, n *ir.Node) *ir.Global {
	switch n.Lex {
	case lex.IDENTIFIER:
		return M.GetSymbol(n.Text)
	case lex.DOUBLECOLON:
		return M.GetExternalSymbol(n.Leaves[0].Text, n.Leaves[1].Text)
	}
	return nil
}

func resExternalID(M *ir.Module, n *ir.Node, disallow bool) *Error {
	module := n.Leaves[0].Text
	name := n.Leaves[1].Text
	sy := M.GetExternalSymbol(module, name)
	if sy.Kind != GK.Const && disallow {
		return msg.NonConstExpr(M, n)
	}
	return nil
}

func resDepID(M *ir.Module, sy ir.SyField, n *ir.Node, disallow bool) *Error {
	other := M.GetSymbol(n.Text)
	if other == nil {
		return msg.ErrorNameNotDefined(M, n)
	}
	if other.Kind != GK.Const && disallow {
		// we can't allow procedures and data declarations arbitrarely,
		// since we don't know the addresses yet.
		return msg.NonConstExpr(M, n)
	}
	if !other.External {
		sy.Link(other)
	}
	return nil
}

func checkGlobalCycles(M *ir.Module) *Error {
	for _, sy := range M.Globals {
		prev := []ir.SyField{}
		err := checkSymbolCycle(M, ir.FromSymbol(sy), prev)
		if err != nil {
			return err
		}
	}
	return nil
}

func checkSymbolCycle(M *ir.Module, d ir.SyField, prev []ir.SyField) *Error {
	if syHasVisited(prev, d) {
		return msg.ErrorInvalidSymbolCycle(M, prev, d)
	}
	prev = append(prev, d)
	if d.IsField() {
		field := d.Sy.Struct.Fields[d.Field]
		return checkRefs(M, field.Refs, prev)
	} else {
		return checkRefs(M, d.Sy.Refs, prev)
	}
}

func checkRefs(M *ir.Module, refs ir.Refs, prev []ir.SyField) *Error {
	top := len(prev)
	for _, ref := range refs.Symbols {
		err := checkSymbolCycle(M, ref, prev[0:top])
		if err != nil {
			return err
		}
	}
	return nil
}

func syHasVisited(visited []ir.SyField, b ir.SyField) bool {
	for _, v := range visited {
		if b.Sy.Name == v.Sy.Name &&
			b.Field == v.Field {
			return true
		}
	}
	return false
}
