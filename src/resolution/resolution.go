package resolution

import (
	"fmt"
	"io/ioutil"
	"strings"

	EK "mpc/core/errorkind"
	GK "mpc/core/module/globalkind"
	LK "mpc/core/module/lexkind"

	. "mpc/core"
	mod "mpc/core/module"
	"mpc/format"
	"mpc/lexer"
	msg "mpc/messages"
	"mpc/parser"
)

// _fmt set to true will format every file from AST before parsing again
func Resolve(filePath string, _fmt bool) (*mod.Module, *Error) {
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
	Modules       map[string]*mod.Module
	BaseFolder    string
	FilesInFolder []string

	RefNode   *mod.Node // for errors
	RefModule *mod.Module

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
		Modules:       map[string]*mod.Module{},
		BaseFolder:    folder,
		FilesInFolder: filenames,
		_fmt:          _fmt,
	}, nil
}

func resolveModule(s *state, modID string) (*mod.Module, *Error) {
	module, ok := s.Modules[modID]
	if ok {
		return module, nil
	}
	fileName, err := findFile(s, modID)
	if err != nil {
		return nil, err
	}
	n, err := openAndParse(s, fileName)
	if err != nil {
		return nil, err
	}
	module = newModule(s.BaseFolder, modID, fileName, n)
	s.Modules[modID] = module

	err = resolveDependencies(s, n.Leaves[0], module)
	if err != nil {
		return nil, err
	}
	return module, nil
}

func resolveDependencies(s *state, coupling *mod.Node, module *mod.Module) *Error {
	if coupling.Lex != LK.COUPLINGS {
		panic("resolver: resolveDependencies: bad node")
	}
	s.RefModule = module
	for _, n := range coupling.Leaves {
		switch n.Lex {
		case LK.IMPORT:
			err := multiImport(s, n, module)
			if err != nil {
				return err
			}
		case LK.FROM:
			err := fromImport(s, n, module)
			if err != nil {
				return err
			}
		case LK.EXPORT:
			// must run after the symbol resolution
		}
	}
	return nil
}

func fromImport(s *state, n *mod.Node, dependentMod *mod.Module) *Error {
	modID := n.Leaves[0].Text
	s.RefNode = n.Leaves[0]

	mod, err := resolveModule(s, modID)
	if err != nil {
		err.Location = place(n, dependentMod)
		return err
	}

	return addDependency(dependentMod, mod, n, modID)
}

func multiImport(s *state, n *mod.Node, dependentMod *mod.Module) *Error {
	if n.Lex != LK.IMPORT {
		panic("resolver: singleImport: bad node")
	}

	items := n.Leaves[0]
	if items.Lex == LK.ALL {
		return msg.CantImportAll(dependentMod, items)
	}

	for _, n := range items.Leaves {
		s.RefNode = n

		impName := ""
		name := ""
		if n.Lex == LK.AS {
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

func addDependency(parent *mod.Module, dependency *mod.Module, source *mod.Node, name string) *Error {
	_, ok := parent.Dependencies[name]
	if ok {
		return msg.ErrorNameAlreadyDefined(parent, source, name)
	}
	parent.Dependencies[name] = &mod.Dependency{M: dependency, Source: source}
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

func place(n *mod.Node, module *mod.Module) *Location {
	return &Location{
		Range: n.Range,
		File:  module.FullPath,
	}
}

func extractName(filePath string) (string, *Error) {
	path := strings.Split(filePath, "/")
	name := strings.Split(path[len(path)-1], ".")
	if lexer.IsValidIdentifier(name[0]) {
		return name[0], nil
	}
	return "", &Error{
		Code:    EK.InvalidFileName,
		Message: filePath + " : " + name[0],
	}
}

func invalidModuleName(filePath string) *Error {
	return &Error{
		Code:    EK.InvalidFileName,
		Message: filePath,
	}
}

func newModule(basePath, modID, filename string, root *mod.Node) *mod.Module {
	return &mod.Module{
		BasePath:     basePath,
		Name:         modID,
		FullPath:     basePath + "/" + filename,
		Root:         root,
		Dependencies: map[string]*mod.Dependency{},
		Exported:     map[string]*mod.Global{},
		Globals:      map[string]*mod.Global{},
	}
}

func openAndParse(s *state, filename string) (*mod.Node, *Error) {
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
		Code:    EK.FileError,
		Message: e.Error(),
	}
}

func checkDependencyCycles(M *mod.Module) *Error {
	for _, dep := range M.Dependencies {
		prev := []*mod.Dependency{}
		err := checkDepCycle(M, dep, prev)
		if err != nil {
			return err
		}
	}
	return nil
}

func checkDepCycle(M *mod.Module, d *mod.Dependency, prev []*mod.Dependency) *Error {
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

func depHasVisited(visited []*mod.Dependency, b *mod.Dependency) bool {
	for _, v := range visited {
		if b.M == v.M {
			return true
		}
	}
	return false
}

func resolveNames(M *mod.Module) *Error {
	err := resolve(M)
	if err != nil {
		return err
	}
	M.ResetVisited()
	return nil
}

func resolve(M *mod.Module) *Error {
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

func createGlobals(M *mod.Module) *Error {
	symbols := M.Root.Leaves[1]
	for _, symbol := range symbols.Leaves {
		err := declareSymbol(M, symbol)
		if err != nil {
			return err
		}
	}
	return nil
}

func importSymbols(M *mod.Module, n *mod.Node) *Error {
	items := n.Leaves[0]
	for _, m := range items.Leaves {
		impName := ""
		name := ""
		if m.Lex == LK.AS {
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

func defineModSymbol(M *mod.Module, n *mod.Node, impName, name string) *Error {
	sy := &mod.Global{
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

func createImportedSymbols(M *mod.Module) *Error {
	coupling := M.Root.Leaves[0]
	for _, n := range coupling.Leaves {
		switch n.Lex {
		case LK.IMPORT:
			err := importSymbols(M, n)
			if err != nil {
				return err
			}
		case LK.FROM:
			err := fromImportSymbols(M, n)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func fromImportSymbols(M *mod.Module, n *mod.Node) *Error {
	mod := n.Leaves[0].Text
	dep, ok := M.Dependencies[mod]
	if !ok {
		panic("dependency should have been found")
	}
	items := n.Leaves[1]
	if items.Lex == LK.ALL {
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
			if item.Lex == LK.AS {
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

func defineExternalSymbol(M *mod.Module, n *mod.Node, sy *mod.Global, impName, name string) *Error {
	newSy := &mod.Global{
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

func checkExports(M *mod.Module) *Error {
	// check if there are duplicated exports
	// look at global symbols and check if they exist
	// insert into the export list
	type export struct {
		impName string
		node    *mod.Node
	}
	coupling := M.Root.Leaves[0]
	exported := map[string]export{}
	for _, exp := range coupling.Leaves {
		if exp.Lex == LK.EXPORT {
			items := exp.Leaves[0]
			if items.Lex == LK.ALL {
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
					if item.Lex == LK.AS {
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

func declareSymbol(M *mod.Module, n *mod.Node) *Error {
	sy := n
	idlist := []string{}
	if n.Lex == LK.ATTR {
		var ok bool
		idlist, ok = idlistToStr(n.Leaves[0])
		if !ok {
			return msg.InvalidFlag(M, n)
		}
		sy = n.Leaves[1]
	}
	switch sy.Lex {
	case LK.PROC:
		return declProcSymbol(M, sy, idlist)
	case LK.DATA:
		return declMemSymbol(M, sy, idlist)
	case LK.CONST:
		return declConstSymbol(M, sy, idlist)
	case LK.STRUCT:
		return declStructSymbol(M, sy, idlist)
	default:
		panic("impossible")
	}
}

func idlistToStr(list *mod.Node) ([]string, bool) {
	out := make([]string, len(list.Leaves))
	for i, id := range list.Leaves {
		if !validFlag(id.Text) {
			return nil, false
		}
		out[i] = id.Text
	}
	return out, true
}

func validFlag(flag string) bool {
	switch flag {
	case "pedantic", "rec_pedantic", "align_pack", "c_pad":
		return true
	}
	return false
}

func declProcSymbol(M *mod.Module, n *mod.Node, idlist []string) *Error {
	sy := getProcSymbol(M, n, idlist)
	_, ok := M.Globals[sy.Name]
	if ok {
		return msg.ErrorNameAlreadyDefined(M, n, sy.Name)
	}
	M.Globals[sy.Name] = sy
	return nil
}

func declMemSymbol(M *mod.Module, n *mod.Node, idlist []string) *Error {
	leaf := n.Leaves[0]
	switch leaf.Lex {
	case LK.BEGIN:
		for _, single := range leaf.Leaves {
			err := setMemSymbol(M, single, idlist)
			if err != nil {
				return err
			}
		}
		return nil
	case LK.SINGLE:
		return setMemSymbol(M, leaf, idlist)
	default:
		panic("impossible")
	}
}

func declConstSymbol(M *mod.Module, n *mod.Node, idlist []string) *Error {
	leaf := n.Leaves[0]
	switch leaf.Lex {
	case LK.BEGIN:
		for _, single := range leaf.Leaves {
			err := setConstSymbol(M, single, idlist)
			if err != nil {
				return err
			}
		}
		return nil
	case LK.SINGLE:
		return setConstSymbol(M, leaf, idlist)
	default:
		panic("impossible")
	}
}

func getProcSymbol(M *mod.Module, n *mod.Node, idlist []string) *mod.Global {
	name := n.Leaves[0].Text
	return &mod.Global{
		Kind:       GK.Proc,
		Name:       name,
		ModuleName: M.Name,
		Attr:       idlist,
		Proc: &mod.Proc{
			Name:   name,
			ArgMap: map[string]mod.PositionalLocal{},
			Vars:   map[string]mod.PositionalLocal{},
			N:      n,
		},
		N: n,
	}
}

func setMemSymbol(M *mod.Module, n *mod.Node, idlist []string) *Error {
	name := n.Leaves[0].Text
	mem := &mod.Data{
		Name: name,
		Init: n.Leaves[2],
	}
	sy := &mod.Global{
		Kind:       GK.Data,
		Name:       name,
		ModuleName: M.Name,
		Data:       mem,
		Attr:       idlist,
		N:          n,
	}
	_, ok := M.Globals[sy.Name]
	if ok {
		return msg.ErrorNameAlreadyDefined(M, n, sy.Name)
	}
	M.Globals[sy.Name] = sy
	return nil
}

func setConstSymbol(M *mod.Module, n *mod.Node, idlist []string) *Error {
	name := n.Leaves[0].Text
	sy := &mod.Global{
		Kind:       GK.Const,
		Name:       name,
		ModuleName: M.Name,
		N:          n,
		Attr:       idlist,
		Const: &mod.Const{
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

func declStructSymbol(M *mod.Module, n *mod.Node, idlist []string) *Error {
	id := n.Leaves[0].Text
	sy := &mod.Global{
		Kind:       GK.Struct,
		Name:       id,
		ModuleName: M.Name,
		N:          n,
		Attr:       idlist,
		Struct: &mod.Struct{
			Fields:   []mod.Field{},
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
			field := mod.Field{
				Name:   id.Text,
				Refs:   mod.Refs{},
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

func resolveGlobalDepGraph(M *mod.Module) *Error {
	for _, sy := range M.Globals {
		if sy.External {
			continue
		}
		if sy.Kind == GK.Const {
			err := resExpr(M, mod.FromSymbol(sy), sy.N.Leaves[2])
			if err != nil {
				return err
			}
		}
		if sy.Kind == GK.Data {
			err := resData(M, sy)
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
		if sy.Kind == GK.Proc {
			err := resProc(M, sy)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func resBaseType(M *mod.Module, sy *mod.Global, n *mod.Node) *Error {
	if n.Lex == LK.IDENTIFIER {
		return resDepID(M, mod.FromSymbol(sy), n, true)
	}
	return nil
}

func resStruct(M *mod.Module, sy *mod.Global) *Error {
	size := sy.N.Leaves[1]
	if size != nil {
		err := resExpr(M, mod.FromSymbol(sy), size)
		if err != nil {
			return err
		}
	}
	fields := sy.N.Leaves[2]
	for _, field := range fields.Leaves {
		idList := field.Leaves[0]
		offset := field.Leaves[2]
		if offset != nil {
			for _, id := range idList.Leaves {
				field := sy.Struct.FieldMap[id.Text]
				sf := mod.SyField{
					Sy:    sy,
					Field: field,
				}
				err := resExpr(M, sf, offset)
				if err != nil {
					return err
				}
			}
		}
	}
	return nil
}

func resProc(M *mod.Module, sy *mod.Global) *Error {
	body := sy.N.Leaves[4]
	// this code is just trying to find each
	// const expression in the asm code :)
	if body.Lex == LK.ASM {
		lines := body.Leaves[0]
		for _, line := range lines.Leaves {
			if line.Lex == LK.INSTR {
				opList := line.Leaves[1]
				err := resOpList(M, sy, opList)
				if err != nil {
					return err
				}
			}
		}
	}
	return nil
}

func resOpList(M *mod.Module, sy *mod.Global, opList *mod.Node) *Error {
	for _, op := range opList.Leaves {
		if op.Lex == LK.LEFTBRACE {
			expr := op.Leaves[0]
			err := resExpr(M, mod.FromSymbol(sy), expr)
			if err != nil {
				return err
			}
		}
		if op.Lex == LK.LEFTBRACKET {
			innerList := op.Leaves[0]
			err := resOpList(M, sy, innerList)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func resData(M *mod.Module, sy *mod.Global) *Error {
	annot := sy.N.Leaves[1]
	contents := sy.N.Leaves[2]
	if annot != nil {
		t := annot.Leaves[0]
		if t.Lex == LK.IDENTIFIER { // we don't care about the case '::'
			err := resDepID(M, mod.FromSymbol(sy), t, false)
			if err != nil {
				return err
			}
		}
	}
	var err *Error
	if contents != nil {
		switch contents.Lex {
		case LK.STRING_LIT:
		case LK.BLOB:
			err = resBlobExpr(M, sy, contents)
		default:
			err = resExpr(M, mod.FromSymbol(sy), contents)
		}

		if err != nil {
			return err
		}
	} else {
		if annot == nil {
			return msg.InvalidDataDecl(M, sy.N)
		}
	}
	return nil
}

func resBlobExpr(M *mod.Module, sy *mod.Global, n *mod.Node) *Error {
	blobContents := n.Leaves[1]
	for _, leaf := range blobContents.Leaves {
		err := resExpr(M, mod.FromSymbol(sy), leaf)
		if err != nil {
			return err
		}
	}
	return nil
}

func resExpr(M *mod.Module, sy mod.SyField, n *mod.Node) *Error {
	switch n.Lex {
	case LK.IDENTIFIER:
		return resDepID(M, sy, n, true)
	case LK.DOUBLECOLON:
		return resExternalID(M, n, true)
	case LK.DOT:
		return resDotExpr(M, sy, n)
	case LK.STRING_LIT:
		return msg.CannotUseStringInExpr(M, n)
	case LK.CALL, LK.AT:
		return msg.NonConstExpr(M, n)
	case LK.I64_LIT, LK.I32_LIT, LK.I16_LIT, LK.I8_LIT,
		LK.U64_LIT, LK.U32_LIT, LK.U16_LIT, LK.U8_LIT,
		LK.FALSE, LK.TRUE, LK.PTR_LIT, LK.CHAR_LIT:
		return nil // nothing to resolve here
	case LK.SIZEOF:
		return resSizeof(M, sy, n)
	case LK.NEG, LK.BITWISENOT, LK.NOT:
		return resExpr(M, sy, n.Leaves[0])
	case LK.PLUS, LK.MINUS, LK.MULTIPLICATION,
		LK.DIVISION, LK.REMAINDER, LK.BITWISEAND,
		LK.BITWISEXOR, LK.BITWISEOR, LK.SHIFTLEFT,
		LK.SHIFTRIGHT, LK.EQUALS, LK.DIFFERENT,
		LK.MORE, LK.MOREEQ, LK.LESS, LK.LESSEQ,
		LK.AND, LK.OR:
		err := resExpr(M, sy, n.Leaves[0])
		if err != nil {
			return err
		}
		return resExpr(M, sy, n.Leaves[1])
	case LK.COLON:
		return resExpr(M, sy, n.Leaves[1])
	}
	return nil
}

func resSizeof(M *mod.Module, sy mod.SyField, n *mod.Node) *Error {
	op := n.Leaves[0]
	dot := n.Leaves[1]
	if dot == nil {
		switch op.Lex {
		case LK.IDENTIFIER:
			return resDepID(M, sy, op, false)
		case LK.DOUBLECOLON:
			return resExternalID(M, op, false)
		default:
			return nil
		}
	}
	// if dot is not nil, we don't care, fields have static sizes,
	// ie, they have a basic type size.
	return nil
}

func resDotExpr(M *mod.Module, sy mod.SyField, n *mod.Node) *Error {
	field := n.Leaves[0]
	expr := n.Leaves[1]

	if expr.Lex == LK.IDENTIFIER {
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

func getSymbolFromExpr(M *mod.Module, n *mod.Node) *mod.Global {
	switch n.Lex {
	case LK.IDENTIFIER:
		return M.GetSymbol(n.Text)
	case LK.DOUBLECOLON:
		return M.GetExternalSymbol(n.Leaves[0].Text, n.Leaves[1].Text)
	}
	return nil
}

func resExternalID(M *mod.Module, n *mod.Node, disallow bool) *Error {
	module := n.Leaves[0].Text
	name := n.Leaves[1].Text
	sy := M.GetExternalSymbol(module, name)
	if sy.Kind != GK.Const && disallow {
		return msg.NonConstExpr(M, n)
	}
	return nil
}

func resDepID(M *mod.Module, sy mod.SyField, n *mod.Node, disallow bool) *Error {
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

func checkGlobalCycles(M *mod.Module) *Error {
	for _, sy := range M.Globals {
		prev := []mod.SyField{}
		err := checkSymbolCycle(M, mod.FromSymbol(sy), prev)
		if err != nil {
			return err
		}
		if sy.Kind == GK.Struct {
			for i := range sy.Struct.Fields {
				sf := mod.SyField{
					Sy:    sy,
					Field: i,
				}
				err := checkSymbolCycle(M, sf, prev)
				if err != nil {
					return err
				}
			}
		}
	}
	return nil
}

func checkSymbolCycle(M *mod.Module, d mod.SyField, prev []mod.SyField) *Error {
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

func checkRefs(M *mod.Module, refs mod.Refs, prev []mod.SyField) *Error {
	top := len(prev)
	for _, ref := range refs.Symbols {
		err := checkSymbolCycle(M, ref, prev[0:top])
		if err != nil {
			return err
		}
	}
	return nil
}

func syHasVisited(visited []mod.SyField, b mod.SyField) bool {
	for _, v := range visited {
		if b.Sy.Name == v.Sy.Name &&
			b.Field == v.Field {
			return true
		}
	}
	return false
}
