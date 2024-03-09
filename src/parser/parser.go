package parser

import (
	"fmt"
	. "mpc/core"
	et "mpc/core/errorkind"
	mod "mpc/core/module"
	lex "mpc/core/module/lexkind"
	sv "mpc/core/severity"
	. "mpc/lexer"
)

func Parse(filename string, s string) (*mod.Node, *Error) {
	st := NewLexer(filename, s)
	err := st.Next()
	if err != nil {
		return nil, err
	}
	n, err := module(st)
	if err != nil {
		return nil, err
	}
	computeRanges(n)
	return n, nil
}

// Module := {Coupling} {AttrSymbol}.
func module(s *Lexer) (*mod.Node, *Error) {
	Track(s, "module")
	coupl, err := repeat(s, coupling)
	if err != nil {
		return nil, err
	}
	symb, err := repeat(s, attrSymbol)
	if err != nil {
		return nil, err
	}
	n := &mod.Node{}
	n.AddLeaf(createNode(coupl, lex.COUPLINGS))
	n.AddLeaf(createNode(symb, lex.SYMBOLS))
	if s.Word.Lex != lex.EOF {
		return nil, expectedEOF(s)
	}
	return n, nil
}

// Coupling := Import | FromImport | Export.
func coupling(s *Lexer) (*mod.Node, *Error) {
	switch s.Word.Lex {
	case lex.IMPORT:
		return _import(s)
	case lex.FROM:
		return _fromImport(s)
	case lex.EXPORT:
		return _export(s)
	}
	return nil, nil
}

// Import := 'import' Items.
func _import(s *Lexer) (*mod.Node, *Error) {
	kw, err := expect(s, lex.IMPORT)
	if err != nil {
		return nil, err
	}
	listNode, err := items(s)
	if err != nil {
		return nil, err
	}
	kw.AddLeaf(listNode)
	return kw, nil
}

// FromImport := 'from' id 'import' Items.
func _fromImport(s *Lexer) (*mod.Node, *Error) {
	kw, err := expect(s, lex.FROM)
	if err != nil {
		return nil, err
	}
	id, err := expect(s, lex.IDENTIFIER)
	if err != nil {
		return nil, err
	}

	_, err = expect(s, lex.IMPORT)
	if err != nil {
		return nil, err
	}

	listNode, err := items(s)
	if err != nil {
		return nil, err
	}
	kw.SetLeaves([]*mod.Node{id, listNode})
	return kw, nil
}

// Export := 'export' Items.
func _export(s *Lexer) (*mod.Node, *Error) {
	kw, err := expect(s, lex.EXPORT)
	if err != nil {
		return nil, err
	}
	leaf, err := items(s)
	if err != nil {
		return nil, err
	}
	kw.AddLeaf(leaf)
	return kw, nil
}

// Items := (AliasList | 'all')
func items(s *Lexer) (*mod.Node, *Error) {
	if s.Word.Lex == lex.ALL {
		return consume(s)
	} else {
		list, err := repeatCommaList(s, alias)
		if err != nil {
			return nil, err
		}
		listNode := &mod.Node{Lex: lex.ALIASLIST}
		listNode.SetLeaves(list)
		return listNode, nil
	}
}

// AliasList := Alias {',' Alias} [','].
// Items := (AliasList | 'all')
func alias(s *Lexer) (*mod.Node, *Error) {
	if s.Word.Lex == lex.IDENTIFIER {
		id, err := consume(s)
		if err != nil {
			return nil, err
		}
		if s.Word.Lex == lex.AS {
			kw, err := consume(s)
			if err != nil {
				return nil, err
			}
			id2, err := expect(s, lex.IDENTIFIER)
			if err != nil {
				return nil, err
			}
			kw.SetLeaves([]*mod.Node{id, id2})
			return kw, nil
		}
		return id, nil
	}
	return nil, nil
}

func attrSymbol(s *Lexer) (*mod.Node, *Error) {
	if s.Word.Lex == lex.ATTR {
		attr, err := consume(s)
		if err != nil {
			return nil, err
		}
		list, err := repeatCommaList(s, ident)
		if err != nil {
			return nil, err
		}
		listNode := &mod.Node{Lex: lex.IDLIST}
		listNode.SetLeaves(list)
		attr.AddLeaf(listNode)

		sy, err := symbol(s)
		if err != nil {
			return nil, err
		}
		attr.AddLeaf(sy)
		return attr, nil
	}
	return symbol(s)
}

// Symbol := Procedure | Data | Const.
func symbol(s *Lexer) (*mod.Node, *Error) {
	Track(s, "symbol")
	var n *mod.Node
	var err *Error
	switch s.Word.Lex {
	case lex.PROC:
		n, err = procDef(s)
	case lex.DATA:
		n, err = dataDef(s)
	case lex.CONST:
		n, err = constDef(s)
	case lex.TYPE:
		n, err = typeDef(s)
	default:
		return nil, nil
	}
	return n, err
}

// TypeDef := 'type' (SingleType|MultipleType).
func typeDef(s *Lexer) (*mod.Node, *Error) {
	kw, err := expect(s, lex.TYPE)
	if err != nil {
		return nil, err
	}

	err = check(s, lex.BEGIN, lex.IDENTIFIER)
	if err != nil {
		return nil, err
	}
	var leaf *mod.Node
	if s.Word.Lex == lex.BEGIN {
		leaf, err = multipleType(s)
	} else if s.Word.Lex == lex.IDENTIFIER {
		leaf, err = singleType(s)
	}
	if err != nil {
		return nil, err
	}
	kw.AddLeaf(leaf)
	return kw, nil
}

// SingleType := id ('as'|'is') Type.
func singleType(s *Lexer) (*mod.Node, *Error) {
	if s.Word.Lex != lex.IDENTIFIER {
		return nil, nil
	}
	id, err := consume(s)
	if err != nil {
		return nil, err
	}
	kw, err := expect(s, lex.AS, lex.IS)
	if err != nil {
		return nil, err
	}
	tp, err := expectProd(s, _type, "type")
	if err != nil {
		return nil, err
	}
	kw.SetLeaves([]*mod.Node{id, tp})
	return kw, nil
}

// SingleType ';'.
func singleTypeSemicolon(s *Lexer) (*mod.Node, *Error) {
	n, err := singleType(s)
	if err != nil {
		return nil, err
	}
	if n == nil {
		return nil, nil
	}
	_, err = expect(s, lex.SEMICOLON)
	if err != nil {
		return nil, err
	}
	return n, nil
}

// MultipleType := 'begin' {SingleType ';'} 'end'.
func multipleType(s *Lexer) (*mod.Node, *Error) {
	kw, err := expect(s, lex.BEGIN)
	if err != nil {
		return nil, err
	}
	typeDefs, err := repeat(s, singleTypeSemicolon)
	_, err = expect(s, lex.END)
	if err != nil {
		return nil, err
	}
	kw.SetLeaves(typeDefs)
	return kw, nil
}

// Const := 'const' (SingleConst|MultipleConst).
func constDef(s *Lexer) (*mod.Node, *Error) {
	kw, err := expect(s, lex.CONST)
	if err != nil {
		return nil, err
	}
	err = check(s, lex.BEGIN, lex.IDENTIFIER)
	if err != nil {
		return nil, err
	}
	var leaf *mod.Node
	switch s.Word.Lex {
	case lex.BEGIN:
		leaf, err = multipleConst(s)
	case lex.IDENTIFIER:
		leaf, err = singleConst(s)
	}
	if err != nil {
		return nil, err
	}
	kw.AddLeaf(leaf)
	return kw, nil
}

// SingleConst := id '=' Expr.
func singleConst(s *Lexer) (*mod.Node, *Error) {
	if s.Word.Lex != lex.IDENTIFIER {
		return nil, nil
	}
	id, err := consume(s)
	if err != nil {
		return nil, err
	}

	_, err = expect(s, lex.ASSIGNMENT)
	if err != nil {
		return nil, err
	}

	expr, err := expr(s)
	if err != nil {
		return nil, err
	}

	n := &mod.Node{
		Lex: lex.SINGLE,
	}
	n.SetLeaves([]*mod.Node{id, expr})
	return n, nil
}

// MultipleConst := 'begin' {SingleConst ';'} 'end'.
func multipleConst(s *Lexer) (*mod.Node, *Error) {
	kw, err := expect(s, lex.BEGIN)
	if err != nil {
		return nil, err
	}
	leafs, err := repeat(s, singleConstSemicolon)
	if err != nil {
		return nil, err
	}
	_, err = expect(s, lex.END)
	if err != nil {
		return nil, err
	}
	kw.SetLeaves(leafs)
	return kw, nil
}

// SingleConst ';'.
func singleConstSemicolon(s *Lexer) (*mod.Node, *Error) {
	n, err := singleConst(s)
	if err != nil {
		return nil, err
	}
	if n == nil {
		return nil, nil
	}
	_, err = expect(s, lex.SEMICOLON)
	if err != nil {
		return nil, err
	}
	return n, nil
}

// Data := 'data' (SingleData|MultipleData).
func dataDef(s *Lexer) (*mod.Node, *Error) {
	kw, err := expect(s, lex.DATA)
	if err != nil {
		return nil, err
	}
	err = check(s, lex.BEGIN, lex.IDENTIFIER)
	if err != nil {
		return nil, err
	}
	var leaf *mod.Node
	switch s.Word.Lex {
	case lex.BEGIN:
		leaf, err = multipleData(s)
	case lex.IDENTIFIER:
		leaf, err = singleData(s)
	}
	if err != nil {
		return nil, err
	}
	kw.AddLeaf(leaf)
	return kw, nil
}

// MultipleData := 'begin' {SingleData [';']} 'end'.
func multipleData(s *Lexer) (*mod.Node, *Error) {
	kw, err := expect(s, lex.BEGIN)
	if err != nil {
		return nil, err
	}
	leafs, err := repeat(s, singleDataSemicolon)
	if err != nil {
		return nil, err
	}
	_, err = expect(s, lex.END)
	if err != nil {
		return nil, err
	}
	kw.SetLeaves(leafs)
	return kw, nil
}

// SingleData ';'
func singleDataSemicolon(s *Lexer) (*mod.Node, *Error) {
	sg, err := singleData(s)
	if err != nil {
		return nil, err
	}
	if sg == nil {
		return nil, nil
	}
	_, err = expect(s, lex.SEMICOLON)
	if err != nil {
		return nil, err
	}
	return sg, nil
}

// SingleData :=  id [Annot] (DExpr|string|Blob).
func singleData(s *Lexer) (*mod.Node, *Error) {
	if s.Word.Lex != lex.IDENTIFIER {
		return nil, nil
	}
	id, err := consume(s)
	if err != nil {
		return nil, err
	}

	var ann *mod.Node
	if s.Word.Lex == lex.COLON {
		ann, err = annot(s)
		if err != nil {
			return nil, err
		}
	}

	err = check(s, lex.LEFTBRACKET, lex.STRING_LIT, lex.LEFTBRACE)
	if err != nil {
		return nil, err
	}
	var definition *mod.Node
	switch s.Word.Lex {
	case lex.STRING_LIT:
		definition, err = expect(s, lex.STRING_LIT)
		if err != nil {
			return nil, err
		}
	case lex.LEFTBRACKET:
		definition, err = dExpr(s)
		if err != nil {
			return nil, err
		}
	case lex.LEFTBRACE:
		definition, err = blob(s)
		if err != nil {
			return nil, err
		}
	}

	sg := &mod.Node{
		Lex: lex.SINGLE,
	}
	sg.SetLeaves([]*mod.Node{id, ann, definition})
	return sg, nil
}

// Blob := '{' ExprList '}'.
func blob(s *Lexer) (*mod.Node, *Error) {
	_, err := expect(s, lex.LEFTBRACE)
	if err != nil {
		return nil, err
	}
	n, err := exprList(s)
	if err != nil {
		return nil, err
	}
	_, err = expect(s, lex.RIGHTBRACE)
	if err != nil {
		return nil, err
	}
	sg := &mod.Node{
		Lex:    lex.BLOB,
		Leaves: n.Leaves,
	}
	return sg, nil
}

// DExpr := '[' [Expr] ']'.
func dExpr(s *Lexer) (*mod.Node, *Error) {
	_, err := expect(s, lex.LEFTBRACKET)
	if err != nil {
		return nil, err
	}
	expr, err := expr(s)
	if err != nil {
		return nil, err
	}
	_, err = expect(s, lex.RIGHTBRACKET)
	if err != nil {
		return nil, err
	}
	return expr, nil
}

// Procedure := 'proc' id [Annotatted|Direct] [Vars] Block.
// Annotatted := Annot [AArgs].
// Direct := DArgs [Rets].
func procDef(s *Lexer) (*mod.Node, *Error) {
	Track(s, "Procedure")
	kw, err := expect(s, lex.PROC)
	if err != nil {
		return nil, err
	}
	var id, ann, args, rets, vars *mod.Node
	id, err = expect(s, lex.IDENTIFIER)
	if err != nil {
		return nil, err
	}
	switch s.Word.Lex {
	case lex.COLON:
		ann, err = annot(s)
		if err != nil {
			return nil, err
		}
		args, err = aArgs(s)
		if err != nil {
			return nil, err
		}
	case lex.LEFTBRACKET:
		args, err = procArgs(s)
		if err != nil {
			return nil, err
		}
		rets, err = typeList(s)
		if err != nil {
			return nil, err
		}
	}
	if s.Word.Lex == lex.VAR {
		vars, err = procVars(s)
		if err != nil {
			return nil, err
		}
	}
	body, err := block(s)
	if err != nil {
		return nil, err
	}
	kw.SetLeaves([]*mod.Node{id, ann, args, rets, vars, body})
	return kw, nil
}

// AArgs := '[' [idList] ']'.
func aArgs(s *Lexer) (*mod.Node, *Error) {
	Track(s, "aArgs")
	if s.Word.Lex != lex.LEFTBRACKET {
		return nil, nil
	}
	_, err := consume(s)
	if err != nil {
		return nil, err
	}
	list, err := repeatCommaList(s, ident)
	if err != nil {
		return nil, err
	}
	_, err = expect(s, lex.RIGHTBRACKET)
	if err != nil {
		return nil, err
	}
	if list != nil {
		n := &mod.Node{
			Lex:    lex.IDLIST,
			Leaves: list,
		}
		return n, nil
	}
	return nil, nil
}

// Args := '[' [DeclList] ']'.
func procArgs(s *Lexer) (*mod.Node, *Error) {
	Track(s, "Args")
	_, err := expect(s, lex.LEFTBRACKET)
	if err != nil {
		return nil, err
	}

	if s.Word.Lex != lex.RIGHTBRACKET {
		n, err := declList(s)
		if err != nil {
			return nil, err
		}
		_, err = expect(s, lex.RIGHTBRACKET)
		if err != nil {
			return nil, err
		}
		return n, nil
	}

	_, err = expect(s, lex.RIGHTBRACKET)
	if err != nil {
		return nil, err
	}
	return nil, nil
}

// Rets := TypeList.
// TypeList := type {',' type} [','].
func typeList(s *Lexer) (*mod.Node, *Error) {
	Track(s, "TypeList")
	types, err := repeatCommaList(s, _type)
	if err != nil {
		return nil, err
	}
	if types == nil {
		return nil, nil
	}
	n := &mod.Node{
		Lex: lex.TYPELIST,
	}
	n.SetLeaves(types)
	return n, nil
}

func obligatoryTypeList(s *Lexer) (*mod.Node, *Error) {
	Track(s, "obligatoryTypeList")
	rets, err := repeatCommaList(s, _type)
	if err != nil {
		return nil, err
	}
	if rets == nil {
		return nil, failedExpected(s, "type list")
	}
	n := &mod.Node{Lex: lex.TYPELIST}
	n.SetLeaves(rets)
	return n, nil
}

// Type := (basicType | ProcType | Name) ['^' Layout].
func _type(s *Lexer) (*mod.Node, *Error) {
	Track(s, "type")
	var n *mod.Node
	var err *Error
	switch s.Word.Lex {
	case lex.I16, lex.I8, lex.I32, lex.I64,
		lex.U16, lex.U8, lex.U32, lex.U64, lex.BOOL, lex.PTR,
		lex.VOID:
		n, err = consume(s)
	case lex.PROC:
		n, err = procType(s)
	case lex.IDENTIFIER:
		n, err = name(s)
	default:
		return nil, nil
	}
	if err != nil {
		return nil, err
	}
	if s.Word.Lex == lex.CARET {
		caret, err := consume(s)
		if err != nil {
			return nil, err
		}
		lay, err := layout(s)
		caret.SetLeaves([]*mod.Node{n, lay})
		return caret, nil
	}
	return n, nil
}

// ProcType := 'proc' ProcTTList ProcTTList.
func procType(s *Lexer) (*mod.Node, *Error) {
	Track(s, "procType")
	keyword, err := expect(s, lex.PROC)
	if err != nil {
		return nil, err
	}
	args, err := procTTList(s)
	if err != nil {
		return nil, err
	}
	rets, err := procTTList(s)
	if err != nil {
		return nil, err
	}
	keyword.SetLeaves([]*mod.Node{args, rets})
	return keyword, err
}

// ProcTTList := '[' [TypeList] ']'.
func procTTList(s *Lexer) (*mod.Node, *Error) {
	Track(s, "procTypeTypeList")
	var tps *mod.Node
	_, err := expect(s, lex.LEFTBRACKET)
	if err != nil {
		return nil, err
	}
	if s.Word.Lex != lex.RIGHTBRACKET {
		tps, err = obligatoryTypeList(s)
		if err != nil {
			return nil, err
		}
	}
	_, err = expect(s, lex.RIGHTBRACKET)
	if err != nil {
		return nil, err
	}
	return tps, nil
}

// Layout := Type | Struct.
func layout(s *Lexer) (*mod.Node, *Error) {
	if s.Word.Lex == lex.BEGIN {
		return _struct(s)
	}
	n, err := expectProd(s, _type, "type")
	if err != nil {
		return nil, err
	}
	return n, nil
}

// Struct := 'begin' [Size] {Field ';'} 'end'.
func _struct(s *Lexer) (*mod.Node, *Error) {
	n, err := expect(s, lex.BEGIN)
	if err != nil {
		return nil, err
	}
	var _size *mod.Node
	if s.Word.Lex == lex.LEFTBRACKET {
		_size, err = size(s)
		if err != nil {
			return nil, err
		}
	}
	fields, err := repeat(s, fieldSemicolon)
	if err != nil {
		return nil, err
	}
	fieldList := &mod.Node{
		Lex:    lex.FIELDLIST,
		Leaves: fields,
	}
	_, err = expect(s, lex.END)
	if err != nil {
		return nil, err
	}
	n.SetLeaves([]*mod.Node{_size, fieldList})
	return n, nil
}

// Size := '[' Expr ']'.
func size(s *Lexer) (*mod.Node, *Error) {
	_, err := expect(s, lex.LEFTBRACKET)
	if err != nil {
		return nil, err
	}
	exp, err := expectProd(s, expr, "expression")
	if err != nil {
		return nil, err
	}
	_, err = expect(s, lex.RIGHTBRACKET)
	if err != nil {
		return nil, err
	}
	return exp, nil
}

// Field ';'.
func fieldSemicolon(s *Lexer) (*mod.Node, *Error) {
	n, err := field(s)
	if err != nil {
		return nil, err
	}
	if n == nil {
		return nil, nil
	}
	_, err = expect(s, lex.SEMICOLON)
	if err != nil {
		return nil, err
	}
	return n, nil
}

// Field := IdList Annot [Offset].
func field(s *Lexer) (*mod.Node, *Error) {
	ids, err := repeatCommaList(s, ident)
	if err != nil {
		return nil, err
	}
	if ids == nil {
		return nil, nil
	}
	ann, err := annot(s)
	if err != nil {
		return nil, err
	}
	var _offset *mod.Node
	if s.Word.Lex == lex.LEFTBRACE {
		_offset, err = offset(s)
		if err != nil {
			return nil, err
		}
	}
	idList := &mod.Node{
		Lex:    lex.IDLIST,
		Leaves: ids,
	}
	field := &mod.Node{
		Lex:    lex.FIELD,
		Leaves: []*mod.Node{idList, ann, _offset},
	}
	return field, nil
}

// Offset := '{' Expr '}'.
func offset(s *Lexer) (*mod.Node, *Error) {
	_, err := expect(s, lex.LEFTBRACE)
	if err != nil {
		return nil, err
	}
	exp, err := expectProd(s, expr, "expression")
	if err != nil {
		return nil, err
	}
	_, err = expect(s, lex.RIGHTBRACE)
	if err != nil {
		return nil, err
	}
	return exp, nil
}

// Vars := 'var' DeclList.
func procVars(s *Lexer) (*mod.Node, *Error) {
	Track(s, "Var")
	_, err := expect(s, lex.VAR)
	if err != nil {
		return nil, err
	}
	return expectProd(s, declList, "declaration")
}

// DeclList := Decl {',' Decl} [','].
func declList(s *Lexer) (*mod.Node, *Error) {
	Track(s, "DeclList")
	nodes, err := repeatCommaList(s, decl)
	if err != nil {
		return nil, err
	}
	if len(nodes) == 0 {
		return nil, nil
	}
	n := &mod.Node{Lex: lex.PROCDECLS}
	n.SetLeaves(nodes)
	return n, nil
}

// Decl := idList Annot.
func decl(s *Lexer) (*mod.Node, *Error) {
	Track(s, "Decl")
	if s.Word.Lex != lex.IDENTIFIER {
		return nil, nil
	}
	ids, err := repeatCommaList(s, ident)
	if err != nil {
		return nil, err
	}
	idList := &mod.Node{
		Lex:    lex.IDLIST,
		Leaves: ids,
	}
	colon, err := annot(s)
	if err != nil {
		return nil, err
	}
	colon.SetLeaves([]*mod.Node{idList, colon.Leaves[0]})
	return colon, nil
}

func ident(s *Lexer) (*mod.Node, *Error) {
	if s.Word.Lex == lex.IDENTIFIER {
		return consume(s)
	}
	return nil, nil
}

/*
Statement := If [';']
      | While [';']
      | Return ';'
      | Set ';'
      | Exit ';'
      | Expr ';'.
*/
func statement(s *Lexer) (*mod.Node, *Error) {
	Track(s, "code")
	var n *mod.Node
	var err *Error
	semicolon := true
	switch s.Word.Lex {
	case lex.EOF:
		return nil, nil
	case lex.IF:
		n, err = _if(s)
		semicolon = false
	case lex.WHILE:
		n, err = _while(s)
		semicolon = false
	case lex.RETURN:
		n, err = _return(s)
	case lex.SET:
		n, err = _set(s)
	case lex.EXIT:
		n, err = _exit(s)
	default:
		n, err = expr(s)
		if n == nil && err == nil {
			return nil, nil
		}
	}
	if err != nil {
		return nil, err
	}
	if semicolon {
		_, err := expect(s, lex.SEMICOLON)
		if err != nil {
			return nil, err
		}
	}
	return n, nil
}

// Set := 'set' ExprList '=' Expr.
func _set(s *Lexer) (*mod.Node, *Error) {
	Track(s, "Set")
	kw, err := expect(s, lex.SET)
	if err != nil {
		return nil, err
	}
	ass, err := exprList(s)
	if err != nil {
		return nil, err
	}
	op, err := expect(s, lex.ASSIGNMENT, lex.PLUS_ASSIGN, lex.MINUS_ASSIGN,
		lex.MULTIPLICATION_ASSIGN, lex.DIVISION_ASSIGN, lex.REMAINDER_ASSIGN)
	if err != nil {
		return nil, err
	}
	exp, err := expectProd(s, expr, "expression")
	if err != nil {
		return nil, err
	}
	kw.SetLeaves([]*mod.Node{ass, op, exp})
	return kw, nil
}

// ExprList := Expr {',' Expr} [','].
func exprList(s *Lexer) (*mod.Node, *Error) {
	asses, err := repeatCommaList(s, expr)
	if err != nil {
		return nil, err
	}
	n := &mod.Node{Lex: lex.EXPRLIST}
	n.SetLeaves(asses)
	return n, nil
}

// Return := 'return' [ExprList].
func _return(s *Lexer) (*mod.Node, *Error) {
	Track(s, "return")
	kw, err := consume(s)
	if err != nil {
		return nil, err
	}
	exp, err := repeatCommaList(s, expr) // optional
	if err != nil {
		return nil, err
	}
	if exp != nil {
		kw.SetLeaves(exp)
	}
	return kw, err
}

// Exit := 'exit' ['?'] Expr.
func _exit(s *Lexer) (*mod.Node, *Error) {
	Track(s, "exit")
	kw, err := consume(s)
	if err != nil {
		return nil, err
	}
	var mark *mod.Node
	if s.Word.Lex == lex.QUESTION {
		mark, err = consume(s)
		if err != nil {
			return nil, err
		}
	}
	exp, err := expectProd(s, expr, "expression")
	if err != nil {
		return nil, err
	}
	kw.SetLeaves([]*mod.Node{mark, exp})
	return kw, err
}

// Expr = And {"or" And}.
func expr(s *Lexer) (*mod.Node, *Error) {
	Track(s, "Expr")
	return repeatBinary(s, boolAnd, "expression", orOp)
}

// And = Comp {"and" Comp}.
func boolAnd(s *Lexer) (*mod.Node, *Error) {
	Track(s, "And")
	return repeatBinary(s, comparative, "expression", andOp)
}

// Comp = Sum {compOp Sum}.
func comparative(s *Lexer) (*mod.Node, *Error) {
	Track(s, "Comp")
	return repeatBinary(s, additive, "expression", compOp)
}

// Sum = Mult {sumOp Mult}.
func additive(s *Lexer) (*mod.Node, *Error) {
	Track(s, "Sum")
	return repeatBinary(s, multiplicative, "expression", sumOp)
}

// Mult = Unary {multOp Unary}.
func multiplicative(s *Lexer) (*mod.Node, *Error) {
	Track(s, "Mult")
	return repeatBinary(s, unaryPrefix, "expression", multOp)
}

// Prefix = "not" | "~" | "!".
// UnaryPrefix := {Prefix} UnarySuffix.
func unaryPrefix(s *Lexer) (*mod.Node, *Error) {
	Track(s, "unary prefix")

	preFirst, preLast, err := repeatUnaryLeft(s, prefixOp)
	if err != nil {
		return nil, err
	}

	suff, err := unarySuffix(s)
	if err != nil {
		return nil, err
	}
	if preFirst != nil && suff == nil {
		msg := "expected expression after prefix operator"
		err := newCompilerError(s, et.ExpectedProd, msg)
		return nil, err
	}
	if suff == nil {
		return nil, nil
	}

	if preFirst != nil {
		preLast.AddLeaf(suff)
		suff = preFirst
	}

	return suff, nil
}

// UnarySuffix := Factor {Suffix}.
func unarySuffix(s *Lexer) (*mod.Node, *Error) {
	Track(s, "unary suffix")
	fact, err := factor(s)
	if err != nil {
		return nil, err
	}
	suFirst, suLast, err := repeatUnaryRight(s, suffix)
	if err != nil {
		return nil, err
	}
	if suFirst != nil {
		suFirst.AddLeaf(fact)
		fact = suLast
	}
	return fact, nil
}

/*
Suffix := Conversion
    | Deref
    | Call
    | DotAccess
    | ArrowAccess.
*/
func suffix(s *Lexer) (*mod.Node, *Error) {
	Track(s, "suffix")
	switch s.Word.Lex {
	case lex.LEFTBRACKET:
		return call(s)
	case lex.AT:
		return deref(s)
	case lex.COLON:
		return annot(s)
	case lex.DOT:
		return dotAccess(s)
	case lex.ARROW:
		return arrowAccess(s)
	}
	return nil, nil
}

// DotAccess := '.' (id|Call).
func dotAccess(s *Lexer) (*mod.Node, *Error) {
	if s.Word.Lex != lex.DOT { // we use it in sizeof production too
		return nil, nil
	}
	dot, err := consume(s)
	if err != nil {
		return nil, err
	}
	err = check(s, lex.LEFTBRACKET, lex.IDENTIFIER)
	if err != nil {
		return nil, err
	}
	var field *mod.Node
	switch s.Word.Lex {
	case lex.IDENTIFIER:
		field, err = consume(s)
	case lex.LEFTBRACKET:
		field, err = call(s)
	}
	if err != nil {
		return nil, err
	}
	dot.AddLeaf(field)
	return dot, err
}

// ArrowAccess := '->' (id|Call).
func arrowAccess(s *Lexer) (*mod.Node, *Error) {
	dot, err := expect(s, lex.ARROW)
	if err != nil {
		return nil, err
	}
	err = check(s, lex.LEFTBRACKET, lex.IDENTIFIER)
	if err != nil {
		return nil, err
	}
	var field *mod.Node
	switch s.Word.Lex {
	case lex.IDENTIFIER:
		field, err = consume(s)
	case lex.LEFTBRACKET:
		field, err = call(s)
	}
	if err != nil {
		return nil, err
	}
	dot.AddLeaf(field)
	return dot, err
}

// Deref := '@' Type.
func deref(s *Lexer) (*mod.Node, *Error) {
	Track(s, "deref")
	at, err := expect(s, lex.AT)
	if err != nil {
		return nil, err
	}
	t, err := expectProd(s, _type, "type")
	if err != nil {
		return nil, err
	}
	at.AddLeaf(t)
	return at, nil
}

// Call = "[" [ExprList] "]".
func call(s *Lexer) (*mod.Node, *Error) {
	Track(s, "call")
	lB, err := expect(s, lex.LEFTBRACKET)
	if err != nil {
		return nil, err
	}

	explist, err := exprList(s)
	if err != nil {
		return nil, err
	}
	if explist == nil {
		explist = &mod.Node{
			Lex:    lex.EXPRLIST,
			Leaves: []*mod.Node{},
		}
	}

	rB, err := expect(s, lex.RIGHTBRACKET)
	if err != nil {
		return nil, err
	}
	n := &mod.Node{
		Lex: lex.CALL,
		Range: &Range{
			Begin: lB.Range.Begin,
			End:   rB.Range.End,
		},
	}
	n.AddLeaf(explist)
	return n, nil
}

// Annot = ":" Type.
// Conversion = Annot.
func annot(s *Lexer) (*mod.Node, *Error) {
	Track(s, "annot")
	colon, err := expect(s, lex.COLON)
	if err != nil {
		return nil, err
	}
	tp, err := expectProd(s, _type, "type")
	if err != nil {
		return nil, err
	}
	colon.AddLeaf(tp)
	return colon, nil
}

/*
Factor := Name
	| Terminal
	| NestedExpr
	| "sizeof" Type.
*/
func factor(s *Lexer) (*mod.Node, *Error) {
	Track(s, "factor")
	switch s.Word.Lex {
	case lex.IDENTIFIER:
		return name(s)
	case lex.LEFTPAREN:
		lP, err := consume(s)
		if err != nil {
			return nil, err
		}
		n, err := expr(s)
		if err != nil {
			return nil, err
		}
		rP, err := expect(s, lex.RIGHTPAREN)
		if err != nil {
			return nil, err
		}
		n.Range = &Range{
			Begin: lP.Range.Begin,
			End:   rP.Range.End,
		}
		return n, nil
	case lex.SIZEOF:
		return sizeof(s)
	case lex.I64_LIT, lex.I32_LIT, lex.I16_LIT, lex.I8_LIT,
		lex.U64_LIT, lex.U32_LIT, lex.U16_LIT, lex.U8_LIT,
		lex.CHAR_LIT, lex.TRUE, lex.FALSE, lex.PTR_LIT:
		return consume(s)
	}
	return nil, nil
}

// Sizeof := 'sizeof' ['^'] '[' Type {DotAccess} ']'.
func sizeof(s *Lexer) (*mod.Node, *Error) {
	kw, err := expect(s, lex.SIZEOF)
	if err != nil {
		return nil, err
	}
	var caret *mod.Node
	if s.Word.Lex == lex.CARET {
		caret, err = consume(s)
		if err != nil {
			return nil, err
		}
	}
	_, err = expect(s, lex.LEFTBRACKET)
	if err != nil {
		return nil, err
	}
	t, err := expectProd(s, _type, "type")
	if err != nil {
		return nil, err
	}
	dots, err := repeat(s, dotAccess)
	if err != nil {
		return nil, err
	}
	_, err = expect(s, lex.RIGHTBRACKET)
	if err != nil {
		return nil, err
	}
	var accesses *mod.Node
	if dots != nil {
		accesses = &mod.Node{Leaves: dots}
	}
	kw.SetLeaves([]*mod.Node{caret, t, accesses})
	return kw, nil
}

func name(s *Lexer) (*mod.Node, *Error) {
	id, err := consume(s)
	if err != nil {
		return nil, err
	}
	if s.Word.Lex == lex.DOUBLECOLON {
		dcolon, err := consume(s)
		if err != nil {
			return nil, err
		}
		id2, err := consume(s)
		if err != nil {
			return nil, err
		}
		dcolon.SetLeaves([]*mod.Node{id, id2})
		return dcolon, nil
	}
	return id, nil
}

// Block := 'begin' {Statement} 'end'.
func block(s *Lexer) (*mod.Node, *Error) {
	Track(s, "block")
	begin, err := expect(s, lex.BEGIN)
	if err != nil {
		return nil, err
	}
	leaves, err := repeat(s, statement)
	if err != nil {
		return nil, err
	}
	end, err := expect(s, lex.END)
	if err != nil {
		return nil, err
	}
	bl := &mod.Node{
		Lex: lex.BLOCK,
		Range: &Range{
			Begin: begin.Range.Begin,
			End:   end.Range.End,
		},
	}
	bl.SetLeaves(leaves)
	return bl, nil
}

// If := 'if' Expr Block [Else] 'if'.
func _if(s *Lexer) (*mod.Node, *Error) {
	Track(s, "if")
	keyword, err := expect(s, lex.IF)
	var exp, bl, else_, elseif_ *mod.Node

	exp, err = expectProd(s, expr, "expression")
	if err != nil {
		return nil, err
	}

	bl, err = expectProd(s, block, "block")
	if err != nil {
		return nil, err
	}

	if s.Word.Lex == lex.ELSEIF {
		elseif_, err = elseifchain(s)
		if err != nil {
			return nil, err
		}
	}

	if s.Word.Lex == lex.ELSE {
		else_, err = _else(s)
		if err != nil {
			return nil, err
		}
	}
	keyword.SetLeaves([]*mod.Node{exp, bl, elseif_, else_})
	return keyword, nil
}

// Else := 'else' Block.
func _else(s *Lexer) (*mod.Node, *Error) {
	Track(s, "else")
	kw, err := consume(s)
	if err != nil {
		return nil, err
	}
	bl, err := block(s)
	if err != nil {
		return nil, err
	}
	kw.AddLeaf(bl)
	return kw, nil
}

func elseifchain(s *Lexer) (*mod.Node, *Error) {
	elses, err := repeat(s, _elseif)
	if err != nil {
		return nil, err
	}
	n := &mod.Node{
		Lex: lex.ELSEIFCHAIN,
	}
	n.SetLeaves(elses)
	return n, nil
}

func _elseif(s *Lexer) (*mod.Node, *Error) {
	if s.Word.Lex != lex.ELSEIF {
		return nil, nil
	}
	kw, err := consume(s)
	if err != nil {
		return nil, err
	}
	exp, err := expectProd(s, expr, "expression")
	if err != nil {
		return nil, err
	}
	bl, err := block(s)
	if err != nil {
		return nil, err
	}
	kw.SetLeaves([]*mod.Node{exp, bl})
	return kw, nil
}

// While = 'while' Expr Block 'while'.
func _while(s *Lexer) (*mod.Node, *Error) {
	Track(s, "while")
	keyword, err := expect(s, lex.WHILE)
	if err != nil {
		return nil, err
	}
	exp, err := expectProd(s, expr, "expression")
	if err != nil {
		return nil, err
	}
	bl, err := block(s)
	if err != nil {
		return nil, err
	}
	keyword.SetLeaves([]*mod.Node{exp, bl})
	return keyword, nil
}

func number(s *Lexer) (*mod.Node, *Error) {
	return expect(s, lex.I64_LIT, lex.I32_LIT, lex.I16_LIT, lex.I8_LIT,
		lex.U64_LIT, lex.U32_LIT, lex.U16_LIT, lex.U8_LIT, lex.PTR_LIT)
}

func numberOrString(s *Lexer) (*mod.Node, *Error) {
	return expect(s, lex.I64_LIT, lex.I32_LIT, lex.I16_LIT, lex.I8_LIT,
		lex.U64_LIT, lex.U32_LIT, lex.U16_LIT, lex.U8_LIT, lex.PTR_LIT, lex.STRING_LIT)
}

func sumOp(n *mod.Node) bool {
	switch n.Lex {
	case lex.PLUS, lex.MINUS, lex.BITWISEOR, lex.BITWISEXOR:
		return true
	}
	return false
}

func multOp(n *mod.Node) bool {
	switch n.Lex {
	case lex.MULTIPLICATION, lex.DIVISION, lex.REMAINDER,
		lex.BITWISEAND, lex.SHIFTLEFT, lex.SHIFTRIGHT:
		return true
	}
	return false
}

func compOp(n *mod.Node) bool {
	switch n.Lex {
	case lex.LESS, lex.LESSEQ, lex.EQUALS,
		lex.MOREEQ, lex.MORE, lex.DIFFERENT:
		return true
	}
	return false
}

func orOp(n *mod.Node) bool {
	return n.Lex == lex.OR
}

func andOp(n *mod.Node) bool {
	return n.Lex == lex.AND
}

// Prefix := 'not' | '~' | '!' | '@'.
func prefixOp(st *Lexer) (*mod.Node, *Error) {
	switch st.Word.Lex {
	case lex.NOT, lex.NEG, lex.BITWISENOT, lex.AT:
		return consume(st)
	}
	return nil, nil
}

func isComma(n *mod.Node) bool {
	return n.Lex == lex.COMMA
}

func consume(st *Lexer) (*mod.Node, *Error) {
	n := st.Word
	err := st.Next()
	return n, err
}

func check(st *Lexer, tpList ...lex.LexKind) *Error {
	for _, tp := range tpList {
		if st.Word.Lex == tp {
			return nil
		}
	}
	message := fmt.Sprintf("Expected one of %v: instead found '%v'",
		lex.FmtToUser(tpList...),
		lex.FmtToUser(st.Word.Lex))

	err := newCompilerError(st, et.ExpectedSymbol, message)
	return err
}

func expect(st *Lexer, tpList ...lex.LexKind) (*mod.Node, *Error) {
	for _, tp := range tpList {
		if st.Word.Lex == tp {
			return consume(st)
		}
	}
	message := fmt.Sprintf("Expected one of %v: instead found %v",
		lex.FmtToUser(tpList...),
		lex.FmtToUser(st.Word.Lex))

	err := newCompilerError(st, et.ExpectedSymbol, message)
	return nil, err
}

func expectProd(st *Lexer, prod production, name string) (*mod.Node, *Error) {
	n, err := prod(st)
	if err != nil {
		return nil, err
	}
	if n == nil {
		err := failedExpected(st, name)
		return nil, err
	}
	return n, err
}

func failedExpected(st *Lexer, name string) *Error {
	message := fmt.Sprintf("expected %v instead found %v", name, lex.FmtToUser(st.Word.Lex))
	return newCompilerError(st, et.ExpectedProd, message)
}

type production func(st *Lexer) (*mod.Node, *Error)
type validator func(*mod.Node) bool

/* repeatBinary implements the following pattern
for a given Production and Terminal:

	repeatBinary := Production {Terminal Production}

Validator checks for terminals.
Left to Right precedence
*/
func repeatBinary(st *Lexer, prod production, name string, v validator) (*mod.Node, *Error) {
	last, err := prod(st)
	if err != nil {
		return nil, err
	}
	if last == nil {
		return nil, nil
	}
	for v(st.Word) {
		parent, err := consume(st)
		if err != nil {
			return nil, err
		}
		parent.AddLeaf(last)

		newLeaf, err := expectProd(st, prod, name)
		if err != nil {
			return nil, err
		}
		parent.AddLeaf(newLeaf)

		last = parent
	}
	return last, nil
}

/* repeat implements the following pattern
for a given Production:

	repeat := {Production}.
*/
func repeat(st *Lexer, prod production) ([]*mod.Node, *Error) {
	out := []*mod.Node{}
	n, err := prod(st)
	if err != nil {
		return nil, err
	}
	if n == nil {
		return nil, nil
	}
	for n != nil {
		out = append(out, n)
		n, err = prod(st)
		if err != nil {
			return nil, err
		}
	}
	return out, nil
}

/*repeatUnaryLeft implements the following pattern
for a given Production:

	repeatUnaryLeft := {Production}.

But returns the first and last item in the tree.

It's Left associative: first<-second<-last
*/
func repeatUnaryLeft(st *Lexer, prod production) (*mod.Node, *mod.Node, *Error) {
	first, err := prod(st)
	if err != nil {
		return nil, nil, err
	}
	if first == nil {
		return nil, nil, nil
	}
	last := first
	for first != nil {
		n, err := prod(st)
		if err != nil {
			return nil, nil, err
		}
		if n == nil {
			break
		}
		last.AddLeaf(n)
		last = n
	}
	return first, last, nil
}

func repeatUnaryRight(st *Lexer, prod production) (*mod.Node, *mod.Node, *Error) {
	first, err := prod(st)
	if err != nil {
		return nil, nil, err
	}
	if first == nil {
		return nil, nil, nil
	}
	last := first
	for first != nil {
		n, err := prod(st)
		if err != nil {
			return nil, nil, err
		}
		if n == nil {
			break
		}
		n.AddLeaf(last)
		last = n
	}
	return first, last, nil
}

/* repeatList implements the following pattern
for a given Production and Terminal:

	RepeatBinary := Production {Terminal Production}

Validator checks for terminals.

It differs from RepeatBinary in that it returns a slice
instead of a Tree with precedence
*/
func repeatList(st *Lexer, prod production, val validator) ([]*mod.Node, *Error) {
	first, err := prod(st)
	if err != nil {
		return nil, err
	}
	if first == nil {
		return nil, nil
	}
	out := []*mod.Node{first}
	for val(st.Word) {
		st.Next()
		n, err := prod(st)
		if err != nil {
			return nil, err
		}
		out = append(out, n)
	}
	return out, nil
}

// Implements the pattern:
//    RepeatBinary := Production {',' Production} [','].
func repeatCommaList(st *Lexer, prod production) ([]*mod.Node, *Error) {
	first, err := prod(st)
	if err != nil {
		return nil, err
	}
	if first == nil {
		return nil, nil
	}
	out := []*mod.Node{first}
	for st.Word.Lex == lex.COMMA {
		st.Next()
		n, err := prod(st)
		if err != nil {
			return nil, err
		}
		if n == nil {
			break
		}
		out = append(out, n)
	}
	return out, nil
}

func createNode(nodes []*mod.Node, t lex.LexKind) *mod.Node {
	n := &mod.Node{Lex: t}
	n.SetLeaves(nodes)
	return n
}

func expectedEOF(s *Lexer) *Error {
	return newCompilerError(s, et.ExpectedEOF, "unexpected symbol, expected EOF")
}

func newCompilerError(st *Lexer, t et.ErrorKind, message string) *Error {
	return &Error{
		Code:     t,
		Severity: sv.Error,
		Location: &Location{Range: st.Word.Range, File: st.File},
		Message:  message,
	}
}

func computeRanges(curr *mod.Node) {
	if curr == nil {
		return
	}
	for _, leaf := range curr.Leaves {
		computeRanges(leaf)
	}
	for _, n := range curr.Leaves {
		if n == nil || n.Range == nil {
			continue
		}
		if curr.Range == nil {
			r := *n.Range
			curr.Range = &r
			continue
		}
		if curr.Range.Begin.MoreThan(n.Range.Begin) {
			curr.Range.Begin = n.Range.Begin
		}
		if curr.Range.End.LessThan(n.Range.End) {
			curr.Range.End = n.Range.End
		}
	}
}
