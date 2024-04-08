package parser

import (
	"fmt"
	. "mpc/core"
	ek "mpc/core/errorkind"
	mod "mpc/core/module"
	lk "mpc/core/module/lexkind"
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
	n.AddLeaf(createNode(coupl, lk.COUPLINGS))
	n.AddLeaf(createNode(symb, lk.SYMBOLS))
	if s.Word.Lex != lk.EOF {
		return nil, expectedEOF(s)
	}
	return n, nil
}

// Coupling := Import | FromImport | Export.
func coupling(s *Lexer) (*mod.Node, *Error) {
	switch s.Word.Lex {
	case lk.IMPORT:
		return _import(s)
	case lk.FROM:
		return _fromImport(s)
	case lk.EXPORT:
		return _export(s)
	}
	return nil, nil
}

// Import := 'import' Items.
func _import(s *Lexer) (*mod.Node, *Error) {
	kw, err := expect(s, lk.IMPORT)
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
	kw, err := expect(s, lk.FROM)
	if err != nil {
		return nil, err
	}
	id, err := expect(s, lk.IDENTIFIER)
	if err != nil {
		return nil, err
	}

	_, err = expect(s, lk.IMPORT)
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
	kw, err := expect(s, lk.EXPORT)
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
	if s.Word.Lex == lk.ALL {
		return consume(s)
	} else {
		list, err := repeatCommaList(s, alias)
		if err != nil {
			return nil, err
		}
		listNode := &mod.Node{Lex: lk.ALIASLIST}
		listNode.SetLeaves(list)
		return listNode, nil
	}
}

// AliasList := Alias {',' Alias} [','].
func alias(s *Lexer) (*mod.Node, *Error) {
	if s.Word.Lex == lk.IDENTIFIER {
		id, err := consume(s)
		if err != nil {
			return nil, err
		}
		if s.Word.Lex == lk.AS {
			kw, err := consume(s)
			if err != nil {
				return nil, err
			}
			id2, err := expect(s, lk.IDENTIFIER)
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

// IdList = id {',' id} [','].
func idList(s *Lexer) (*mod.Node, *Error) {
	list, err := repeatCommaList(s, ident)
	if err != nil {
		return nil, err
	}
	if list == nil {
		return nil, nil
	}
	listNode := &mod.Node{Lex: lk.IDLIST}
	listNode.SetLeaves(list)
	return listNode, nil
}

// AttSymbol = [Attributes] Symbol [';'].
// Attributes = 'attr' IdList.
func attrSymbol(s *Lexer) (*mod.Node, *Error) {
	if s.Word.Lex == lk.ATTR {
		attr, err := consume(s)
		if err != nil {
			return nil, err
		}
		listNode, err := idList(s)
		if err != nil {
			return nil, err
		}
		attr.AddLeaf(listNode)

		sy, err := symbol(s)
		if err != nil {
			return nil, err
		}
		if s.Word.Lex == lk.SEMICOLON {
			_, err := consume(s)
			if err != nil {
				return nil, err
			}
		}
		attr.AddLeaf(sy)
		return attr, nil
	}
	sy, err := symbol(s)
	if err != nil {
		return nil, err
	}
	if s.Word.Lex == lk.SEMICOLON {
		_, err := consume(s)
		if err != nil {
			return nil, err
		}
	}
	return sy, nil
}

// Symbol = Procedure | Data | Const | Struct.
func symbol(s *Lexer) (*mod.Node, *Error) {
	Track(s, "symbol")
	switch s.Word.Lex {
	case lk.PROC:
		return procDef(s)
	case lk.DATA:
		return dataDef(s)
	case lk.CONST:
		return constDef(s)
	case lk.STRUCT:
		return structDef(s)
	default:
		return nil, nil
	}
}

// Const := 'const' (SingleConst|MultipleConst).
func constDef(s *Lexer) (*mod.Node, *Error) {
	kw, err := expect(s, lk.CONST)
	if err != nil {
		return nil, err
	}
	err = check(s, lk.BEGIN, lk.IDENTIFIER)
	if err != nil {
		return nil, err
	}
	var leaf *mod.Node
	switch s.Word.Lex {
	case lk.BEGIN:
		leaf, err = multipleConst(s)
	case lk.IDENTIFIER:
		leaf, err = singleConst(s)
	}
	if err != nil {
		return nil, err
	}
	kw.AddLeaf(leaf)
	return kw, nil
}

// SingleConst := id [Annot] '=' Expr.
func singleConst(s *Lexer) (*mod.Node, *Error) {
	if s.Word.Lex != lk.IDENTIFIER {
		return nil, nil
	}
	id, err := consume(s)
	if err != nil {
		return nil, err
	}

	var ann *mod.Node
	if s.Word.Lex == lk.COLON {
		ann, err = annot(s)
		if err != nil {
			return nil, err
		}
	}

	_, err = expect(s, lk.ASSIGNMENT)
	if err != nil {
		return nil, err
	}

	expr, err := expr(s)
	if err != nil {
		return nil, err
	}

	n := &mod.Node{
		Lex: lk.SINGLE,
	}
	n.SetLeaves([]*mod.Node{id, ann, expr})
	return n, nil
}

// MultipleConst := 'begin' {SingleConst ';'} 'end'.
func multipleConst(s *Lexer) (*mod.Node, *Error) {
	kw, err := expect(s, lk.BEGIN)
	if err != nil {
		return nil, err
	}
	leafs, err := repeat(s, singleConstSemicolon)
	if err != nil {
		return nil, err
	}
	_, err = expect(s, lk.END)
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
	_, err = expect(s, lk.SEMICOLON)
	if err != nil {
		return nil, err
	}
	return n, nil
}

// Data := 'data' (SingleData|MultipleData).
func dataDef(s *Lexer) (*mod.Node, *Error) {
	kw, err := expect(s, lk.DATA)
	if err != nil {
		return nil, err
	}
	err = check(s, lk.BEGIN, lk.IDENTIFIER)
	if err != nil {
		return nil, err
	}
	var leaf *mod.Node
	switch s.Word.Lex {
	case lk.BEGIN:
		leaf, err = multipleData(s)
	case lk.IDENTIFIER:
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
	kw, err := expect(s, lk.BEGIN)
	if err != nil {
		return nil, err
	}
	leafs, err := repeat(s, singleDataSemicolon)
	if err != nil {
		return nil, err
	}
	_, err = expect(s, lk.END)
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
	_, err = expect(s, lk.SEMICOLON)
	if err != nil {
		return nil, err
	}
	return sg, nil
}

// SingleData :=  id [Annot] (DExpr|string|Blob).
func singleData(s *Lexer) (*mod.Node, *Error) {
	if s.Word.Lex != lk.IDENTIFIER {
		return nil, nil
	}
	id, err := consume(s)
	if err != nil {
		return nil, err
	}

	var ann *mod.Node
	if s.Word.Lex == lk.COLON {
		ann, err = annot(s)
		if err != nil {
			return nil, err
		}
	}

	err = check(s, lk.LEFTBRACKET, lk.STRING_LIT, lk.LEFTBRACE)
	if err != nil {
		return nil, err
	}
	var definition *mod.Node
	switch s.Word.Lex {
	case lk.STRING_LIT:
		definition, err = expect(s, lk.STRING_LIT)
		if err != nil {
			return nil, err
		}
	case lk.LEFTBRACKET:
		definition, err = dExpr(s)
		if err != nil {
			return nil, err
		}
	case lk.LEFTBRACE:
		definition, err = blob(s)
		if err != nil {
			return nil, err
		}
	}

	sg := &mod.Node{
		Lex: lk.SINGLE,
	}
	sg.SetLeaves([]*mod.Node{id, ann, definition})
	return sg, nil
}

// Blob := '{' ExprList '}'.
func blob(s *Lexer) (*mod.Node, *Error) {
	_, err := expect(s, lk.LEFTBRACE)
	if err != nil {
		return nil, err
	}
	n, err := exprList(s)
	if err != nil {
		return nil, err
	}
	_, err = expect(s, lk.RIGHTBRACE)
	if err != nil {
		return nil, err
	}
	sg := &mod.Node{
		Lex:    lk.BLOB,
		Leaves: n.Leaves,
	}
	return sg, nil
}

// DExpr := '[' [Expr] ']'.
func dExpr(s *Lexer) (*mod.Node, *Error) {
	_, err := expect(s, lk.LEFTBRACKET)
	if err != nil {
		return nil, err
	}
	expr, err := expr(s)
	if err != nil {
		return nil, err
	}
	_, err = expect(s, lk.RIGHTBRACKET)
	if err != nil {
		return nil, err
	}
	return expr, nil
}

// Procedure = 'proc' id [CC] [Signature] [Vars] (Asm|Block).
// CC := '<' id '>'.
// Signature := DArgs [Rets].
func procDef(s *Lexer) (*mod.Node, *Error) {
	Track(s, "Procedure")
	kw, err := expect(s, lk.PROC)
	if err != nil {
		return nil, err
	}
	var id, CC, args, rets, vars *mod.Node
	id, err = expect(s, lk.IDENTIFIER)
	if err != nil {
		return nil, err
	}
	if s.Word.Lex == lk.LESS {
		CC, err = cc(s)
		if err != nil {
			return nil, err
		}
	}
	if s.Word.Lex == lk.LEFTBRACKET {
		args, err = procArgs(s)
		if err != nil {
			return nil, err
		}
		rets, err = typeList(s)
		if err != nil {
			return nil, err
		}
	}
	if s.Word.Lex == lk.VAR {
		vars, err = procVars(s)
		if err != nil {
			return nil, err
		}
	}
	var body *mod.Node
	if s.Word.Lex == lk.ASM {
		body, err = _asm(s)
		if err != nil {
			return nil, err
		}
	} else {
		body, err = block(s)
		if err != nil {
			return nil, err
		}
	}
	kw.SetLeaves([]*mod.Node{id, args, rets, vars, body, CC})
	return kw, nil
}

// Args := '[' [DeclList] ']'.
func procArgs(s *Lexer) (*mod.Node, *Error) {
	Track(s, "Args")
	_, err := expect(s, lk.LEFTBRACKET)
	if err != nil {
		return nil, err
	}

	if s.Word.Lex != lk.RIGHTBRACKET {
		n, err := declList(s)
		if err != nil {
			return nil, err
		}
		_, err = expect(s, lk.RIGHTBRACKET)
		if err != nil {
			return nil, err
		}
		return n, nil
	}

	_, err = expect(s, lk.RIGHTBRACKET)
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
		Lex: lk.TYPELIST,
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
	n := &mod.Node{Lex: lk.TYPELIST}
	n.SetLeaves(rets)
	return n, nil
}

// Type := (basicType | ProcType | Name).
func _type(s *Lexer) (*mod.Node, *Error) {
	Track(s, "type")
	switch s.Word.Lex {
	case lk.I16, lk.I8, lk.I32, lk.I64,
		lk.U16, lk.U8, lk.U32, lk.U64, lk.BOOL, lk.PTR,
		lk.VOID:
		return consume(s)
	case lk.PROC:
		return procType(s)
	case lk.IDENTIFIER:
		return name(s)
	default:
		return nil, nil
	}
}

func cc(s *Lexer) (*mod.Node, *Error) {
	_, err := consume(s)
	if err != nil {
		return nil, err
	}
	cc, err := expect(s, lk.IDENTIFIER)
	if err != nil {
		return nil, err
	}
	_, err = expect(s, lk.MORE)
	if err != nil {
		return nil, err
	}
	return cc, nil
}

// ProcType := 'proc' [CC] ProcTTList ProcTTList.
func procType(s *Lexer) (*mod.Node, *Error) {
	Track(s, "procType")
	keyword, err := expect(s, lk.PROC)
	if err != nil {
		return nil, err
	}
	var CC *mod.Node
	if s.Word.Lex == lk.LESS {
		CC, err = cc(s)
		if err != nil {
			return nil, err
		}
	}
	args, err := procTTList(s)
	if err != nil {
		return nil, err
	}
	rets, err := procTTList(s)
	if err != nil {
		return nil, err
	}
	keyword.SetLeaves([]*mod.Node{args, rets, CC})
	return keyword, err
}

// ProcTTList := '[' [TypeList] ']'.
func procTTList(s *Lexer) (*mod.Node, *Error) {
	Track(s, "procTypeTypeList")
	var tps *mod.Node
	_, err := expect(s, lk.LEFTBRACKET)
	if err != nil {
		return nil, err
	}
	if s.Word.Lex != lk.RIGHTBRACKET {
		tps, err = obligatoryTypeList(s)
		if err != nil {
			return nil, err
		}
	}
	_, err = expect(s, lk.RIGHTBRACKET)
	if err != nil {
		return nil, err
	}
	return tps, nil
}

// Struct = 'struct' id [Size] 'begin' {Field ';'} 'end'.
func structDef(s *Lexer) (*mod.Node, *Error) {
	kw, err := expect(s, lk.STRUCT)
	if err != nil {
		return nil, err
	}
	id, err := expect(s, lk.IDENTIFIER)
	if err != nil {
		return nil, err
	}
	var _size *mod.Node
	if s.Word.Lex == lk.LEFTBRACKET {
		_size, err = size(s)
		if err != nil {
			return nil, err
		}
	}
	_, err = expect(s, lk.BEGIN)
	if err != nil {
		return nil, err
	}
	fields, err := repeat(s, fieldSemicolon)
	if err != nil {
		return nil, err
	}
	fieldList := &mod.Node{
		Lex:    lk.FIELDLIST,
		Leaves: fields,
	}
	_, err = expect(s, lk.END)
	if err != nil {
		return nil, err
	}
	kw.SetLeaves([]*mod.Node{id, _size, fieldList})
	return kw, nil
}

// Size := '[' Expr ']'.
func size(s *Lexer) (*mod.Node, *Error) {
	_, err := expect(s, lk.LEFTBRACKET)
	if err != nil {
		return nil, err
	}
	exp, err := expectProd(s, expr, "expression")
	if err != nil {
		return nil, err
	}
	_, err = expect(s, lk.RIGHTBRACKET)
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
	_, err = expect(s, lk.SEMICOLON)
	if err != nil {
		return nil, err
	}
	return n, nil
}

// Field := IdList Annot [Offset].
func field(s *Lexer) (*mod.Node, *Error) {
	list, err := idList(s)
	if err != nil {
		return nil, err
	}
	if list == nil {
		return nil, nil
	}
	ann, err := annot(s)
	if err != nil {
		return nil, err
	}
	var _offset *mod.Node
	if s.Word.Lex == lk.LEFTBRACE {
		_offset, err = offset(s)
		if err != nil {
			return nil, err
		}
	}
	field := &mod.Node{
		Lex:    lk.FIELD,
		Leaves: []*mod.Node{list, ann, _offset},
	}
	return field, nil
}

// Offset := '{' Expr '}'.
func offset(s *Lexer) (*mod.Node, *Error) {
	_, err := expect(s, lk.LEFTBRACE)
	if err != nil {
		return nil, err
	}
	exp, err := expectProd(s, expr, "expression")
	if err != nil {
		return nil, err
	}
	_, err = expect(s, lk.RIGHTBRACE)
	if err != nil {
		return nil, err
	}
	return exp, nil
}

// Vars := 'var' DeclList.
func procVars(s *Lexer) (*mod.Node, *Error) {
	Track(s, "Var")
	_, err := expect(s, lk.VAR)
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
	n := &mod.Node{Lex: lk.PROCDECLS}
	n.SetLeaves(nodes)
	return n, nil
}

// Decl := idList Annot.
func decl(s *Lexer) (*mod.Node, *Error) {
	Track(s, "Decl")
	if s.Word.Lex != lk.IDENTIFIER {
		return nil, nil
	}
	list, err := idList(s)
	if err != nil {
		return nil, err
	}
	colon, err := annot(s)
	if err != nil {
		return nil, err
	}
	colon.SetLeaves([]*mod.Node{list, colon.Leaves[0]})
	return colon, nil
}

func ident(s *Lexer) (*mod.Node, *Error) {
	if s.Word.Lex == lk.IDENTIFIER {
		return consume(s)
	}
	return nil, nil
}

/*
Statement = If [';']
      | While [';']
      | DoWhile ';'
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
	case lk.EOF:
		return nil, nil
	case lk.IF:
		n, err = _if(s)
		semicolon = false
	case lk.WHILE:
		n, err = _while(s)
		semicolon = false
	case lk.DO:
		n, err = _dowhile(s)
	case lk.RETURN:
		n, err = _return(s)
	case lk.SET:
		n, err = _set(s)
	case lk.EXIT:
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
		_, err := expect(s, lk.SEMICOLON)
		if err != nil {
			return nil, err
		}
	}
	return n, nil
}

// Set := 'set' ExprList '=' Expr.
// Set = 'set' ExprList (Assign|IncDec).
func _set(s *Lexer) (*mod.Node, *Error) {
	Track(s, "Set")
	kw, err := expect(s, lk.SET)
	if err != nil {
		return nil, err
	}
	ass, err := exprList(s)
	if err != nil {
		return nil, err
	}
	var op, exp *mod.Node
	if s.Word.Lex == lk.PLUS_PLUS ||
		s.Word.Lex == lk.MINUS_MINUS {
		op, err = consume(s)
		if err != nil {
			return nil, err
		}
	} else {
		op, err = expect(s, lk.ASSIGNMENT, lk.PLUS_ASSIGN, lk.MINUS_ASSIGN,
			lk.MULTIPLICATION_ASSIGN, lk.DIVISION_ASSIGN, lk.REMAINDER_ASSIGN, lk.SWAP)
		if err != nil {
			return nil, err
		}
		exp, err = expectProd(s, expr, "expression")
		if err != nil {
			return nil, err
		}
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
	n := &mod.Node{Lex: lk.EXPRLIST}
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
	if s.Word.Lex == lk.QUESTION {
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
		err := newCompilerError(s, ek.ExpectedProd, msg)
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
	case lk.LEFTBRACKET:
		return call(s)
	case lk.AT:
		return deref(s)
	case lk.COLON:
		return annot(s)
	case lk.DOT:
		return dotAccess(s)
	case lk.ARROW:
		return arrowAccess(s)
	}
	return nil, nil
}

// DotAccess := '.' id.
func dotAccess(s *Lexer) (*mod.Node, *Error) {
	if s.Word.Lex != lk.DOT { // we use it in sizeof production too
		return nil, nil
	}
	dot, err := consume(s)
	if err != nil {
		return nil, err
	}
	field, err := expect(s, lk.IDENTIFIER)
	if err != nil {
		return nil, err
	}
	if err != nil {
		return nil, err
	}
	dot.AddLeaf(field)
	return dot, err
}

// ArrowAccess := '->' id.
func arrowAccess(s *Lexer) (*mod.Node, *Error) {
	dot, err := expect(s, lk.ARROW)
	if err != nil {
		return nil, err
	}
	field, err := expect(s, lk.IDENTIFIER)
	if err != nil {
		return nil, err
	}
	dot.AddLeaf(field)
	return dot, err
}

// Deref := '@' Type.
func deref(s *Lexer) (*mod.Node, *Error) {
	Track(s, "deref")
	at, err := expect(s, lk.AT)
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
	lB, err := expect(s, lk.LEFTBRACKET)
	if err != nil {
		return nil, err
	}

	explist, err := exprList(s)
	if err != nil {
		return nil, err
	}
	if explist == nil {
		explist = &mod.Node{
			Lex:    lk.EXPRLIST,
			Leaves: []*mod.Node{},
		}
	}

	rB, err := expect(s, lk.RIGHTBRACKET)
	if err != nil {
		return nil, err
	}
	n := &mod.Node{
		Lex: lk.CALL,
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
	colon, err := expect(s, lk.COLON)
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
	| Literal
	| NestedExpr
	| Sizeof.
*/
func factor(s *Lexer) (*mod.Node, *Error) {
	Track(s, "factor")
	switch s.Word.Lex {
	case lk.IDENTIFIER:
		return name(s)
	case lk.LEFTPAREN:
		lP, err := consume(s)
		if err != nil {
			return nil, err
		}
		n, err := expr(s)
		if err != nil {
			return nil, err
		}
		rP, err := expect(s, lk.RIGHTPAREN)
		if err != nil {
			return nil, err
		}
		n.Range = &Range{
			Begin: lP.Range.Begin,
			End:   rP.Range.End,
		}
		return n, nil
	case lk.SIZEOF:
		return sizeof(s)
	case lk.I64_LIT, lk.I32_LIT, lk.I16_LIT, lk.I8_LIT,
		lk.U64_LIT, lk.U32_LIT, lk.U16_LIT, lk.U8_LIT,
		lk.CHAR_LIT, lk.TRUE, lk.FALSE, lk.PTR_LIT:
		return consume(s)
	}
	return nil, nil
}

// Sizeof := 'sizeof' '[' Type [DotAccess] ']'.
func sizeof(s *Lexer) (*mod.Node, *Error) {
	kw, err := expect(s, lk.SIZEOF)
	if err != nil {
		return nil, err
	}
	_, err = expect(s, lk.LEFTBRACKET)
	if err != nil {
		return nil, err
	}
	t, err := expectProd(s, _type, "type")
	if err != nil {
		return nil, err
	}
	var dot *mod.Node
	if s.Word.Lex == lk.DOT {
		dot, err = dotAccess(s)
		if err != nil {
			return nil, err
		}
	}
	_, err = expect(s, lk.RIGHTBRACKET)
	if err != nil {
		return nil, err
	}
	kw.SetLeaves([]*mod.Node{t, dot})
	return kw, nil
}

// Name := id ['::' id].
func name(s *Lexer) (*mod.Node, *Error) {
	id, err := consume(s)
	if err != nil {
		return nil, err
	}
	if s.Word.Lex == lk.DOUBLECOLON {
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

// Asm := 'asm' 'begin' {AsmLine} 'end'.
func _asm(s *Lexer) (*mod.Node, *Error) {
	kw, err := expect(s, lk.ASM)
	if err != nil {
		return nil, err
	}
	_, err = expect(s, lk.BEGIN)
	if err != nil {
		return nil, err
	}
	lines, err := repeat(s, asmLine)
	if err != nil {
		return nil, err
	}
	asmlines := &mod.Node{
		Lex:    lk.ASMLINES,
		Leaves: lines,
	}
	_, err = expect(s, lk.END)
	if err != nil {
		return nil, err
	}
	kw.SetLeaves([]*mod.Node{asmlines})
	return kw, nil
}

// ClobberSet := '[' idList ']'.
func clobberSet(s *Lexer) (*mod.Node, *Error) {
	_, err := expect(s, lk.LEFTBRACKET)
	if err != nil {
		return nil, err
	}
	list, err := idList(s)
	if err != nil {
		return nil, err
	}
	_, err = expect(s, lk.RIGHTBRACKET)
	if err != nil {
		return nil, err
	}
	return list, nil
}

// AsmLine := Label | Instruction.
func asmLine(s *Lexer) (*mod.Node, *Error) {
	if s.Word.Lex == lk.DOT {
		return label(s)
	} else if s.Word.Lex == lk.IDENTIFIER {
		return instruction(s)
	} else {
		return nil, nil // used in repeat()
	}
}

// Label := '.' id ':'.
func label(s *Lexer) (*mod.Node, *Error) {
	dot, err := expect(s, lk.DOT)
	if err != nil {
		return nil, err
	}
	id, err := expect(s, lk.IDENTIFIER)
	if err != nil {
		return nil, err
	}
	_, err = expect(s, lk.COLON)
	if err != nil {
		return nil, err
	}
	dot.AddLeaf(id)
	return dot, nil
}

// Instruction := InstrName [OpList] ';'.
// InstrName := id.
func instruction(s *Lexer) (*mod.Node, *Error) {
	id, err := expect(s, lk.IDENTIFIER)
	if err != nil {
		return nil, err
	}
	ops, err := repeatCommaList(s, op)
	if err != nil {
		return nil, err
	}
	oplist := &mod.Node{
		Lex:    lk.OPLIST,
		Leaves: ops,
	}
	_, err = expect(s, lk.SEMICOLON)
	if err != nil {
		return nil, err
	}
	instr := &mod.Node{
		Lex:    lk.INSTR,
		Leaves: []*mod.Node{id, oplist},
	}
	return instr, nil
}

// Op := id | Addressing | ConstOp | Literal.
func op(s *Lexer) (*mod.Node, *Error) {
	switch s.Word.Lex {
	case lk.IDENTIFIER:
		return name(s)
	case lk.LEFTBRACKET:
		return addressing(s)
	case lk.LEFTBRACE:
		return constOp(s)
	case lk.I64_LIT, lk.I32_LIT, lk.I16_LIT, lk.I8_LIT,
		lk.U64_LIT, lk.U32_LIT, lk.U16_LIT, lk.U8_LIT,
		lk.CHAR_LIT, lk.TRUE, lk.FALSE, lk.PTR_LIT:
		return consume(s)
	default:
		return nil, nil
	}
}

// Addressing := '[' OpList ']' ['@' id].
func addressing(s *Lexer) (*mod.Node, *Error) {
	bracket, err := expect(s, lk.LEFTBRACKET)
	if err != nil {
		return nil, err
	}
	ops, err := repeatCommaList(s, op)
	if err != nil {
		return nil, err
	}
	opList := &mod.Node{
		Lex:    lk.OPLIST,
		Leaves: ops,
	}
	_, err = expect(s, lk.RIGHTBRACKET)
	if err != nil {
		return nil, err
	}
	var mode *mod.Node
	if s.Word.Lex == lk.AT {
		_, err = consume(s)
		if err != nil {
			return nil, err
		}
		mode, err = expect(s, lk.IDENTIFIER)
		if err != nil {
			return nil, err
		}
	}
	bracket.SetLeaves([]*mod.Node{opList, mode})
	return bracket, nil
}

// ConstOp := '{' Expr '}'.
func constOp(s *Lexer) (*mod.Node, *Error) {
	brace, err := expect(s, lk.LEFTBRACE)
	if err != nil {
		return nil, err
	}
	exp, err := expectProd(s, expr, "expression")
	if err != nil {
		return nil, err
	}
	_, err = expect(s, lk.RIGHTBRACE)
	if err != nil {
		return nil, err
	}
	brace.AddLeaf(exp)
	return brace, nil
}

// Block := 'begin' {Statement} 'end'.
func block(s *Lexer) (*mod.Node, *Error) {
	Track(s, "block")
	begin, err := expect(s, lk.BEGIN)
	if err != nil {
		return nil, err
	}
	leaves, err := repeat(s, statement)
	if err != nil {
		return nil, err
	}
	end, err := expect(s, lk.END)
	if err != nil {
		return nil, err
	}
	bl := &mod.Node{
		Lex: lk.BLOCK,
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
	keyword, err := expect(s, lk.IF)
	var exp, bl, else_, elseif_ *mod.Node

	exp, err = expectProd(s, expr, "expression")
	if err != nil {
		return nil, err
	}

	bl, err = expectProd(s, block, "block")
	if err != nil {
		return nil, err
	}

	if s.Word.Lex == lk.ELSEIF {
		elseif_, err = elseifchain(s)
		if err != nil {
			return nil, err
		}
	}

	if s.Word.Lex == lk.ELSE {
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
		Lex: lk.ELSEIFCHAIN,
	}
	n.SetLeaves(elses)
	return n, nil
}

func _elseif(s *Lexer) (*mod.Node, *Error) {
	if s.Word.Lex != lk.ELSEIF {
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
	keyword, err := expect(s, lk.WHILE)
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

// DoWhile = 'do' Block 'while' Expr.
func _dowhile(s *Lexer) (*mod.Node, *Error) {
	Track(s, "dowhile")
	keyword, err := expect(s, lk.DO)
	if err != nil {
		return nil, err
	}
	bl, err := block(s)
	if err != nil {
		return nil, err
	}
	_, err = expect(s, lk.WHILE)
	if err != nil {
		return nil, err
	}
	exp, err := expectProd(s, expr, "expression")
	if err != nil {
		return nil, err
	}
	keyword.SetLeaves([]*mod.Node{bl, exp})
	return keyword, nil
}

func number(s *Lexer) (*mod.Node, *Error) {
	return expect(s, lk.I64_LIT, lk.I32_LIT, lk.I16_LIT, lk.I8_LIT,
		lk.U64_LIT, lk.U32_LIT, lk.U16_LIT, lk.U8_LIT, lk.PTR_LIT)
}

func numberOrString(s *Lexer) (*mod.Node, *Error) {
	return expect(s, lk.I64_LIT, lk.I32_LIT, lk.I16_LIT, lk.I8_LIT,
		lk.U64_LIT, lk.U32_LIT, lk.U16_LIT, lk.U8_LIT, lk.PTR_LIT, lk.STRING_LIT)
}

func sumOp(n *mod.Node) bool {
	switch n.Lex {
	case lk.PLUS, lk.MINUS, lk.BITWISEOR, lk.BITWISEXOR:
		return true
	}
	return false
}

func multOp(n *mod.Node) bool {
	switch n.Lex {
	case lk.MULTIPLICATION, lk.DIVISION, lk.REMAINDER,
		lk.BITWISEAND, lk.SHIFTLEFT, lk.SHIFTRIGHT:
		return true
	}
	return false
}

func compOp(n *mod.Node) bool {
	switch n.Lex {
	case lk.LESS, lk.LESSEQ, lk.EQUALS,
		lk.MOREEQ, lk.MORE, lk.DIFFERENT:
		return true
	}
	return false
}

func orOp(n *mod.Node) bool {
	return n.Lex == lk.OR
}

func andOp(n *mod.Node) bool {
	return n.Lex == lk.AND
}

// Prefix := 'not' | '~' | '!'.
func prefixOp(st *Lexer) (*mod.Node, *Error) {
	switch st.Word.Lex {
	case lk.NOT, lk.NEG, lk.BITWISENOT:
		return consume(st)
	}
	return nil, nil
}

func isComma(n *mod.Node) bool {
	return n.Lex == lk.COMMA
}

func consume(st *Lexer) (*mod.Node, *Error) {
	n := st.Word
	err := st.Next()
	return n, err
}

func check(st *Lexer, tpList ...lk.LexKind) *Error {
	for _, tp := range tpList {
		if st.Word.Lex == tp {
			return nil
		}
	}
	message := fmt.Sprintf("Expected one of %v: instead found '%v'",
		lk.FmtToUser(tpList...),
		lk.FmtToUser(st.Word.Lex))

	err := newCompilerError(st, ek.ExpectedSymbol, message)
	return err
}

func expect(st *Lexer, tpList ...lk.LexKind) (*mod.Node, *Error) {
	for _, tp := range tpList {
		if st.Word.Lex == tp {
			return consume(st)
		}
	}
	message := fmt.Sprintf("Expected one of %v: instead found %v",
		lk.FmtToUser(tpList...),
		lk.FmtToUser(st.Word.Lex))

	err := newCompilerError(st, ek.ExpectedSymbol, message)
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
	message := fmt.Sprintf("expected %v instead found %v", name, lk.FmtToUser(st.Word.Lex))
	return newCompilerError(st, ek.ExpectedProd, message)
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
	for st.Word.Lex == lk.COMMA {
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

func createNode(nodes []*mod.Node, t lk.LexKind) *mod.Node {
	n := &mod.Node{Lex: t}
	n.SetLeaves(nodes)
	return n
}

func expectedEOF(s *Lexer) *Error {
	return newCompilerError(s, ek.ExpectedEOF, "unexpected symbol, expected EOF")
}

func newCompilerError(st *Lexer, t ek.ErrorKind, message string) *Error {
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
