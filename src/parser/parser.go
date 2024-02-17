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

// Module := {Coupling} {Symbol}.
func module(s *Lexer) (*mod.Node, *Error) {
	Track(s, "module")
	coupl, err := repeat(s, coupling)
	if err != nil {
		return nil, err
	}
	symb, err := repeat(s, symbol)
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

// Import := 'import' IdList.
func _import(s *Lexer) (*mod.Node, *Error) {
	kw, err := expect(s, lex.IMPORT)
	if err != nil {
		return nil, err
	}
	list, err := repeatCommaList(s, ident)
	if err != nil {
		return nil, err
	}
	kw.SetLeaves(list)
	return kw, nil
}

// FromImport := 'from' id 'import' IdList.
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

	list, err := repeatCommaList(s, ident)
	if err != nil {
		return nil, err
	}
	listNode := &mod.Node{Lex: lex.IDLIST}
	listNode.SetLeaves(list)
	kw.SetLeaves([]*mod.Node{id, listNode})
	return kw, nil
}

// Export := 'export' IdList.
func _export(s *Lexer) (*mod.Node, *Error) {
	kw, err := expect(s, lex.EXPORT)
	if err != nil {
		return nil, err
	}
	list, err := repeatCommaList(s, ident)
	if err != nil {
		return nil, err
	}
	kw.SetLeaves(list)
	return kw, nil
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
	default:
		return nil, nil
	}
	return n, err
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

// MultipleConst := 'begin' {SingleConst [';']} 'end' 'const'.
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
	_, err = expect(s, lex.CONST)
	if err != nil {
		return nil, err
	}
	kw.SetLeaves(leafs)
	return kw, nil
}

func singleConstSemicolon(s *Lexer) (*mod.Node, *Error) {
	n, err := singleConst(s)
	if err != nil {
		return nil, err
	}
	return n, optSemicolon(s)
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

// MultipleData := 'begin' {SingleData [';']} 'end' 'data'.
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
	_, err = expect(s, lex.DATA)
	if err != nil {
		return nil, err
	}
	kw.SetLeaves(leafs)
	return kw, nil
}

// SingleData [';']
func singleDataSemicolon(s *Lexer) (*mod.Node, *Error) {
	sg, err := singleData(s)
	if err != nil {
		return nil, err
	}
	return sg, optSemicolon(s)
}

// SingleData := id (dExpr|string).
func singleData(s *Lexer) (*mod.Node, *Error) {
	id, err := expect(s, lex.IDENTIFIER)
	if err != nil {
		return nil, err
	}

	err = check(s, lex.LEFTBRACKET, lex.STRING_LIT)
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
	}

	sg := &mod.Node{
		Lex: lex.SINGLE,
	}
	sg.SetLeaves([]*mod.Node{id, definition})
	return sg, nil
}

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

// Procedure := 'proc' id [Args [Rets]] [Vars] Block 'proc'.
func procDef(s *Lexer) (*mod.Node, *Error) {
	Track(s, "Procedure")
	kw, err := expect(s, lex.PROC)
	if err != nil {
		return nil, err
	}
	var id, args, rets, vars *mod.Node
	id, err = expect(s, lex.IDENTIFIER)
	if err != nil {
		return nil, err
	}
	if s.Word.Lex == lex.LEFTBRACKET {
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
	_, err = expect(s, lex.PROC)
	if err != nil {
		return nil, err
	}
	kw.SetLeaves([]*mod.Node{id, args, rets, vars, body})
	return kw, nil
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

// _type := basic | ProcType.
func _type(s *Lexer) (*mod.Node, *Error) {
	Track(s, "type")
	switch s.Word.Lex {
	case lex.I16, lex.I8, lex.I32, lex.I64,
		lex.U16, lex.U8, lex.U32, lex.U64, lex.BOOL, lex.PTR:
		return consume(s)
	case lex.PROC:
		return procType(s)
	}
	return nil, nil
}

// ProcType := 'proc' '[' [TypeList] ']' ProcTypeRet.
// ProcTypeRet := '[' [TypeList] ']' | [Type].
func procType(s *Lexer) (*mod.Node, *Error) {
	Track(s, "procType")
	keyword, err := expect(s, lex.PROC)
	if err != nil {
		return nil, err
	}
	args, err := procTypeTypeList(s)
	if err != nil {
		return nil, err
	}
	var rets *mod.Node
	if s.Word.Lex == lex.LEFTBRACKET {
		rets, err = procTypeTypeList(s)
		if err != nil {
			return nil, err
		}
	} else { // sorry
		t, err := _type(s)
		if err != nil {
			return nil, err
		}
		if t == nil {
			rets = nil
		} else {
			rets = &mod.Node{Lex: lex.TYPELIST}
			rets.AddLeaf(t)
		}
	}
	keyword.SetLeaves([]*mod.Node{args, rets})
	Track(s, "help")
	return keyword, err
}

func procTypeTypeList(s *Lexer) (*mod.Node, *Error) {
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

// Decl := id [Annot].
func decl(s *Lexer) (*mod.Node, *Error) {
	Track(s, "Decl")
	if s.Word.Lex != lex.IDENTIFIER {
		return nil, nil
	}
	id, err := consume(s)
	if err != nil {
		return nil, err
	}
	if s.Word.Lex == lex.COLON {
		colon, err := annot(s)
		if err != nil {
			return nil, err
		}
		colon.SetLeaves([]*mod.Node{id, colon.Leaves[0]})
		return colon, nil
	}
	return id, nil
}

func ident(s *Lexer) (*mod.Node, *Error) {
	if s.Word.Lex == lex.IDENTIFIER {
		return consume(s)
	}
	return nil, nil
}

/*
Code := If
      | While
      | Return
      | Set
      | Exit
      | Expr.
*/
func code(s *Lexer) (*mod.Node, *Error) {
	Track(s, "code")
	switch s.Word.Lex {
	case lex.EOF:
		return nil, nil
	case lex.IF:
		return _if(s)
	case lex.WHILE:
		return _while(s)
	case lex.RETURN:
		return _return(s)
	case lex.SET:
		return _set(s)
	case lex.EXIT:
		return _exit(s)
	default:
		return expr(s)
	}
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

// Exit := 'exit' Expr.
func _exit(s *Lexer) (*mod.Node, *Error) {
	Track(s, "exit")
	kw, err := consume(s)
	if err != nil {
		return nil, err
	}
	exp, err := expectProd(s, expr, "expression")
	if err != nil {
		return nil, err
	}
	kw.SetLeaves([]*mod.Node{exp})
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
Suffix  := Conversion
	| Deref
	| Call.
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
		return propertyAccess(s)
	}
	return nil, nil
}

// PropertyAccess := '.' id.
func propertyAccess(s *Lexer) (*mod.Node, *Error) {
	dot, err := expect(s, lex.DOT)
	if err != nil {
		return nil, err
	}
	id, err := expect(s, lex.IDENTIFIER)
	if err != nil {
		return nil, err
	}
	dot.AddLeaf(id)
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
		kw, err := consume(s)
		if err != nil {
			return nil, err
		}
		t, err := expectProd(s, _type, "type")
		if err != nil {
			return nil, err
		}
		kw.AddLeaf(t)
		return kw, nil
	case lex.I64_LIT, lex.I32_LIT, lex.I16_LIT, lex.I8_LIT,
		lex.U64_LIT, lex.U32_LIT, lex.U16_LIT, lex.U8_LIT,
		lex.CHAR_LIT, lex.TRUE, lex.FALSE, lex.PTR_LIT:
		return consume(s)
	}
	return nil, nil
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

// Block := 'begin' {CodeSemicolon} 'end'.
func block(s *Lexer) (*mod.Node, *Error) {
	Track(s, "block")
	begin, err := expect(s, lex.BEGIN)
	if err != nil {
		return nil, err
	}
	leaves, err := repeat(s, codeSemicolon)
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

// CodeSemicolon := Code [';'].
func codeSemicolon(s *Lexer) (*mod.Node, *Error) {
	Track(s, "CodeSemicolon")
	n, err := code(s)
	if err != nil {
		return nil, err
	}
	return n, optSemicolon(s)
}

func optSemicolon(s *Lexer) *Error {
	if s.Word.Lex == lex.SEMICOLON {
		_, err := consume(s)
		if err != nil {
			return err
		}
	}
	return nil
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
	_, err = expect(s, lex.IF)
	if err != nil {
		return nil, err
	}
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
	_, err = expect(s, lex.WHILE)
	if err != nil {
		return nil, err
	}
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

func prefixOp(st *Lexer) (*mod.Node, *Error) {
	switch st.Word.Lex {
	case lex.NOT, lex.NEG, lex.BITWISENOT:
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
