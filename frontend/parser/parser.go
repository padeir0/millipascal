package parser

import (
	. "mpc/core"
	et "mpc/core/errorkind"
	ir "mpc/core/module"
	T "mpc/core/module/lexkind"
	. "mpc/core/module/util"
	. "mpc/frontend/lexer"
)

func Parse(s string) (*ir.Node, *Error) {
	st := NewLexer(s)
	err := Next(st)
	if err != nil {
		return nil, err
	}
	n, err := module(st)
	return n, err
}

// Module := {Coupling} {Symbol}.
func module(s *Lexer) (*ir.Node, *Error) {
	Track(s, "module")
	coupl, err := Repeat(s, coupling)
	if err != nil {
		return nil, err
	}
	symb, err := Repeat(s, symbol)
	if err != nil {
		return nil, err
	}
	n := &ir.Node{
		Leaves: []*ir.Node{
			CreateNode(coupl, T.COUPLINGS),
			CreateNode(symb, T.SYMBOLS),
		},
	}
	if s.Word.Lex != T.EOF {
		return nil, ExpectedEOF(s)
	}
	return n, nil
}

// Coupling := Import | FromImport | Export.
func coupling(s *Lexer) (*ir.Node, *Error) {
	switch s.Word.Lex {
	case T.IMPORT:
		return _import(s)
	case T.FROM:
		return _fromImport(s)
	case T.EXPORT:
		return _export(s)
	}
	return nil, nil
}

// Import := 'import' IdList.
func _import(s *Lexer) (*ir.Node, *Error) {
	kw, err := Expect(s, T.IMPORT)
	if err != nil {
		return nil, err
	}
	list, err := RepeatCommaList(s, expectIdent)
	if err != nil {
		return nil, err
	}
	kw.Leaves = list
	return kw, nil
}

// FromImport := 'from' id 'import' IdList.
func _fromImport(s *Lexer) (*ir.Node, *Error) {
	kw, err := Expect(s, T.FROM)
	if err != nil {
		return nil, err
	}
	id, err := Expect(s, T.IDENTIFIER)
	if err != nil {
		return nil, err
	}

	_, err = Expect(s, T.IMPORT)
	if err != nil {
		return nil, err
	}

	list, err := RepeatCommaList(s, expectIdent)
	if err != nil {
		return nil, err
	}
	listNode := &ir.Node{Lex: T.IDLIST, Leaves: list}
	kw.Leaves = []*ir.Node{id, listNode}
	return kw, nil
}

// Export := 'export' IdList.
func _export(s *Lexer) (*ir.Node, *Error) {
	kw, err := Expect(s, T.EXPORT)
	if err != nil {
		return nil, err
	}
	list, err := RepeatCommaList(s, expectIdent)
	if err != nil {
		return nil, err
	}
	kw.Leaves = list
	return kw, nil
}

// Symbol := Procedure | Memory.
func symbol(s *Lexer) (*ir.Node, *Error) {
	Track(s, "symbol")
	var n *ir.Node
	var err *Error
	switch s.Word.Lex {
	case T.PROC:
		n, err = procDef(s)
	case T.MEMORY:
		n, err = memDef(s)
	default:
		return nil, nil
	}
	return n, err
}

// Memory := 'memory' id (number|string).
func memDef(s *Lexer) (*ir.Node, *Error) {
	kw, err := Expect(s, T.MEMORY)
	if err != nil {
		return nil, err
	}
	id, err := Expect(s, T.IDENTIFIER)
	if err != nil {
		return nil, err
	}
	definition, err := numberOrString(s)
	if err != nil {
		return nil, err
	}
	kw.Leaves = []*ir.Node{id, definition}
	return kw, nil
}

// Procedure := 'proc' id [Args [Rets]] [Vars] Block 'proc'.
func procDef(s *Lexer) (*ir.Node, *Error) {
	Track(s, "Procedure")
	kw, err := Expect(s, T.PROC)
	if err != nil {
		return nil, err
	}
	var id, args, rets, vars *ir.Node
	id, err = Expect(s, T.IDENTIFIER)
	if err != nil {
		return nil, err
	}
	if s.Word.Lex == T.LEFTBRACKET {
		args, err = procArgs(s)
		if err != nil {
			return nil, err
		}
		rets, err = typeList(s)
		if err != nil {
			return nil, err
		}
	}
	if s.Word.Lex == T.VAR {
		vars, err = procVars(s)
		if err != nil {
			return nil, err
		}
	}
	body, err := block(s)
	if err != nil {
		return nil, err
	}
	_, err = Expect(s, T.PROC)
	if err != nil {
		return nil, err
	}
	kw.Leaves = []*ir.Node{id, args, rets, vars, body}
	return kw, nil
}

// Args := '[' [DeclList] ']'.
func procArgs(s *Lexer) (*ir.Node, *Error) {
	Track(s, "Args")
	_, err := Expect(s, T.LEFTBRACKET)
	if err != nil {
		return nil, err
	}

	if s.Word.Lex != T.RIGHTBRACKET {
		n, err := declList(s)
		if err != nil {
			return nil, err
		}
		_, err = Expect(s, T.RIGHTBRACKET)
		if err != nil {
			return nil, err
		}
		return n, nil
	}

	_, err = Expect(s, T.RIGHTBRACKET)
	if err != nil {
		return nil, err
	}
	return nil, nil
}

// Rets := TypeList.
// TypeList := type {',' type} [','].
func typeList(s *Lexer) (*ir.Node, *Error) {
	Track(s, "TypeList")
	rets, err := RepeatCommaList(s, _type)
	if err != nil {
		return nil, err
	}
	if rets == nil {
		return nil, nil
	}
	return &ir.Node{
		Lex:    T.TYPELIST,
		Leaves: rets,
	}, nil
}

func obligatoryTypeList(s *Lexer) (*ir.Node, *Error) {
	Track(s, "obligatoryTypeList")
	rets, err := RepeatCommaList(s, _obligatoryType)
	if err != nil {
		return nil, err
	}
	if rets == nil {
		return nil, nil
	}
	return &ir.Node{
		Lex:    T.TYPELIST,
		Leaves: rets,
	}, nil
}

// _type := basic | ProcType.
func _type(s *Lexer) (*ir.Node, *Error) {
	Track(s, "type")
	switch s.Word.Lex {
	case T.I16, T.I8, T.I32, T.I64, T.BOOL, T.PTR:
		return Consume(s)
	case T.PROC:
		return procType(s)
	}
	return nil, nil
}

// _type := basic | ProcType.
func _obligatoryType(s *Lexer) (*ir.Node, *Error) {
	return ExpectProd(s, _type, "type")
}

// ProcType := 'proc' '[' [TypeList] ']' ProcTypeRet.
// ProcTypeRet := '[' [TypeList] ']' | [Type].
func procType(s *Lexer) (*ir.Node, *Error) {
	Track(s, "procType")
	keyword, err := Expect(s, T.PROC)
	if err != nil {
		return nil, err
	}
	args, err := procTypeTypeList(s)
	if err != nil {
		return nil, err
	}
	var rets *ir.Node
	if s.Word.Lex == T.LEFTBRACKET {
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
			rets = &ir.Node{
				Lex:    T.TYPELIST,
				Leaves: []*ir.Node{t},
			}
		}
	}
	keyword.Leaves = []*ir.Node{args, rets}
	Track(s, "help")
	return keyword, err
}

func procTypeTypeList(s *Lexer) (*ir.Node, *Error) {
	Track(s, "procTypeTypeList")
	var tps *ir.Node
	_, err := Expect(s, T.LEFTBRACKET)
	if err != nil {
		return nil, err
	}
	if s.Word.Lex != T.RIGHTBRACKET {
		tps, err = obligatoryTypeList(s)
		if err != nil {
			return nil, err
		}
	}
	_, err = Expect(s, T.RIGHTBRACKET)
	if err != nil {
		return nil, err
	}
	return tps, nil
}

// Vars := 'var' DeclList.
func procVars(s *Lexer) (*ir.Node, *Error) {
	Track(s, "Var")
	_, err := Expect(s, T.VAR)
	if err != nil {
		return nil, err
	}
	return ExpectProd(s, declList, "declaration")
}

// DeclList := Decl {',' Decl} [','].
func declList(s *Lexer) (*ir.Node, *Error) {
	Track(s, "DeclList")
	nodes, err := RepeatCommaList(s, decl)
	if err != nil {
		return nil, err
	}
	if len(nodes) == 0 {
		return nil, nil
	}
	return &ir.Node{
		Lex:    T.PROCDECLS,
		Leaves: nodes,
	}, nil
}

// Decl := id [Annot].
func decl(s *Lexer) (*ir.Node, *Error) {
	Track(s, "Decl")
	if s.Word.Lex != T.IDENTIFIER {
		return nil, nil
	}
	id, err := Consume(s)
	if err != nil {
		return nil, err
	}
	if s.Word.Lex == T.COLON {
		colon, err := annot(s)
		if err != nil {
			return nil, err
		}
		colon.Leaves = []*ir.Node{id, colon.Leaves[0]}
		return colon, nil
	}
	return id, nil
}

func ident(s *Lexer) (*ir.Node, *Error) {
	if s.Word.Lex == T.IDENTIFIER {
		return Consume(s)
	}
	return nil, nil
}

func expectIdent(s *Lexer) (*ir.Node, *Error) {
	return Expect(s, T.IDENTIFIER)
}

/*
Code := If
      | While
      | Return
      | Set
      | Exit
      | Expr.
*/
func code(s *Lexer) (*ir.Node, *Error) {
	Track(s, "code")
	switch s.Word.Lex {
	case T.EOF:
		return nil, nil
	case T.IF:
		return _if(s)
	case T.WHILE:
		return _while(s)
	case T.RETURN:
		return _return(s)
	case T.SET:
		return _set(s)
	case T.EXIT:
		return _exit(s)
	default:
		return expr(s)
	}
}

// Set := 'set' ExprList '=' Expr.
func _set(s *Lexer) (*ir.Node, *Error) {
	Track(s, "Set")
	kw, err := Expect(s, T.SET)
	if err != nil {
		return nil, err
	}
	ass, err := exprList(s)
	if err != nil {
		return nil, err
	}
	op, err := Expect(s, T.ASSIGNMENT, T.PLUS_ASSIGN, T.MINUS_ASSIGN,
		T.MULTIPLICATION_ASSIGN, T.DIVISION_ASSIGN, T.REMAINDER_ASSIGN)
	if err != nil {
		return nil, err
	}
	exp, err := ExpectProd(s, expr, "expression")
	if err != nil {
		return nil, err
	}
	kw.Leaves = []*ir.Node{ass, op, exp}
	return kw, nil
}

// ExprList := Expr {',' Expr} [','].
func exprList(s *Lexer) (*ir.Node, *Error) {
	asses, err := RepeatCommaList(s, expr)
	if err != nil {
		return nil, err
	}
	return &ir.Node{
		Lex:    T.EXPRLIST,
		Leaves: asses,
	}, nil
}

// Return := 'return' [ExprList].
func _return(s *Lexer) (*ir.Node, *Error) {
	Track(s, "return")
	kw, err := Consume(s)
	if err != nil {
		return nil, err
	}
	exp, err := RepeatCommaList(s, expr) // optional
	if err != nil {
		return nil, err
	}
	if exp != nil {
		kw.Leaves = exp
	}
	return kw, err
}

// Exit := 'exit' Expr.
func _exit(s *Lexer) (*ir.Node, *Error) {
	Track(s, "exit")
	kw, err := Consume(s)
	if err != nil {
		return nil, err
	}
	exp, err := ExpectProd(s, expr, "expression")
	if err != nil {
		return nil, err
	}
	kw.Leaves = []*ir.Node{exp}
	return kw, err
}

// Expr = And {"or" And}.
func expr(s *Lexer) (*ir.Node, *Error) {
	Track(s, "Expr")
	return RepeatBinary(s, boolAnd, "expression", orOp)
}

// And = Comp {"and" Comp}.
func boolAnd(s *Lexer) (*ir.Node, *Error) {
	Track(s, "And")
	return RepeatBinary(s, comparative, "expression", andOp)
}

// Comp = Sum {compOp Sum}.
func comparative(s *Lexer) (*ir.Node, *Error) {
	Track(s, "Comp")
	return RepeatBinary(s, additive, "expression", compOp)
}

// Sum = Mult {sumOp Mult}.
func additive(s *Lexer) (*ir.Node, *Error) {
	Track(s, "Sum")
	return RepeatBinary(s, multiplicative, "expression", sumOp)
}

// Mult = Unary {multOp Unary}.
func multiplicative(s *Lexer) (*ir.Node, *Error) {
	Track(s, "Mult")
	return RepeatBinary(s, unaryPrefix, "expression", multOp)
}

// Prefix = "not" | "~".
// UnaryPrefix := {Prefix} UnarySuffix.
func unaryPrefix(s *Lexer) (*ir.Node, *Error) {
	Track(s, "unary prefix")

	preFirst, preLast, err := RepeatUnaryLeft(s, prefixOp)
	if err != nil {
		return nil, err
	}

	suff, err := unarySuffix(s)
	if err != nil {
		return nil, err
	}
	if preFirst != nil && suff == nil {
		msg := "expected expression after prefix operator"
		err := NewCompilerError(s, et.ExpectedProd, msg)
		return nil, err
	}
	if suff == nil {
		return nil, nil
	}

	if preFirst != nil {
		AddLeaf(preLast, suff)
		suff = preFirst
	}

	return suff, nil
}

// UnarySuffix := Factor {Suffix}.
func unarySuffix(s *Lexer) (*ir.Node, *Error) {
	Track(s, "unary suffix")
	fact, err := factor(s)
	if err != nil {
		return nil, err
	}
	suFirst, suLast, err := RepeatUnaryRight(s, suffix)
	if err != nil {
		return nil, err
	}
	if suFirst != nil {
		AddLeaf(suFirst, fact)
		fact = suLast
	}
	return fact, nil
}

/*
Suffix  := Conversion
	| Deref
	| Call.
*/
func suffix(s *Lexer) (*ir.Node, *Error) {
	Track(s, "suffix")
	switch s.Word.Lex {
	case T.LEFTBRACKET:
		return call(s)
	case T.AT:
		return deref(s)
	case T.COLON:
		return annot(s)
	case T.DOT:
		return propertyAccess(s)
	}
	return nil, nil
}

// PropertyAccess := '.' id.
func propertyAccess(s *Lexer) (*ir.Node, *Error) {
	dot, err := Expect(s, T.DOT)
	if err != nil {
		return nil, err
	}
	id, err := Expect(s, T.IDENTIFIER)
	if err != nil {
		return nil, err
	}
	dot.Leaves = []*ir.Node{id}
	return dot, err
}

// Deref := '@' Type.
func deref(s *Lexer) (*ir.Node, *Error) {
	Track(s, "deref")
	at, err := Expect(s, T.AT)
	if err != nil {
		return nil, err
	}
	t, err := ExpectProd(s, _type, "type")
	if err != nil {
		return nil, err
	}
	at.Leaves = []*ir.Node{t}
	return at, nil
}

// Call = "[" [ExprList] "]".
func call(s *Lexer) (*ir.Node, *Error) {
	Track(s, "call")
	_, err := Expect(s, T.LEFTBRACKET)
	if err != nil {
		return nil, err
	}

	explist, err := exprList(s)
	if err != nil {
		return nil, err
	}
	if explist == nil {
		explist = &ir.Node{
			Lex:    T.EXPRLIST,
			Leaves: []*ir.Node{},
		}
	}

	_, err = Expect(s, T.RIGHTBRACKET)
	if err != nil {
		return nil, err
	}
	return &ir.Node{
		Lex:    T.CALL,
		Leaves: []*ir.Node{explist},
	}, nil
}

// Annot = ":" Type.
// Conversion = Annot.
func annot(s *Lexer) (*ir.Node, *Error) {
	Track(s, "annot")
	colon, err := Expect(s, T.COLON)
	if err != nil {
		return nil, err
	}
	tp, err := ExpectProd(s, _type, "type")
	if err != nil {
		return nil, err
	}
	colon.Leaves = []*ir.Node{tp}
	return colon, nil
}

/*
Factor := Name
	| Terminal
	| NestedExpr.
*/
func factor(s *Lexer) (*ir.Node, *Error) {
	Track(s, "factor")
	switch s.Word.Lex {
	case T.IDENTIFIER:
		return name(s)
	case T.LEFTPAREN:
		_, err := Consume(s)
		if err != nil {
			return nil, err
		}
		n, err := expr(s)
		if err != nil {
			return nil, err
		}
		_, err = Expect(s, T.RIGHTPAREN)
		if err != nil {
			return nil, err
		}
		return n, nil
	case T.I64_LIT, T.I32_LIT, T.I16_LIT, T.I8_LIT, T.CHAR_LIT,
		T.TRUE, T.FALSE, T.PTR_LIT:
		return Consume(s)
	}
	return nil, nil
}

func name(s *Lexer) (*ir.Node, *Error) {
	id, err := Consume(s)
	if err != nil {
		return nil, err
	}
	if s.Word.Lex == T.DOUBLECOLON {
		dcolon, err := Consume(s)
		if err != nil {
			return nil, err
		}
		id2, err := Consume(s)
		if err != nil {
			return nil, err
		}
		dcolon.Leaves = []*ir.Node{id, id2}
		return dcolon, nil
	}
	return id, nil
}

// Block := 'begin' {CodeSemicolon} 'end'.
func block(s *Lexer) (*ir.Node, *Error) {
	Track(s, "block")
	_, err := Expect(s, T.BEGIN)
	if err != nil {
		return nil, err
	}
	n, err := Repeat(s, codeSemicolon)
	if err != nil {
		return nil, err
	}
	_, err = Expect(s, T.END)
	if err != nil {
		return nil, err
	}
	return &ir.Node{
		Leaves: n,
		Lex:    T.BLOCK,
	}, nil
}

// CodeSemicolon := Code [';'].
func codeSemicolon(s *Lexer) (*ir.Node, *Error) {
	Track(s, "CodeSemicolon")
	n, err := code(s)
	if err != nil {
		return nil, err
	}
	if s.Word.Lex == T.SEMICOLON {
		_, err := Consume(s)
		if err != nil {
			return nil, err
		}
	}
	return n, nil
}

// If := 'if' Expr Block [Else] 'if'.
func _if(s *Lexer) (*ir.Node, *Error) {
	Track(s, "if")
	keyword, err := Expect(s, T.IF)
	var exp, bl, else_, elseif_ *ir.Node

	exp, err = ExpectProd(s, expr, "expression")
	if err != nil {
		return nil, err
	}

	bl, err = ExpectProd(s, block, "block")
	if err != nil {
		return nil, err
	}

	if s.Word.Lex == T.ELSEIF {
		elseif_, err = elseifchain(s)
		if err != nil {
			return nil, err
		}
	}

	if s.Word.Lex == T.ELSE {
		else_, err = _else(s)
		if err != nil {
			return nil, err
		}
	}
	keyword.Leaves = []*ir.Node{exp, bl, elseif_, else_}
	_, err = Expect(s, T.IF)
	if err != nil {
		return nil, err
	}
	return keyword, nil
}

// Else := 'else' Block.
func _else(s *Lexer) (*ir.Node, *Error) {
	Track(s, "else")
	kw, err := Consume(s)
	if err != nil {
		return nil, err
	}
	bl, err := block(s)
	if err != nil {
		return nil, err
	}
	kw.Leaves = []*ir.Node{bl}
	return kw, nil
}

func elseifchain(s *Lexer) (*ir.Node, *Error) {
	elses, err := Repeat(s, _elseif)
	if err != nil {
		return nil, err
	}
	return &ir.Node{
		Lex:    T.ELSEIFCHAIN,
		Leaves: elses,
	}, nil
}

func _elseif(s *Lexer) (*ir.Node, *Error) {
	if s.Word.Lex != T.ELSEIF {
		return nil, nil
	}
	kw, err := Consume(s)
	if err != nil {
		return nil, err
	}
	exp, err := ExpectProd(s, expr, "expression")
	if err != nil {
		return nil, err
	}
	bl, err := block(s)
	if err != nil {
		return nil, err
	}
	kw.Leaves = []*ir.Node{exp, bl}
	return kw, nil
}

// While = 'while' Expr Block 'while'.
func _while(s *Lexer) (*ir.Node, *Error) {
	Track(s, "while")
	keyword, err := Expect(s, T.WHILE)
	if err != nil {
		return nil, err
	}
	exp, err := ExpectProd(s, expr, "expression")
	if err != nil {
		return nil, err
	}
	bl, err := block(s)
	if err != nil {
		return nil, err
	}
	keyword.Leaves = []*ir.Node{exp, bl}
	_, err = Expect(s, T.WHILE)
	if err != nil {
		return nil, err
	}
	return keyword, nil
}

func numberOrString(s *Lexer) (*ir.Node, *Error) {
	return Expect(s, T.I64_LIT, T.I32_LIT, T.I16_LIT, T.I8_LIT, T.PTR_LIT, T.STRING_LIT)
}

func sumOp(n *ir.Node) bool {
	switch n.Lex {
	case T.PLUS, T.MINUS:
		return true
	}
	return false
}

func multOp(n *ir.Node) bool {
	switch n.Lex {
	case T.MULTIPLICATION, T.DIVISION, T.REMAINDER:
		return true
	}
	return false
}

func compOp(n *ir.Node) bool {
	switch n.Lex {
	case T.LESS, T.LESSEQ, T.EQUALS,
		T.MOREEQ, T.MORE, T.DIFFERENT:
		return true
	}
	return false
}

func orOp(n *ir.Node) bool {
	return n.Lex == T.OR
}

func andOp(n *ir.Node) bool {
	return n.Lex == T.AND
}

func prefixOp(st *Lexer) (*ir.Node, *Error) {
	switch st.Word.Lex {
	case T.NOT, T.NEG:
		return Consume(st)
	}
	return nil, nil
}

func isComma(n *ir.Node) bool {
	return n.Lex == T.COMMA
}
