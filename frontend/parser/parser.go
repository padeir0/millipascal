package parser

import (
	et "mpc/frontend/enums/errType"
	T "mpc/frontend/enums/lexType"
	"mpc/frontend/errors"
	"mpc/frontend/ir"
	. "mpc/frontend/lexer"
	. "mpc/frontend/util/parser"
)

func Parse(s string) (*ir.Node, *errors.CompilerError) {
	st := NewLexer(s)
	err := Next(st)
	if err != nil {
		return nil, err
	}
	n, err := module(st)
	return n, err
}

// Module := {Symbol}.
func module(s *Lexer) (*ir.Node, *errors.CompilerError) {
	Track(s, "module")
	symb, err := Repeat(s, symbol)
	if err != nil {
		return nil, err
	}
	n := &ir.Node{
		Leaves: []*ir.Node{
			CreateNode(symb, T.SYMBOLS),
		},
	}
	if s.Word.Lex != T.EOF {
		return nil, ExpectedEOF(s)
	}
	return n, nil
}

// Symbol := Procedure | Memory.
func symbol(s *Lexer) (*ir.Node, *errors.CompilerError) {
	var n *ir.Node
	var err *errors.CompilerError
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

// Memory := 'memory' id int.
func memDef(s *Lexer) (*ir.Node, *errors.CompilerError) {
	kw, err := Expect(s, T.MEMORY)
	if err != nil {
		return nil, err
	}
	id, err := Expect(s, T.IDENTIFIER)
	if err != nil {
		return nil, err
	}
	size, err := Expect(s, T.INT_LIT)
	if err != nil {
		return nil, err
	}
	kw.Leaves = []*ir.Node{id, size}
	return kw, nil
}

// Procedure := 'proc' id [Args [Rets]] [Vars] Block 'proc'.
func procDef(s *Lexer) (*ir.Node, *errors.CompilerError) {
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
		rets, err = procRets(s)
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
func procArgs(s *Lexer) (*ir.Node, *errors.CompilerError) {
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

// Rets := type {',' type}.
func procRets(s *Lexer) (*ir.Node, *errors.CompilerError) {
	Track(s, "Rets")
	rets, err := RepeatList(s, _type, isComma)
	if err != nil {
		return nil, err
	}
	if rets == nil {
		return nil, nil
	}
	return &ir.Node{
		Lex:    T.PROCDECLS,
		Leaves: rets,
	}, nil
}

func _type(s *Lexer) (*ir.Node, *errors.CompilerError) {
	Track(s, "type")
	switch s.Word.Lex {
	case T.I16, T.I8, T.I32, T.I64, T.BOOL, T.PTR:
		return Consume(s)
	}
	return nil, nil
}

// Vars := 'var' DeclList.
func procVars(s *Lexer) (*ir.Node, *errors.CompilerError) {
	Track(s, "Var")
	_, err := Expect(s, T.VAR)
	if err != nil {
		return nil, err
	}
	return ExpectProd(s, declList, "declaration")
}

// DeclList := Decl {',' Decl} [','].
func declList(s *Lexer) (*ir.Node, *errors.CompilerError) {
	Track(s, "DeclList")
	nodes, err := RepeatList(s, decl, isComma)
	if err != nil {
		return nil, err
	}
	if len(nodes) == 0 {
		return nil, nil
	}
	if s.Word.Lex == T.COMMA {
		_, err := Consume(s)
		if err != nil {
			return nil, err
		}
	}
	return &ir.Node{
		Lex:    T.PROCDECLS,
		Leaves: nodes,
	}, nil
}

// Decl := id [Annot].
func decl(s *Lexer) (*ir.Node, *errors.CompilerError) {
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

// IdList = ident {"," ident} [","].
func idList(s *Lexer) (*ir.Node, *errors.CompilerError) {
	Track(s, "IdList")
	nodes, err := RepeatList(s, ident, isComma)
	if err != nil {
		return nil, err
	}
	if nodes == nil {
		return nil, nil
	}
	if s.Word.Lex == T.COMMA {
		_, err = Consume(s)
		if err != nil {
			return nil, err
		}
	}
	return &ir.Node{
		Lex:    T.IDLIST,
		Leaves: nodes,
	}, nil
}

func ident(s *Lexer) (*ir.Node, *errors.CompilerError) {
	if s.Word.Lex == T.IDENTIFIER {
		return Consume(s)
	}
	return nil, nil
}

/*
Code := If
      | While
      | Return
      | Copy
      | Set
      | Expr.
*/
func code(s *Lexer) (*ir.Node, *errors.CompilerError) {
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
	default:
		return expr(s)
	}
}

// Set := 'set' ExprList '=' Expr.
func _set(s *Lexer) (*ir.Node, *errors.CompilerError) {
	Track(s, "Set")
	kw, err := Expect(s, T.SET)
	if err != nil {
		return nil, err
	}
	ass, err := exprList(s)
	if err != nil {
		return nil, err
	}
	_, err = Expect(s, T.ASSIGNMENT)
	if err != nil {
		return nil, err
	}
	exp, err := ExpectProd(s, expr, "expression")
	if err != nil {
		return nil, err
	}
	kw.Leaves = []*ir.Node{ass, exp}
	return kw, nil
}

// ExprList := Expr {',' Expr} [','].
func exprList(s *Lexer) (*ir.Node, *errors.CompilerError) {
	asses, err := RepeatList(s, expr, isComma)
	if err != nil {
		return nil, err
	}
	if isComma(s.Word) {
		_, err := Consume(s)
		if err != nil {
			return nil, err
		}
	}
	return &ir.Node{
		Lex:    T.EXPRLIST,
		Leaves: asses,
	}, nil
}

// Return := 'return' [ExprList].
func _return(s *Lexer) (*ir.Node, *errors.CompilerError) {
	Track(s, "return")
	kw, err := Consume(s)
	if err != nil {
		return nil, err
	}
	exp, err := RepeatList(s, expr, isComma) // optional
	if err != nil {
		return nil, err
	}
	if exp != nil {
		kw.Leaves = exp
	}
	return kw, err
}

// Expr = And {"or" And}.
func expr(s *Lexer) (*ir.Node, *errors.CompilerError) {
	Track(s, "Expr")
	return RepeatBinary(s, boolAnd, "expression", orOp)
}

// And = Comp {"and" Comp}.
func boolAnd(s *Lexer) (*ir.Node, *errors.CompilerError) {
	Track(s, "And")
	return RepeatBinary(s, comparative, "expression", andOp)
}

// Comp = Sum {compOp Sum}.
func comparative(s *Lexer) (*ir.Node, *errors.CompilerError) {
	Track(s, "Comp")
	return RepeatBinary(s, additive, "expression", compOp)
}

// Sum = Mult {sumOp Mult}.
func additive(s *Lexer) (*ir.Node, *errors.CompilerError) {
	Track(s, "Sum")
	return RepeatBinary(s, multiplicative, "expression", sumOp)
}

// Mult = Unary {multOp Unary}.
func multiplicative(s *Lexer) (*ir.Node, *errors.CompilerError) {
	Track(s, "Mult")
	return RepeatBinary(s, unaryPrefix, "expression", multOp)
}

// Prefix = "not" | "+" | "-".
// UnaryPrefix := {Prefix} UnarySuffix.
func unaryPrefix(s *Lexer) (*ir.Node, *errors.CompilerError) {
	Track(s, "unary suffix")

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
func unarySuffix(s *Lexer) (*ir.Node, *errors.CompilerError) {
	Track(s, "unaryPrefix")
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
func suffix(s *Lexer) (*ir.Node, *errors.CompilerError) {
	switch s.Word.Lex {
	case T.LEFTBRACKET:
		return call(s)
	case T.AT:
		return Deref(s)
	case T.COLON:
		return annot(s)
	}
	return nil, nil
}

func Deref(s *Lexer) (*ir.Node, *errors.CompilerError) {
	at, err := Expect(s, T.AT)
	if err != nil {
		return nil, err
	}
	t, err := ExpectType(s)
	if err != nil {
		return nil, err
	}
	at.Leaves = []*ir.Node{t}
	return at, nil
}

// Call = "[" [ExprList] "]".
func call(s *Lexer) (*ir.Node, *errors.CompilerError) {
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
func annot(s *Lexer) (*ir.Node, *errors.CompilerError) {
	colon, err := Expect(s, T.COLON)
	if err != nil {
		return nil, err
	}
	tp, err := ExpectType(s)
	if err != nil {
		return nil, err
	}
	colon.Leaves = []*ir.Node{tp}
	return colon, nil
}

/*
Factor := Terminal
	| NestedExpr.
*/
func factor(s *Lexer) (*ir.Node, *errors.CompilerError) {
	Track(s, "factor")
	switch s.Word.Lex {
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
	case T.INT_LIT, T.TRUE, T.FALSE, T.IDENTIFIER, T.PTR_LIT:
		return Consume(s)
	}
	return nil, nil
}

// Block := 'begin' {CodeSemicolon} 'end'.
func block(s *Lexer) (*ir.Node, *errors.CompilerError) {
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
func codeSemicolon(s *Lexer) (*ir.Node, *errors.CompilerError) {
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
func _if(s *Lexer) (*ir.Node, *errors.CompilerError) {
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
func _else(s *Lexer) (*ir.Node, *errors.CompilerError) {
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

func elseifchain(s *Lexer) (*ir.Node, *errors.CompilerError) {
	elses, err := Repeat(s, _elseif)
	if err != nil {
		return nil, err
	}
	return &ir.Node{
		Lex:    T.ELSEIFCHAIN,
		Leaves: elses,
	}, nil
}

func _elseif(s *Lexer) (*ir.Node, *errors.CompilerError) {
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
func _while(s *Lexer) (*ir.Node, *errors.CompilerError) {
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

func prefixOp(st *Lexer) (*ir.Node, *errors.CompilerError) {
	switch st.Word.Lex {
	case T.NOT, T.PLUS, T.MINUS:
		return Consume(st)
	}
	return nil, nil
}

func isComma(n *ir.Node) bool {
	return n.Lex == T.COMMA
}
