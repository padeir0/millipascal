package parser

import (
	"mpc/frontend/ast"
	T "mpc/frontend/lexType"
	"mpc/frontend/errors"
	et "mpc/frontend/ErrType"
	. "mpc/frontend/lexer"
	. "mpc/frontend/parser_util"
)

func Parse(s string) (*ast.Node, *errors.CompilerError) {
	st := NewLexer(s)
	err := Next(st)
	if err != nil {
		return nil, err
	}
	n, err := module(st)
	return n, err
}

// Module := {Symbol}.
func module(s *Lexer) (*ast.Node, *errors.CompilerError) {
	Track(s, "module")
	symb, err := Repeat(s, symbol)
	if err != nil {
		return nil, err
	}
	n := &ast.Node{
		Leaves: []*ast.Node{
			CreateNode(symb, T.SYMBOLS),
		},
	}
	if s.Word.Lex != T.EOF {
		return nil, ExpectedEOF(s)
	}
	return n, nil
}
// Symbol := Procedure | Memory | Const.
func symbol(s *Lexer) (*ast.Node, *errors.CompilerError) {
	var n *ast.Node
	var err *errors.CompilerError
	switch s.Word.Lex {
	case T.PROC:
		n, err = procDef(s)
	case T.MEM:
		n, err = memDef(s)
	case T.CONST:
		n, err = constDef(s)
	default:
		return nil, nil
	}
	return n, err
}

// Memory := 'mem' id (Reserve | Declare).
func memDef(s *Lexer) (*ast.Node, *errors.CompilerError) {
	kw, err := Expect(s, T.MEM)
	if err != nil {
		return nil, err
	}
	id, err := Expect(s, T.IDENTIFIER)
	if err != nil {
		return nil, err
	}
	var rest *ast.Node
	switch s.Word.Lex {
	case T.RES:
		rest, err = reserve(s)
		if err != nil {
			return nil, err
		}
	case T.DEF:
		rest, err = declare(s)
		if err != nil {
			return nil, err
		}
	}
	kw.Leaves = []*ast.Node{id, rest}
	return kw, nil
}

func reserve(s *Lexer) (*ast.Node, *errors.CompilerError) {
	kw, err := Expect(s, T.RES)
	if err != nil {
		return nil, err
	}
	size, err := Expect(s, T.INT)
	if err != nil {
		return nil, err
	}
	t, err := ExpectType(s)
	if err != nil {
		return nil, err
	}
	_, err = Expect(s, T.SET)
	if err != nil {
		return nil, err
	}
	term, err := terminal(s)
	if err != nil {
		return nil, err
	}
	kw.Leaves = []*ast.Node{size, t, term}
	return kw, nil
}

func declare(s *Lexer) (*ast.Node, *errors.CompilerError){
	kw, err := Expect(s, T.DEF)
	if err != nil {
		return nil, err
	}
	terms, err := termList(s)
	if err != nil {
		return nil, err
	}
	kw.Leaves = terms.Leaves
	return kw, nil
}

// Procedure := 'proc' id [Args [Rets]] [Vars] Block 'proc'.
func procDef(s *Lexer) (*ast.Node, *errors.CompilerError) {
	Track(s, "Procedure")
	kw, err := Expect(s, T.PROC)
	if err != nil {
		return nil, err
	}
	var id, args, rets, vars *ast.Node
	id, err = Expect(s, T.IDENTIFIER)
	if err != nil {
		return nil, err
	}
	if s.Word.Lex == T.LEFTPAREN {
		args, err = procArgs(s)
		if err != nil {
			return nil, err
		}
		rets, err = procRets(s)
		if err != nil {
			return nil, err
		}
	}
	if s.Word.Lex == T.VARS {
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
	kw.Leaves = []*ast.Node{id, args, rets, vars, body}
	return kw, nil
}

// Args := '(' [DeclList] ')'.
func procArgs(s *Lexer) (*ast.Node, *errors.CompilerError) {
	Track(s, "Args")
	_, err := Expect(s, T.LEFTPAREN)
	if err != nil {
		return nil, err
	}

	if s.Word.Lex != T.RIGHTPAREN {
		n, err := declList(s)
		if err != nil {
			return nil, err
		}
		_, err = Expect(s, T.RIGHTPAREN)
		if err != nil {
			return nil, err
		}
		return n, nil
	}

	_, err = Expect(s, T.RIGHTPAREN)
	if err != nil {
		return nil, err
	}
	return nil, nil
}

// Rets := type {',' type}.
func procRets(s *Lexer) (*ast.Node, *errors.CompilerError) {
	Track(s, "Rets")
	rets, err := RepeatList(s, _type, isComma)
	if err != nil {
		return nil, err
	}
	if rets == nil {
		return nil, nil
	}
	return &ast.Node {
		Lex:    T.PROCDECLS,
		Leaves: rets,
	}, nil
}

func _type(s *Lexer) (*ast.Node, *errors.CompilerError) {
	Track(s, "type")
	switch s.Word.Lex {
	case T.WORD, T.BYTE, T.DWORD, T.QWORD:
		return Consume(s)
	}
	return nil, nil
}

// Vars := 'vars' DeclList.
func procVars(s *Lexer) (*ast.Node, *errors.CompilerError) {
	Track(s, "Vars")
	_, err := Expect(s, T.VARS)
	if err != nil {
		return nil, err
	}
	return ExpectProd(s, declList, "declaration")
}

// DeclList := Decl {',' Decl} [','].
func declList(s *Lexer) (*ast.Node, *errors.CompilerError) {
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
	return &ast.Node{
		Lex:    T.PROCDECLS,
		Leaves: nodes,
	}, nil
}

// Decl := id [':' type].
func decl(s *Lexer) (*ast.Node, *errors.CompilerError) {
	Track(s, "Decl")
	if s.Word.Lex != T.IDENTIFIER {
		return nil, nil
	}
	id, err := Consume(s)
	if err != nil {
		return nil, err
	}
	if s.Word.Lex == T.COLON {
		colon, err := Consume(s)
		if err != nil {
			return nil, err
		}
		tp, err := ExpectType(s)
		if err != nil {
			return nil, err
		}
		colon.Leaves = []*ast.Node{id, tp}
		return colon, nil
	}
	return id, nil
}

// IdList = ident {"," ident} [","].
func idList(s *Lexer) (*ast.Node, *errors.CompilerError) {
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
	return &ast.Node{
		Lex:    T.IDLIST,
		Leaves: nodes,
	}, nil
}

func ident(s *Lexer) (*ast.Node, *errors.CompilerError) {
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
func code(s *Lexer) (*ast.Node, *errors.CompilerError) {
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
	case T.COPY:
		return _copy(s)
	case T.SET:
		return _set(s)
	default:
		return expr(s)
	}
}

// Set := 'set' Assignees '=' ExprList.
func _set(s *Lexer) (*ast.Node, *errors.CompilerError) {
	kw, err := Expect(s, T.SET)
	if err != nil {
		return nil, err
	}
	ass, err := assignees(s)
	if err != nil {
		return nil, err
	}
	_, err = Expect(s, T.ASSIGNMENT)
	if err != nil {
		return nil, err
	}
	expList, err := ExpectProd(s, exprList, "expression list")
	if err != nil {
		return nil, err
	}
	kw.Leaves = []*ast.Node{ass, expList}
	return kw, nil
}

func assignees(s *Lexer) (*ast.Node, *errors.CompilerError) {
	asses, err := RepeatList(s, assignee, isComma)
	if err != nil {
		return nil, err
	}
	if isComma(s.Word) {
		_, err := Consume(s)
		if err != nil {
			return nil, err
		}
	}
	return &ast.Node{
		Lex: T.ASSIGNEES,
		Leaves: asses,
	}, nil
}

func assignee(s *Lexer) (*ast.Node, *errors.CompilerError) {
	id, err := Expect(s, T.IDENTIFIER)
	if err != nil {
		return nil, err
	}
	if s.Word.Lex == T.LEFTBRACKET {
		n, err := arrayAccess(s)
		if err != nil {
			return nil, err
		}
		n.Leaves = []*ast.Node{id, n.Leaves[0]}
		return n, nil
	}
	return id, nil
}

// Copy := 'copy' TermList 'to' id.
func _copy(s *Lexer) (*ast.Node, *errors.CompilerError) {
	kw, err := Expect(s, T.COPY)
	if err != nil {
		return nil, err
	}
	terms, err := termList(s)
	if err != nil {
		return nil, err
	}
	_, err = Expect(s, T.TO)
	if err != nil {
		return nil, err
	}
	id, err := Expect(s, T.IDENTIFIER)
	if err != nil {
		return nil, err
	}
	kw.Leaves = []*ast.Node{terms, id}
	return kw, nil
}

func termList(s *Lexer) (*ast.Node, *errors.CompilerError) {
	terms, err := RepeatList(s, terminal, isComma)
	if err != nil {
		return nil, err
	}
	if isComma(s.Word) {
		_, err := Consume(s)
		if err != nil {
			return nil, err
		}
	}
	return &ast.Node{
		Lex: T.TERMLIST,
		Leaves: terms,
	}, nil
}

// Return := 'return' [ExprList].
func _return(s *Lexer) (*ast.Node, *errors.CompilerError) {
	Track(s, "return")
	kw, err := Consume(s)
	if err != nil {
		return nil, err
	}
	exp, err := RepeatList(s, expr, isComma)// optional
	if err != nil {
		return nil, err
	}
	if exp != nil {
		kw.Leaves = exp
	}
	return kw, err
}

// Expr = And {"or" And}.
func expr(s *Lexer) (*ast.Node, *errors.CompilerError) {
	Track(s, "Expr")
	return RepeatBinary(s, boolAnd, "expression", orOp)
}

// And = Comp {"and" Comp}.
func boolAnd(s *Lexer) (*ast.Node, *errors.CompilerError) {
	Track(s, "And")
	return RepeatBinary(s, comparative, "expression", andOp)
}

// Comp = Sum {compOp Sum}.
func comparative(s *Lexer) (*ast.Node, *errors.CompilerError) {
	Track(s, "Comp")
	return RepeatBinary(s, additive, "expression", compOp)
}

// Sum = Mult {sumOp Mult}.
func additive(s *Lexer) (*ast.Node, *errors.CompilerError) {
	Track(s, "Sum")
	return RepeatBinary(s, multiplicative, "expression", sumOp)
}

// Mult = Unary {multOp Unary}.
func multiplicative(s *Lexer) (*ast.Node, *errors.CompilerError) {
	Track(s, "Mult")
	return RepeatBinary(s, unarySuffix, "expression", multOp)
}

// Unary = unaryPrefix {Suffix}.
func unarySuffix(s *Lexer) (*ast.Node, *errors.CompilerError) {
	Track(s, "unary suffix")

	pref, err := unaryPrefix(s)
	if err != nil {
		return nil, err
	}
	if pref == nil {
		return nil, nil
	}

	suStart, suEnd, err := RepeatUnaryRight(s, suffix)
	if err != nil {
		return nil, err
	}

	if suStart != nil {
		AddLeaf(suStart, pref)
		pref = suEnd
	}

	return pref, nil
}

// Prefix = "not" | "+" | "-".
func unaryPrefix(s *Lexer) (*ast.Node, *errors.CompilerError) {
	Track(s, "unaryPrefix")
	preFirst, preLast, err := RepeatUnaryLeft(s, prefixOp)
	if err != nil {
		return nil, err
	}
	fact, err := factor(s)
	if err != nil {
		return nil, err
	}
	if preFirst != nil && fact == nil {
		msg := "expected expression after prefix operator"
		err := NewCompilerError(s, et.ExpectedProd, msg)
		return nil, err
	}
	if preFirst != nil {
		AddLeaf(preLast, fact)
		fact = preFirst
	}
	return fact, nil
}

/*
Suffix  := MemAccess
	| Conversion
	| Call.
*/
func suffix(s *Lexer) (*ast.Node, *errors.CompilerError) {
	switch s.Word.Lex {
	case T.LEFTBRACKET:
		return arrayAccess(s)
	case T.LEFTPAREN:
		return call(s)
	case T.COLON:
		return typeConversion(s)
	}
	return nil, nil
}

// ArrayAccess = "[" Expr "]".
func arrayAccess(s *Lexer) (*ast.Node, *errors.CompilerError) {
	n, err := Expect(s, T.LEFTBRACKET)
	if err != nil {
		return nil, err
	}
	exp, err := ExpectProd(s, expr, "expression")
	if err != nil {
		return nil, err
	}
	_, err = Expect(s, T.RIGHTBRACKET)
	if err != nil {
		return nil, err
	}
	n.Leaves = []*ast.Node{exp}
	return n, nil
}

// Call = "(" [ExprList] ")".
func call(s *Lexer) (*ast.Node, *errors.CompilerError) {
	_, err := Expect(s, T.LEFTPAREN)
	if err != nil {
		return nil, err
	}

	explist, err := exprList(s)
	if err != nil {
		return nil, err
	}
	if explist == nil {
		explist = &ast.Node{
			Lex: T.EXPRLIST,
			Leaves: []*ast.Node{},
		}
	}

	_, err = Expect(s, T.RIGHTPAREN)
	if err != nil {
		return nil, err
	}
	return &ast.Node{
		Lex:    T.CALL,
		Leaves: []*ast.Node{explist},
	}, nil
}

func exprList(s *Lexer)(*ast.Node, *errors.CompilerError)  {
	explist, err := RepeatList(s, expr, isComma)
	if err != nil {
		return nil, err
	}
	if explist == nil {
		return nil, nil
	}
	return &ast.Node{
		Lex: T.EXPRLIST,
		Leaves: explist,
	}, nil
}

// TypeConversion = ":" Type.
func typeConversion(s *Lexer) (*ast.Node, *errors.CompilerError) {
	colon, err := Expect(s, T.COLON)
	if err != nil {
		return nil, err
	}
	tp, err := ExpectType(s)
	if err != nil {
		return nil, err
	}
	colon.Leaves = []*ast.Node{tp}
	return colon, nil
}

/*
Factor := Terminal
	| NestedExpr.
*/
func factor(s *Lexer) (*ast.Node, *errors.CompilerError) {
	Track(s, "factor")
	switch s.Word.Lex {
	case T.LEFTPAREN:
		_, err :=Consume(s)
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
	case T.INT, T.STRING, T.CHAR,
		T.TRUE, T.FALSE, T.IDENTIFIER:
		return Consume(s)
	}
	return nil, nil
}

// Block := 'begin' {Code} 'end'.
func block(s *Lexer) (*ast.Node, *errors.CompilerError) {
	Track(s, "block")
	_, err := Expect(s, T.BEGIN)
	if err != nil {
		return nil, err
	}
	n, err := Repeat(s, code)
	if err != nil {
		return nil, err
	}
	_, err = Expect(s, T.END)
	if err != nil {
		return nil, err
	}
	return &ast.Node{
		Leaves: n,
		Lex:    T.BLOCK,
	}, nil
}

func constDef(s *Lexer) (*ast.Node, *errors.CompilerError) {
	Track(s, "const")
	kw, err := Expect(s, T.CONST)
	if err != nil {
		return nil, err
	}
	id, err := Expect(s, T.IDENTIFIER)
	if err != nil {
		return nil, err
	}
	term, err := terminal(s)
	if err != nil {
		return nil, err
	}
	kw.Leaves = []*ast.Node{id, term}
	return kw, nil
}

func terminal(s *Lexer) (*ast.Node, *errors.CompilerError) {
	Track(s, "terminal")
	return Expect(s, T.INT, T.IDENTIFIER, T.STRING, T.CHAR)
}

// If := 'if' Expr Block [Else] 'if'.
func _if(s *Lexer) (*ast.Node, *errors.CompilerError) {
	Track(s, "if")
	keyword, err := Expect(s, T.IF)
	if err != nil {
		return nil, err
	}
	exp, err := ExpectProd(s, expr, "expression")
	if err != nil {
		return nil, err
	}

	block, err := ExpectProd(s, block, "block")
	if err != nil {
		return nil, err
	}

	if s.Word.Lex == T.ELSE || s.Word.Lex == T.ELSEIF {
		else_, err := _else(s)
		if err != nil {
			return nil, err
		}
		keyword.Leaves = []*ast.Node{exp, block, else_}
		return keyword, nil

	}
	keyword.Leaves = []*ast.Node{exp, block}
	_, err = Expect(s, T.IF)
	if err != nil {
		return nil, err
	}
	return keyword, nil
}

/* Else := 'else' Block
         | 'elseif' Expr Block.
*/
func _else(s *Lexer) (*ast.Node, *errors.CompilerError) {
	Track(s, "else")
	switch s.Word.Lex {
	case T.ELSE:
		kw, err := Consume(s)
		if err != nil {
			return nil, err
		}
		bl, err := block(s)
		if err != nil {
			return nil, err
		}
		kw.Leaves = []*ast.Node{bl}
	case T.ELSEIF:
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
		kw.Leaves = []*ast.Node{exp, bl}
	}
	return nil, Check(s, T.ELSE, T.ELSEIF)
}

// While = 'while' Expr Block 'while'.
func _while(s *Lexer) (*ast.Node, *errors.CompilerError) {
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
	keyword.Leaves = []*ast.Node{exp, bl}
	_, err = Expect(s, T.WHILE)
	if err != nil {
		return nil, err
	}
	return keyword, nil
}

func sumOp(n *ast.Node) bool {
	switch n.Lex {
	case T.PLUS, T.MINUS:
		return true
	}
	return false
}

func multOp(n *ast.Node) bool {
	switch n.Lex {
	case T.MULTIPLICATION, T.DIVISION, T.REMAINDER:
		return true
	}
	return false
}

func compOp(n *ast.Node) bool {
	switch n.Lex {
	case T.LESS, T.LESSEQ, T.EQUALS,
		T.MOREEQ, T.MORE, T.DIFFERENT:
		return true
	}
	return false
}

func orOp(n *ast.Node) bool {
	return n.Lex == T.OR
}

func andOp(n *ast.Node) bool {
	return n.Lex == T.AND
}

func prefixOp(st *Lexer) (*ast.Node, *errors.CompilerError) {
	switch st.Word.Lex {
	case T.NOT, T.PLUS, T.MINUS:
		return Consume(st)
	}
	return nil, nil
}

func isComma(n *ast.Node) bool {
	return n.Lex == T.COMMA
}
