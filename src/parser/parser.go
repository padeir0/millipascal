package parser

import (
	"fmt"
	. "mpc/core"
	et "mpc/core/errorkind"
	mod "mpc/core/module"
	T "mpc/core/module/lexkind"
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
	coupl, err := Repeat(s, coupling)
	if err != nil {
		return nil, err
	}
	symb, err := Repeat(s, symbol)
	if err != nil {
		return nil, err
	}
	n := &mod.Node{}
	n.AddLeaf(CreateNode(coupl, T.COUPLINGS))
	n.AddLeaf(CreateNode(symb, T.SYMBOLS))
	if s.Word.Lex != T.EOF {
		return nil, ExpectedEOF(s)
	}
	return n, nil
}

// Coupling := Import | FromImport | Export.
func coupling(s *Lexer) (*mod.Node, *Error) {
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
func _import(s *Lexer) (*mod.Node, *Error) {
	kw, err := Expect(s, T.IMPORT)
	if err != nil {
		return nil, err
	}
	list, err := RepeatCommaList(s, expectIdent)
	if err != nil {
		return nil, err
	}
	kw.SetLeaves(list)
	return kw, nil
}

// FromImport := 'from' id 'import' IdList.
func _fromImport(s *Lexer) (*mod.Node, *Error) {
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
	listNode := &mod.Node{Lex: T.IDLIST}
	listNode.SetLeaves(list)
	kw.SetLeaves([]*mod.Node{id, listNode})
	return kw, nil
}

// Export := 'export' IdList.
func _export(s *Lexer) (*mod.Node, *Error) {
	kw, err := Expect(s, T.EXPORT)
	if err != nil {
		return nil, err
	}
	list, err := RepeatCommaList(s, expectIdent)
	if err != nil {
		return nil, err
	}
	kw.SetLeaves(list)
	return kw, nil
}

// Symbol := Procedure | Memory.
func symbol(s *Lexer) (*mod.Node, *Error) {
	Track(s, "symbol")
	var n *mod.Node
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
func memDef(s *Lexer) (*mod.Node, *Error) {
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
	kw.SetLeaves([]*mod.Node{id, definition})
	return kw, nil
}

// Procedure := 'proc' id [Args [Rets]] [Vars] Block 'proc'.
func procDef(s *Lexer) (*mod.Node, *Error) {
	Track(s, "Procedure")
	kw, err := Expect(s, T.PROC)
	if err != nil {
		return nil, err
	}
	var id, args, rets, vars *mod.Node
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
	kw.SetLeaves([]*mod.Node{id, args, rets, vars, body})
	return kw, nil
}

// Args := '[' [DeclList] ']'.
func procArgs(s *Lexer) (*mod.Node, *Error) {
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
func typeList(s *Lexer) (*mod.Node, *Error) {
	Track(s, "TypeList")
	types, err := RepeatCommaList(s, _type)
	if err != nil {
		return nil, err
	}
	if types == nil {
		return nil, nil
	}
	n := &mod.Node{
		Lex: T.TYPELIST,
	}
	n.SetLeaves(types)
	return n, nil
}

func obligatoryTypeList(s *Lexer) (*mod.Node, *Error) {
	Track(s, "obligatoryTypeList")
	rets, err := RepeatCommaList(s, _obligatoryType)
	if err != nil {
		return nil, err
	}
	if rets == nil {
		return nil, nil
	}
	n := &mod.Node{Lex: T.TYPELIST}
	n.SetLeaves(rets)
	return n, nil
}

// _type := basic | ProcType.
func _type(s *Lexer) (*mod.Node, *Error) {
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
func _obligatoryType(s *Lexer) (*mod.Node, *Error) {
	return ExpectProd(s, _type, "type")
}

// ProcType := 'proc' '[' [TypeList] ']' ProcTypeRet.
// ProcTypeRet := '[' [TypeList] ']' | [Type].
func procType(s *Lexer) (*mod.Node, *Error) {
	Track(s, "procType")
	keyword, err := Expect(s, T.PROC)
	if err != nil {
		return nil, err
	}
	args, err := procTypeTypeList(s)
	if err != nil {
		return nil, err
	}
	var rets *mod.Node
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
			rets = &mod.Node{Lex: T.TYPELIST}
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
func procVars(s *Lexer) (*mod.Node, *Error) {
	Track(s, "Var")
	_, err := Expect(s, T.VAR)
	if err != nil {
		return nil, err
	}
	return ExpectProd(s, declList, "declaration")
}

// DeclList := Decl {',' Decl} [','].
func declList(s *Lexer) (*mod.Node, *Error) {
	Track(s, "DeclList")
	nodes, err := RepeatCommaList(s, decl)
	if err != nil {
		return nil, err
	}
	if len(nodes) == 0 {
		return nil, nil
	}
	n := &mod.Node{Lex: T.PROCDECLS}
	n.SetLeaves(nodes)
	return n, nil
}

// Decl := id [Annot].
func decl(s *Lexer) (*mod.Node, *Error) {
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
		colon.SetLeaves([]*mod.Node{id, colon.Leaves[0]})
		return colon, nil
	}
	return id, nil
}

func ident(s *Lexer) (*mod.Node, *Error) {
	if s.Word.Lex == T.IDENTIFIER {
		return Consume(s)
	}
	return nil, nil
}

func expectIdent(s *Lexer) (*mod.Node, *Error) {
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
func code(s *Lexer) (*mod.Node, *Error) {
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
func _set(s *Lexer) (*mod.Node, *Error) {
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
	kw.SetLeaves([]*mod.Node{ass, op, exp})
	return kw, nil
}

// ExprList := Expr {',' Expr} [','].
func exprList(s *Lexer) (*mod.Node, *Error) {
	asses, err := RepeatCommaList(s, expr)
	if err != nil {
		return nil, err
	}
	n := &mod.Node{Lex: T.EXPRLIST}
	n.SetLeaves(asses)
	return n, nil
}

// Return := 'return' [ExprList].
func _return(s *Lexer) (*mod.Node, *Error) {
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
		kw.SetLeaves(exp)
	}
	return kw, err
}

// Exit := 'exit' Expr.
func _exit(s *Lexer) (*mod.Node, *Error) {
	Track(s, "exit")
	kw, err := Consume(s)
	if err != nil {
		return nil, err
	}
	exp, err := ExpectProd(s, expr, "expression")
	if err != nil {
		return nil, err
	}
	kw.SetLeaves([]*mod.Node{exp})
	return kw, err
}

// Expr = And {"or" And}.
func expr(s *Lexer) (*mod.Node, *Error) {
	Track(s, "Expr")
	return RepeatBinary(s, boolAnd, "expression", orOp)
}

// And = Comp {"and" Comp}.
func boolAnd(s *Lexer) (*mod.Node, *Error) {
	Track(s, "And")
	return RepeatBinary(s, comparative, "expression", andOp)
}

// Comp = Sum {compOp Sum}.
func comparative(s *Lexer) (*mod.Node, *Error) {
	Track(s, "Comp")
	return RepeatBinary(s, additive, "expression", compOp)
}

// Sum = Mult {sumOp Mult}.
func additive(s *Lexer) (*mod.Node, *Error) {
	Track(s, "Sum")
	return RepeatBinary(s, multiplicative, "expression", sumOp)
}

// Mult = Unary {multOp Unary}.
func multiplicative(s *Lexer) (*mod.Node, *Error) {
	Track(s, "Mult")
	return RepeatBinary(s, unaryPrefix, "expression", multOp)
}

// Prefix = "not" | "~".
// UnaryPrefix := {Prefix} UnarySuffix.
func unaryPrefix(s *Lexer) (*mod.Node, *Error) {
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
	suFirst, suLast, err := RepeatUnaryRight(s, suffix)
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
func propertyAccess(s *Lexer) (*mod.Node, *Error) {
	dot, err := Expect(s, T.DOT)
	if err != nil {
		return nil, err
	}
	id, err := Expect(s, T.IDENTIFIER)
	if err != nil {
		return nil, err
	}
	dot.AddLeaf(id)
	return dot, err
}

// Deref := '@' Type.
func deref(s *Lexer) (*mod.Node, *Error) {
	Track(s, "deref")
	at, err := Expect(s, T.AT)
	if err != nil {
		return nil, err
	}
	t, err := ExpectProd(s, _type, "type")
	if err != nil {
		return nil, err
	}
	at.AddLeaf(t)
	return at, nil
}

// Call = "[" [ExprList] "]".
func call(s *Lexer) (*mod.Node, *Error) {
	Track(s, "call")
	lB, err := Expect(s, T.LEFTBRACKET)
	if err != nil {
		return nil, err
	}

	explist, err := exprList(s)
	if err != nil {
		return nil, err
	}
	if explist == nil {
		explist = &mod.Node{
			Lex:    T.EXPRLIST,
			Leaves: []*mod.Node{},
		}
	}

	rB, err := Expect(s, T.RIGHTBRACKET)
	if err != nil {
		return nil, err
	}
	n := &mod.Node{
		Lex: T.CALL,
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
	colon, err := Expect(s, T.COLON)
	if err != nil {
		return nil, err
	}
	tp, err := ExpectProd(s, _type, "type")
	if err != nil {
		return nil, err
	}
	colon.AddLeaf(tp)
	return colon, nil
}

/*
Factor := Name
	| Terminal
	| NestedExpr.
*/
func factor(s *Lexer) (*mod.Node, *Error) {
	Track(s, "factor")
	switch s.Word.Lex {
	case T.IDENTIFIER:
		return name(s)
	case T.LEFTPAREN:
		lP, err := Consume(s)
		if err != nil {
			return nil, err
		}
		n, err := expr(s)
		if err != nil {
			return nil, err
		}
		rP, err := Expect(s, T.RIGHTPAREN)
		if err != nil {
			return nil, err
		}
		n.Range = &Range{
			Begin: lP.Range.Begin,
			End:   rP.Range.End,
		}
		return n, nil
	case T.I64_LIT, T.I32_LIT, T.I16_LIT, T.I8_LIT, T.CHAR_LIT,
		T.TRUE, T.FALSE, T.PTR_LIT:
		return Consume(s)
	}
	return nil, nil
}

func name(s *Lexer) (*mod.Node, *Error) {
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
		dcolon.SetLeaves([]*mod.Node{id, id2})
		return dcolon, nil
	}
	return id, nil
}

// Block := 'begin' {CodeSemicolon} 'end'.
func block(s *Lexer) (*mod.Node, *Error) {
	Track(s, "block")
	begin, err := Expect(s, T.BEGIN)
	if err != nil {
		return nil, err
	}
	n, err := Repeat(s, codeSemicolon)
	if err != nil {
		return nil, err
	}
	end, err := Expect(s, T.END)
	if err != nil {
		return nil, err
	}
	bl := &mod.Node{
		Lex: T.BLOCK,
		Range: &Range{
			Begin: begin.Range.Begin,
			End:   end.Range.End,
		},
	}
	bl.SetLeaves(n)
	return bl, nil
}

// CodeSemicolon := Code [';'].
func codeSemicolon(s *Lexer) (*mod.Node, *Error) {
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
func _if(s *Lexer) (*mod.Node, *Error) {
	Track(s, "if")
	keyword, err := Expect(s, T.IF)
	var exp, bl, else_, elseif_ *mod.Node

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
	keyword.SetLeaves([]*mod.Node{exp, bl, elseif_, else_})
	_, err = Expect(s, T.IF)
	if err != nil {
		return nil, err
	}
	return keyword, nil
}

// Else := 'else' Block.
func _else(s *Lexer) (*mod.Node, *Error) {
	Track(s, "else")
	kw, err := Consume(s)
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
	elses, err := Repeat(s, _elseif)
	if err != nil {
		return nil, err
	}
	n := &mod.Node{
		Lex: T.ELSEIFCHAIN,
	}
	n.SetLeaves(elses)
	return n, nil
}

func _elseif(s *Lexer) (*mod.Node, *Error) {
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
	kw.SetLeaves([]*mod.Node{exp, bl})
	return kw, nil
}

// While = 'while' Expr Block 'while'.
func _while(s *Lexer) (*mod.Node, *Error) {
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
	keyword.SetLeaves([]*mod.Node{exp, bl})
	_, err = Expect(s, T.WHILE)
	if err != nil {
		return nil, err
	}
	return keyword, nil
}

func numberOrString(s *Lexer) (*mod.Node, *Error) {
	return Expect(s, T.I64_LIT, T.I32_LIT, T.I16_LIT, T.I8_LIT, T.PTR_LIT, T.STRING_LIT)
}

func sumOp(n *mod.Node) bool {
	switch n.Lex {
	case T.PLUS, T.MINUS:
		return true
	}
	return false
}

func multOp(n *mod.Node) bool {
	switch n.Lex {
	case T.MULTIPLICATION, T.DIVISION, T.REMAINDER:
		return true
	}
	return false
}

func compOp(n *mod.Node) bool {
	switch n.Lex {
	case T.LESS, T.LESSEQ, T.EQUALS,
		T.MOREEQ, T.MORE, T.DIFFERENT:
		return true
	}
	return false
}

func orOp(n *mod.Node) bool {
	return n.Lex == T.OR
}

func andOp(n *mod.Node) bool {
	return n.Lex == T.AND
}

func prefixOp(st *Lexer) (*mod.Node, *Error) {
	switch st.Word.Lex {
	case T.NOT, T.NEG:
		return Consume(st)
	}
	return nil, nil
}

func isComma(n *mod.Node) bool {
	return n.Lex == T.COMMA
}

func Consume(st *Lexer) (*mod.Node, *Error) {
	n := st.Word
	err := st.Next()
	return n, err
}

func Check(st *Lexer, tpList ...T.LexKind) *Error {
	for _, tp := range tpList {
		if st.Word.Lex == tp {
			return nil
		}
	}
	message := fmt.Sprintf("Expected one of %v: instead found '%v'",
		T.FmtToUser(tpList...),
		T.FmtToUser(st.Word.Lex))

	err := NewCompilerError(st, et.ExpectedSymbol, message)
	return err
}

func Expect(st *Lexer, tpList ...T.LexKind) (*mod.Node, *Error) {
	for _, tp := range tpList {
		if st.Word.Lex == tp {
			return Consume(st)
		}
	}
	message := fmt.Sprintf("Expected one of %v: instead found %v",
		T.FmtToUser(tpList...),
		T.FmtToUser(st.Word.Lex))

	err := NewCompilerError(st, et.ExpectedSymbol, message)
	return nil, err
}

func ExpectProd(st *Lexer, prod Production, name string) (*mod.Node, *Error) {
	n, err := prod(st)
	if err != nil {
		return nil, err
	}
	if n == nil {
		message := fmt.Sprintf("expected %v instead found %v", name, T.FmtToUser(st.Word.Lex))
		err := NewCompilerError(st, et.ExpectedProd, message)
		return nil, err
	}
	return n, err
}

type Production func(st *Lexer) (*mod.Node, *Error)
type Validator func(*mod.Node) bool

/* RepeatBinary implements the following pattern
for a given Production and Terminal:

	RepeatBinary := Production {Terminal Production}

Validator checks for terminals.
Left to Right precedence
*/
func RepeatBinary(st *Lexer, prod Production, name string, v Validator) (*mod.Node, *Error) {
	last, err := prod(st)
	if err != nil {
		return nil, err
	}
	if last == nil {
		return nil, nil
	}
	for v(st.Word) {
		parent, err := Consume(st)
		if err != nil {
			return nil, err
		}
		parent.AddLeaf(last)

		newLeaf, err := ExpectProd(st, prod, name)
		if err != nil {
			return nil, err
		}
		parent.AddLeaf(newLeaf)

		last = parent
	}
	return last, nil
}

/* Repeat implements the following pattern
for a given Production:

	Repeat := {Production}.
*/
func Repeat(st *Lexer, prod Production) ([]*mod.Node, *Error) {
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

/*RepeatUnaryLeft implements the following pattern
for a given Production:

	RepeatUnaryLeft := {Production}.

But returns the first and last item in the tree.

It's Left associative: first<-second<-last
*/
func RepeatUnaryLeft(st *Lexer, prod Production) (*mod.Node, *mod.Node, *Error) {
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

func RepeatUnaryRight(st *Lexer, prod Production) (*mod.Node, *mod.Node, *Error) {
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

/* RepeatList implements the following pattern
for a given Production and Terminal:

	RepeatBinary := Production {Terminal Production}

Validator checks for terminals.

It differs from RepeatBinary in that it returns a slice
instead of a Tree with precedence
*/
func RepeatList(st *Lexer, prod Production, val Validator) ([]*mod.Node, *Error) {
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
func RepeatCommaList(st *Lexer, prod Production) ([]*mod.Node, *Error) {
	first, err := prod(st)
	if err != nil {
		return nil, err
	}
	if first == nil {
		return nil, nil
	}
	out := []*mod.Node{first}
	for st.Word.Lex == T.COMMA {
		st.Next()
		n, err := prod(st)
		if err != nil {
			return nil, err
		}
		if n != nil {
			out = append(out, n)
		}
	}
	if st.Word.Lex == T.COMMA {
		err := st.Next()
		if err != nil {
			return nil, err
		}
	}
	return out, nil
}

func CreateNode(nodes []*mod.Node, t T.LexKind) *mod.Node {
	n := &mod.Node{Lex: t}
	n.SetLeaves(nodes)
	return n
}

func ExpectedEOF(s *Lexer) *Error {
	return NewCompilerError(s, et.ExpectedEOF, "unexpected symbol, expected EOF")
}

func NewCompilerError(st *Lexer, t et.ErrorKind, message string) *Error {
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
