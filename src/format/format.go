// generates formatted code from AST
package format

import (
	mod "mpc/core/module"
	T "mpc/core/module/lexkind"
)

func Format(n *mod.Node) string {
	ctx := _context()
	module(ctx, n)
	return ctx.String()
}

func _context() *context {
	return &context{
		depth:   0,
		columns: 0,
		head:    nil,
		curr:    nil,
	}
}

type llist struct {
	s    []byte
	next *llist
}

type context struct {
	depth   int // counts scope depth
	columns int // chars per line

	head *llist
	curr *llist
}

func (this *context) indent() {
	d := this.depth
	for d > 0 {
		this.Place([]byte("\t"))
		d--
	}
}

// this allows us to break the line in commas
func (this *context) Comma() {
	this.Place([]byte(", "))
	if this.columns >= 75 {
		this.Newline()
	}
}

func (this *context) Newline() {
	this.columns = 0
	this.Place([]byte("\n"))
	this.indent()
}

func (this *context) Place(s []byte) {
	new := &llist{
		s:    s,
		next: nil,
	}
	this.columns += len(s)
	if this.curr != nil {
		this.curr.next = new
	}
	this.curr = new
	if this.head == nil {
		this.head = new
	}
}

func (this *context) String() string {
	size := this.getSize()
	buff := make([]byte, size)
	index := 0
	curr := this.head
	for curr != nil {
		copy(buff[index:], curr.s)
		index += len(curr.s)
		curr = curr.next
	}
	return string(buff)
}

func (this *context) getSize() int {
	output := 0
	curr := this.head
	for curr != nil {
		output += len(curr.s)
		curr = curr.next
	}
	return output
}

type printer func(*context, *mod.Node)

func commalist(ctx *context, leaves []*mod.Node, p printer) {
	for i, leaf := range leaves {
		p(ctx, leaf)
		if i < len(leaves)-1 {
			ctx.Comma()
		}
	}
}

func module(ctx *context, n *mod.Node) {
	_coupling(ctx, n.Leaves[0])
	ctx.Newline()
	_symbols(ctx, n.Leaves[1])
}

func _coupling(ctx *context, n *mod.Node) {
	for i, leaf := range n.Leaves {
		switch leaf.Lex {
		case T.IMPORT:
			ctx.Place([]byte("import "))
			commalist(ctx, leaf.Leaves, _id)
		case T.FROM:
			_from(ctx, leaf)
		case T.EXPORT:
			ctx.Place([]byte("export "))
			commalist(ctx, leaf.Leaves, _id)
		}
		if i < len(n.Leaves)-1 {
			ctx.Newline()
		}
		ctx.Newline()
	}
}

func _from(ctx *context, n *mod.Node) {
	ctx.Place([]byte("from "))
	id := n.Leaves[0]
	ctx.Place([]byte(id.Text))
	ctx.Newline()
	ctx.Place([]byte("import "))
	idlist := n.Leaves[1]
	commalist(ctx, idlist.Leaves, _id)
}

func _impexp(ctx *context, leaves []*mod.Node, kw []byte) {
}

func _id(ctx *context, n *mod.Node) {
	ctx.Place([]byte(n.Text))
}

func _onlyConst(ctx *context, n *mod.Node) {
	for _, leaf := range n.Leaves {
		if leaf.Lex == T.CONST {
			_const(ctx, leaf)
			ctx.Newline()
		}
	}
	ctx.Newline()
}

func _onlyMem(ctx *context, n *mod.Node) {
	for _, leaf := range n.Leaves {
		if leaf.Lex == T.DATA {
			_mem(ctx, leaf)
			ctx.Newline()
		}
	}
	ctx.Newline()
}

func _onlyProcs(ctx *context, n *mod.Node) {
	for i, leaf := range n.Leaves {
		if leaf.Lex == T.PROC {
			_proc(ctx, leaf)

			if i < len(n.Leaves)-1 {
				ctx.Newline()
			}
			ctx.Newline()
		}
	}
}

func _symbols(ctx *context, n *mod.Node) {
	_onlyConst(ctx, n)
	_onlyMem(ctx, n)
	_onlyProcs(ctx, n)
}

func _mem(ctx *context, n *mod.Node) {
	ctx.Place([]byte("data "))
	id := n.Leaves[0]
	ctx.Place([]byte(id.Text))
	def := n.Leaves[1]
	ctx.Place([]byte(" "))
	ctx.Place([]byte(def.Text))
}

func _const(ctx *context, n *mod.Node) {
	ctx.Place([]byte("const "))
	id := n.Leaves[0]
	ctx.Place([]byte(id.Text))
	def := n.Leaves[1]
	ctx.Place([]byte(" "))
	ctx.Place([]byte(def.Text))
}

func _proc(ctx *context, n *mod.Node) {
	ctx.Place([]byte("proc "))
	id := n.Leaves[0]
	ctx.Place([]byte(id.Text))
	_args(ctx, n.Leaves[1])
	ctx.Place([]byte(" "))
	_rets(ctx, n.Leaves[2])
	ctx.Newline()
	_vars(ctx, n.Leaves[3])
	_block(ctx, n.Leaves[4])
	ctx.Place([]byte(" proc"))
}

func _vars(ctx *context, n *mod.Node) {
	if n != nil {
		ctx.Place([]byte("var "))
		commalist(ctx, n.Leaves, _decl)
		ctx.Newline()
	}
}

func _rets(ctx *context, n *mod.Node) {
	if n != nil {
		commalist(ctx, n.Leaves, _type)
	}
}

func _args(ctx *context, n *mod.Node) {
	ctx.Place([]byte("["))
	if n != nil {
		commalist(ctx, n.Leaves, _decl)
	}
	ctx.Place([]byte("]"))
}

func _decl(ctx *context, n *mod.Node) {
	if n.Lex == T.COLON {
		id := n.Leaves[0]
		t := n.Leaves[1]
		ctx.Place([]byte(id.Text))
		ctx.Place([]byte(":"))
		_type(ctx, t)
		return
	}
	ctx.Place([]byte(n.Text))
}

func _type(ctx *context, n *mod.Node) {
	if n.Lex == T.PROC {
		_procType(ctx, n)
		return
	}
	ctx.Place([]byte(n.Text))
}

func _procType(ctx *context, n *mod.Node) {
	ctx.Place([]byte("proc "))
	args := n.Leaves[0]
	_procTypeTypeList(ctx, args)

	rets := n.Leaves[1]
	if rets != nil {
		if len(rets.Leaves) == 1 {
			_type(ctx, rets.Leaves[0])
		} else {
			_procTypeTypeList(ctx, rets)
		}
	}
}

func _procTypeTypeList(ctx *context, n *mod.Node) {
	ctx.Place([]byte("["))
	if n != nil {
		commalist(ctx, n.Leaves, _type)
	}
	ctx.Place([]byte("]"))
}

func _block(ctx *context, n *mod.Node) {
	ctx.Place([]byte("begin"))
	ctx.depth++
	ctx.Newline()

	for i, leaf := range n.Leaves {
		_code(ctx, leaf)
		if i < len(n.Leaves)-1 {
			ctx.Newline()
		}
	}

	ctx.depth--
	ctx.Newline()
	ctx.Place([]byte("end"))
}

func _code(ctx *context, n *mod.Node) {
	switch n.Lex {
	case T.IF:
		_if(ctx, n)
	case T.WHILE:
		_while(ctx, n)
	case T.RETURN:
		_return(ctx, n)
	case T.SET:
		_set(ctx, n)
	case T.EXIT:
		_exit(ctx, n)
	default:
		_expr(ctx, n)
	}
}

func _return(ctx *context, n *mod.Node) {
	ctx.Place([]byte("return"))
	if n.Leaves != nil && len(n.Leaves) > 0 {
		ctx.Place([]byte(" "))
		commalist(ctx, n.Leaves, _expr)
	}
	ctx.Place([]byte(";"))
}

func _exit(ctx *context, n *mod.Node) {
	ctx.Place([]byte("exit"))
	if n.Leaves != nil && len(n.Leaves) > 0 {
		ctx.Place([]byte(" "))
		_expr(ctx, n.Leaves[0])
	}
	ctx.Place([]byte(";"))
}

func _while(ctx *context, n *mod.Node) {
	ctx.Place([]byte("while "))
	_expr(ctx, n.Leaves[0])
	ctx.Place([]byte(" "))
	_block(ctx, n.Leaves[1])
	ctx.Place([]byte(" while"))
}

func _if(ctx *context, n *mod.Node) {
	ctx.Place([]byte("if "))
	_expr(ctx, n.Leaves[0])
	ctx.Place([]byte(" "))
	_block(ctx, n.Leaves[1])
	_elseifchain(ctx, n.Leaves[2])
	_else(ctx, n.Leaves[3])
	ctx.Place([]byte(" if"))
}

func _elseifchain(ctx *context, n *mod.Node) {
	if n == nil {
		return
	}
	for _, leaf := range n.Leaves {
		_elseif(ctx, leaf)
	}
}

func _elseif(ctx *context, n *mod.Node) {
	ctx.Place([]byte(" elseif "))
	_expr(ctx, n.Leaves[0])
	ctx.Place([]byte(" "))
	_block(ctx, n.Leaves[1])
}

func _else(ctx *context, n *mod.Node) {
	if n == nil {
		return
	}
	ctx.Place([]byte(" else "))
	_block(ctx, n.Leaves[0])
}

func _set(ctx *context, n *mod.Node) {
	ctx.Place([]byte("set "))
	assignees := n.Leaves[0]
	commalist(ctx, assignees.Leaves, _expr)
	op := n.Leaves[1]
	ctx.Place([]byte(" " + op.Text + " "))
	_expr(ctx, n.Leaves[2])
}

func _expr(ctx *context, n *mod.Node) {
	smallNumber := -1 * (1 << 10)
	_exprPrec(ctx, n, smallNumber)
}

func _exprPrec(ctx *context, n *mod.Node, prevPrecedence int) {
	switch n.Lex {
	case T.IDENTIFIER:
		_id(ctx, n)
	case T.SIZEOF:
		ctx.Place([]byte("sizeof "))
		_type(ctx, n.Leaves[0])
	case T.DOUBLECOLON:
		_id(ctx, n.Leaves[0])
		ctx.Place([]byte("::"))
		_id(ctx, n.Leaves[1])
	case T.I64_LIT, T.I32_LIT, T.I16_LIT, T.I8_LIT,
		T.U64_LIT, T.U32_LIT, T.U16_LIT, T.U8_LIT,
		T.FALSE, T.TRUE, T.PTR_LIT, T.STRING_LIT,
		T.CHAR_LIT:
		ctx.Place([]byte(n.Text))
	case T.NEG, T.BITWISENOT, T.NOT:
		if precedence(n.Lex) < prevPrecedence {
			ctx.Place([]byte("("))
			unary(ctx, n)
			ctx.Place([]byte(")"))
		} else {
			unary(ctx, n)
		}
	case T.MULTIPLICATION, T.DIVISION, T.REMAINDER,
		T.BITWISEAND, T.SHIFTLEFT, T.SHIFTRIGHT,
		T.PLUS, T.MINUS, T.BITWISEOR, T.BITWISEXOR,
		T.EQUALS, T.DIFFERENT,
		T.MORE, T.MOREEQ, T.LESS, T.LESSEQ,
		T.AND, T.OR:
		if precedence(n.Lex) < prevPrecedence {
			ctx.Place([]byte("("))
			binary(ctx, n)
			ctx.Place([]byte(")"))
		} else {
			binary(ctx, n)
		}
	// these have the highest precedence
	case T.COLON:
		_exprPrec(ctx, n.Leaves[1], precedence(T.COLON))
		ctx.Place([]byte(":"))
		_type(ctx, n.Leaves[0])
	case T.AT:
		_exprPrec(ctx, n.Leaves[1], precedence(T.AT))
		ctx.Place([]byte("@"))
		_type(ctx, n.Leaves[0])
	case T.DOT:
		_exprPrec(ctx, n.Leaves[1], precedence(T.DOT))
		ctx.Place([]byte("."))
		_id(ctx, n.Leaves[0])
	case T.ARROW:
		_exprPrec(ctx, n.Leaves[1], precedence(T.ARROW))
		ctx.Place([]byte("->"))
		_id(ctx, n.Leaves[0])
	case T.CALL:
		_exprPrec(ctx, n.Leaves[1], precedence(T.CALL))
		ctx.Place([]byte("["))
		exprs := n.Leaves[0]
		commalist(ctx, exprs.Leaves, _expr)
		ctx.Place([]byte("]"))
	default:
		panic("oh no!")
	}
}

func binary(ctx *context, n *mod.Node) {
	_exprPrec(ctx, n.Leaves[0], precedence(n.Lex))
	ctx.Place([]byte(" " + T.Tktosrc[n.Lex] + " "))
	_exprPrec(ctx, n.Leaves[1], precedence(n.Lex))
}

func unary(ctx *context, n *mod.Node) {
	ctx.Place([]byte(T.Tktosrc[n.Lex] + " "))
	_exprPrec(ctx, n.Leaves[0], precedence(n.Lex))
}

func precedence(lex T.LexKind) int {
	switch lex {
	case T.COLON, T.CALL, T.AT, T.DOT, T.ARROW:
		return 7
	case T.NEG, T.BITWISENOT, T.NOT:
		return 6
	case T.MULTIPLICATION, T.DIVISION, T.REMAINDER,
		T.BITWISEAND, T.SHIFTLEFT, T.SHIFTRIGHT:
		return 5
	case T.PLUS, T.MINUS, T.BITWISEOR, T.BITWISEXOR:
		return 4
	case T.EQUALS, T.DIFFERENT,
		T.MORE, T.MOREEQ, T.LESS, T.LESSEQ:
		return 3
	case T.AND:
		return 2
	case T.OR:
		return 1
	}
	panic("oh no!!")
}
