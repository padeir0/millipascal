package gen

import (
	hirc "mpc/frontend/enums/HIRClass"
	T "mpc/frontend/enums/Type"
	IT "mpc/frontend/enums/instrType"
	lex "mpc/frontend/enums/lexType"
	ST "mpc/frontend/enums/symbolType"
	"mpc/frontend/ir"
	RIU "mpc/frontend/util/ir"
	"strconv"
)

type context struct {
	Proc *ir.Proc

	CurrBlock *ir.BasicBlock

	LabelCounter int
	TempCounter  int
}

func newContext(proc *ir.Proc) *context {
	return &context{
		Proc:         proc,
		LabelCounter: 0,
		TempCounter:  0,
	}
}

func (c *context) NewBlock() *ir.BasicBlock {
	counter := strconv.Itoa(c.LabelCounter)
	b := &ir.BasicBlock{
		Label: ".L" + counter,
		Code:  []*ir.Instr{},
	}
	c.LabelCounter++
	return b
}

func (c *context) AllocTemp(t T.Type) *ir.Operand {
	op := &ir.Operand{
		Hirc: hirc.Temp,
		Type: t,
		Num:  c.TempCounter,
	}
	c.TempCounter++
	return op
}

func Generate(M *ir.Module) {
	for _, sy := range M.Globals {
		switch sy.T {
		case ST.Proc:
			genProc(M, sy.Proc)
		}
	}
}

func genProc(M *ir.Module, proc *ir.Proc) {
	c := newContext(proc)
	start := c.NewBlock()
	proc.Code = start
	c.CurrBlock = start

	body := proc.N.Leaves[4]
	genBlock(M, c, body)
	if !proc.Code.HasFlow() {
		proc.Code.Return([]*ir.Operand{})
	}
	return
}

func genBlock(M *ir.Module, c *context, body *ir.Node) {
	for _, code := range body.Leaves {
		switch code.Lex {
		case lex.IF:
			genIf(M, c, code)
		case lex.WHILE:
			genWhile(M, c, code)
		case lex.RETURN:
			genReturn(M, c, code)
		case lex.SET:
			genSet(M, c, code)
		default:
			genExpr(M, c, code)
		}
	}
}

func genIf(M *ir.Module, c *context, if_ *ir.Node) {
	exp := if_.Leaves[0]
	block := if_.Leaves[1]
	elseifchain := if_.Leaves[2]
	else_ := if_.Leaves[3]

	op := genExpr(M, c, exp)
	truebl := c.NewBlock()
	falsebl := c.NewBlock()
	outbl := c.NewBlock()
	c.CurrBlock.Branch(op, truebl, falsebl)

	c.CurrBlock = truebl
	genBlock(M, c, block)
	c.CurrBlock.Jmp(outbl)

	c.CurrBlock = falsebl
	if elseifchain != nil {
		genElseIfChain(M, c, elseifchain, outbl)
	}
	if else_ != nil {
		genBlock(M, c, else_.Leaves[0])
	}
	c.CurrBlock.Jmp(outbl)
	c.CurrBlock = outbl
}

func genElseIfChain(M *ir.Module, c *context, elseifchain *ir.Node, outbl *ir.BasicBlock) {
	for _, elseif := range elseifchain.Leaves {
		exp := elseif.Leaves[0]
		block := elseif.Leaves[1]

		op := genExpr(M, c, exp)
		truebl := c.NewBlock()
		falsebl := c.NewBlock()
		c.CurrBlock.Branch(op, truebl, falsebl)

		c.CurrBlock = truebl
		genBlock(M, c, block)
		c.CurrBlock.Jmp(outbl)
		c.CurrBlock = falsebl
	}
}

func genWhile(M *ir.Module, c *context, while *ir.Node) {
	loop_start := c.NewBlock()
	loop_body := c.NewBlock()
	loop_end := c.NewBlock()

	c.CurrBlock.Jmp(loop_start)
	c.CurrBlock = loop_start

	op := genExpr(M, c, while.Leaves[0])
	c.CurrBlock.Branch(op, loop_body, loop_end)

	c.CurrBlock = loop_body
	genBlock(M, c, while.Leaves[1])
	c.CurrBlock.Jmp(loop_start)

	c.CurrBlock = loop_end

}

func genReturn(M *ir.Module, c *context, return_ *ir.Node) {
	operands := []*ir.Operand{}
	for _, ret := range return_.Leaves {
		op := genExpr(M, c, ret)
		operands = append(operands, op)
	}
	c.CurrBlock.Return(operands)
}

func genSet(M *ir.Module, c *context, set *ir.Node) {
	assignees := set.Leaves[0]
	expr := set.Leaves[1]

	if len(assignees.Leaves) > 1 && len(expr.Leaves) == 1 {
		genMultiProcAssign(M, c, assignees, expr.Leaves[0])
		return
	}

	genSingleAssign(M, c, assignees.Leaves[0], expr)
	return
}

func genMultiProcAssign(M *ir.Module, c *context, assignees, call *ir.Node) {
	if call.Lex != lex.CALL {
		panic("must be CALL:\n" + ir.FmtNode(call))
	}
	// TODO: OPT: pass assignees to genCall so that no copying needs to happen
	rets := genCall(M, c, call)

	genLoadAssignRets(M, c, assignees, rets)
}

func genIntOp(num int) *ir.Operand {
	return &ir.Operand{
		Hirc: hirc.Lit,
		Num:  num,
	}
}

func genLoadAssignRets(M *ir.Module, c *context, assignees *ir.Node, ops []*ir.Operand) {
	for i, ass := range assignees.Leaves {
		op := ops[i]
		if ass.Lex == lex.IDENTIFIER {
			genCallAssign(M, c, ass, op)
			continue
		}
		if ass.Lex == lex.LEFTBRACKET {
			genCallAssignMem(M, c, ass, op)
			continue
		}
	}
}

func genCallInstr(c *context, proc *ir.Operand, args, rets []*ir.Operand) {
	operands := []*ir.Operand{proc}
	operands = append(operands, args...)
	iCall := &ir.Instr{
		T:           IT.Call,
		Operands:    operands,
		Destination: rets,
	}
	c.CurrBlock.AddInstr(iCall)
}

func genArgs(M *ir.Module, c *context, args *ir.Node) []*ir.Operand {
	output := []*ir.Operand{}
	for _, arg := range args.Leaves {
		res := genExpr(M, c, arg)
		output = append(output, res)
	}
	return output
}

func genRets(M *ir.Module, c *context, proc *ir.Proc) []*ir.Operand {
	output := []*ir.Operand{}
	for _, ret := range proc.Rets {
		op := c.AllocTemp(ret)
		output = append(output, op)
	}
	return output
}

func genCallAssign(M *ir.Module, c *context, ass *ir.Node, op *ir.Operand) {
	dest := genExprID(M, c, ass)
	// TODO: OPT: try to avoid this COPY instruction
	loadRet := RIU.Copy(op, dest)
	c.CurrBlock.AddInstr(loadRet)
}

// TODO: BUG: fix this
func genCallAssignMem(M *ir.Module, c *context, ass *ir.Node, op *ir.Operand) {
	ptrOp := genExpr(M, c, ass.Leaves[1]) // CHECK
	loadPtr := RIU.StorePtr(op, ptrOp)
	c.CurrBlock.AddInstr(loadPtr)
}

func genMultiAssign(M *ir.Module, c *context, assignees, exprlist *ir.Node) {
	for i := range assignees.Leaves {
		ass := assignees.Leaves[i]
		exp := exprlist.Leaves[i]
		genSingleAssign(M, c, ass, exp)
	}
}

func genSingleAssign(M *ir.Module, c *context, assignee, expr *ir.Node) {
	if assignee.Lex == lex.IDENTIFIER {
		genNormalAssign(M, c, assignee, expr)
		return
	}
	genDerefAssign(M, c, assignee, expr)
	return
}

func genNormalAssign(M *ir.Module, c *context, assignee, expr *ir.Node) {
	op := genExprID(M, c, assignee)
	exp := genExpr(M, c, expr)
	// TODO: OPT: Try to avoid this COPY instruction
	store := RIU.Copy(exp, op)
	c.CurrBlock.AddInstr(store)
}

func genDerefAssign(M *ir.Module, c *context, left, right *ir.Node) {
	leftExpr := left.Leaves[1]
	leftOp := genExpr(M, c, leftExpr)
	rightOp := genExpr(M, c, right)

	store := RIU.StorePtr(rightOp, leftOp)
	c.CurrBlock.AddInstr(store)
}

func genExpr(M *ir.Module, c *context, exp *ir.Node) *ir.Operand {
	switch exp.Lex {
	case lex.IDENTIFIER:
		return genExprID(M, c, exp)
	case lex.FALSE, lex.TRUE:
		return genBoolLit(M, c, exp)
	case lex.INT_LIT:
		return genIntLit(M, c, exp)
	case lex.PTR_LIT:
		return genPtrLit(M, c, exp)
	case lex.PLUS, lex.MINUS:
		return genPlusMinus(M, c, exp)
	case lex.MULTIPLICATION, lex.DIVISION, lex.REMAINDER,
		lex.EQUALS, lex.DIFFERENT,
		lex.MORE, lex.MOREEQ, lex.LESS, lex.LESSEQ,
		lex.AND, lex.OR:
		return genBinaryOp(M, c, exp)
	case lex.COLON:
		return genConversion(M, c, exp)
	case lex.CALL:
		return genCall(M, c, exp)[0]
	case lex.AT:
		return genDeref(M, c, exp)
	case lex.NOT:
		return genUnaryOp(M, c, exp)
	}
	return nil
}

// assume a single return
func genCall(M *ir.Module, c *context, call *ir.Node) []*ir.Operand {
	proc := call.Leaves[1]
	args := call.Leaves[0]

	procOp := genExprID(M, c, proc)

	argOps := genArgs(M, c, args)
	retOps := genRets(M, c, procOp.Symbol.Proc)
	genCallInstr(c, procOp, argOps, retOps)

	return retOps
}

func genDeref(M *ir.Module, c *context, memAccess *ir.Node) *ir.Operand {
	t := memAccess.Leaves[0].T
	exp := memAccess.Leaves[1]

	ptrOp := genExpr(M, c, exp)

	dest := c.AllocTemp(t)
	loadPtr := RIU.LoadPtr(ptrOp, dest)
	c.CurrBlock.AddInstr(loadPtr)

	return dest
}

func genExprID(M *ir.Module, c *context, id *ir.Node) *ir.Operand {
	decl, ok := c.Proc.Vars[id.Text]
	if ok {
		return &ir.Operand{
			Hirc:   hirc.Local,
			Type:   id.T,
			Symbol: decl,
		}
	}
	posSy, ok := c.Proc.ArgMap[id.Text]
	if ok {
		return &ir.Operand{
			Hirc:   hirc.Arg,
			Type:   id.T,
			Symbol: posSy.Symbol,
			Num:    posSy.Position,
		}
	}
	global, ok := M.Globals[id.Text]
	if ok {
		return globalToOperand(id, global)
	}
	panic("genExprID: global not found")
}

func globalToOperand(id *ir.Node, global *ir.Symbol) *ir.Operand {
	switch global.T {
	case ST.Proc:
		return &ir.Operand{
			Hirc:   hirc.Global,
			Symbol: global,
			Type:   T.Proc,
		}
	case ST.Mem:
		return &ir.Operand{
			Hirc:   hirc.Global,
			Symbol: global,
			Type:   T.Ptr,
		}
	}
	panic("wht jus heppn?")
}

func genConversion(M *ir.Module, c *context, colon *ir.Node) *ir.Operand {
	a := genExpr(M, c, colon.Leaves[1])
	dest := c.AllocTemp(colon.T)
	instr := RIU.Convert(a, dest)
	c.CurrBlock.AddInstr(instr)
	return dest
}

func genIntLit(M *ir.Module, c *context, lit *ir.Node) *ir.Operand {
	value, err := strconv.Atoi(lit.Text)
	if err != nil {
		panic(err)
	}
	return &ir.Operand{
		Hirc: hirc.Lit,
		Type: lit.T,
		Num:  value,
	}
}

func genPtrLit(M *ir.Module, c *context, lit *ir.Node) *ir.Operand {
	numPart := lit.Text[0:len(lit.Text)-1]
	value, err := strconv.Atoi(numPart)
	if err != nil {
		panic(err)
	}
	return &ir.Operand{
		Hirc: hirc.Lit,
		Type: lit.T,
		Num:  value,
	}
}

func genBoolLit(M *ir.Module, c *context, lit *ir.Node) *ir.Operand {
	value := 0
	if lit.Lex == lex.TRUE {
		value = 1
	}
	return &ir.Operand{
		Hirc: hirc.Lit,
		Type: lit.T,
		Num:  value,
	}
}

func genPlusMinus(M *ir.Module, c *context, op *ir.Node) *ir.Operand {
	if len(op.Leaves) == 2 {
		return genBinaryOp(M, c, op)
	}
	return genUnaryOp(M, c, op)
}

func genBinaryOp(M *ir.Module, c *context, op *ir.Node) *ir.Operand {
	it := lexToBinaryOp(op.Lex)
	a := genExpr(M, c, op.Leaves[0])
	b := genExpr(M, c, op.Leaves[1])
	dest := c.AllocTemp(op.T)
	instr := &ir.Instr{
		T:           it,
		Type:        a.Type,
		Operands:    []*ir.Operand{a, b},
		Destination: []*ir.Operand{dest},
	}
	c.CurrBlock.AddInstr(instr)
	return dest
}

func lexToBinaryOp(op lex.TkType) IT.InstrType {
	switch op {
	case lex.MINUS:
		return IT.Sub
	case lex.PLUS:
		return IT.Add
	case lex.MULTIPLICATION:
		return IT.Mult
	case lex.DIVISION:
		return IT.Div
	case lex.REMAINDER:
		return IT.Rem
	case lex.EQUALS:
		return IT.Eq
	case lex.DIFFERENT:
		return IT.Diff
	case lex.MORE:
		return IT.More
	case lex.MOREEQ:
		return IT.MoreEq
	case lex.LESS:
		return IT.Less
	case lex.LESSEQ:
		return IT.LessEq
	case lex.AND:
		return IT.And
	case lex.OR:
		return IT.Or
	}
	panic("lexToBinaryOp: unexpected binOp: " + lex.FmtTypes(op))
}

func genUnaryOp(M *ir.Module, c *context, op *ir.Node) *ir.Operand {
	it := lexToUnaryOp(op.Lex)
	a := genExpr(M, c, op.Leaves[0])
	dest := c.AllocTemp(op.T)
	instr := &ir.Instr{
		T:           it,
		Type:        op.T,
		Operands:    []*ir.Operand{a},
		Destination: []*ir.Operand{dest},
	}
	c.CurrBlock.AddInstr(instr)
	return dest
}

func lexToUnaryOp(op lex.TkType) IT.InstrType {
	switch op {
	case lex.MINUS:
		return IT.UnaryMinus
	case lex.PLUS:
		return IT.UnaryPlus
	case lex.NOT:
		return IT.Not
	}
	panic("lexToUnaryOp: unexpected binOp")
}
