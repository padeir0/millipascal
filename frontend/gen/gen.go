package gen

import (
	"mpc/frontend/ir"
	T "mpc/frontend/enums/Type"
	IT "mpc/frontend/enums/instrType"
	lex "mpc/frontend/enums/lexType"
	OT "mpc/frontend/enums/operandType"
	ST "mpc/frontend/enums/symbolType"
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
		T:   OT.Temp,
		Type: t,
		Num: c.TempCounter,
	}
	c.TempCounter++
	return op
}

func Generate(M *ir.Module)  {
	for _, sy := range M.Globals {
		switch sy.T {
		case ST.Proc:
			genProc(M, sy.Proc)
		}
	}
}

func genProc(M *ir.Module, proc *ir.Proc)  {
	c := newContext(proc)
	start := c.NewBlock()
	proc.Code = start
	c.CurrBlock = start

	body := proc.N.Leaves[4]
	genBlock(M, c, body)
	if !proc.Code.HasFlow() {
		proc.Code.Return()
	}
	return 
}

func genBlock(M *ir.Module, c *context, body *ir.Node)  {
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

func genIf(M *ir.Module, c *context, if_ *ir.Node)  {
	exp := if_.Leaves[0]
	block := if_.Leaves[1]
	elseifchain := if_.Leaves[2]
	else_ := if_.Leaves[3]

	op := genExpr(M, c, exp)
	truebl  := c.NewBlock()
	falsebl := c.NewBlock()
	outbl   := c.NewBlock()
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

func genElseIfChain(M *ir.Module, c *context, elseifchain *ir.Node, outbl *ir.BasicBlock)  {
	for _, elseif := range elseifchain.Leaves {
		exp := elseif.Leaves[0]
		block := elseif.Leaves[1]

		op := genExpr(M, c, exp)
		truebl  := c.NewBlock()
		falsebl := c.NewBlock()
		c.CurrBlock.Branch(op, truebl, falsebl)

		c.CurrBlock = truebl
		genBlock(M, c, block)
		c.CurrBlock.Jmp(outbl)
		c.CurrBlock = falsebl
	}
}

func genWhile(M *ir.Module, c *context, while *ir.Node)  {
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

func genReturn(M *ir.Module, c *context, return_ *ir.Node)  {
	for _, ret := range return_.Leaves {
		op := genExpr(M, c, ret)
		storeRet := &ir.Instr {
			T: IT.Store,
			Type: op.Type,
			Operands: []*ir.Operand{op},
		}
		c.CurrBlock.AddInstr(storeRet)
	}
	c.CurrBlock.Return()
}

func genSet(M *ir.Module, c *context, set *ir.Node)  {
	assignees := set.Leaves[0]
	exprlist := set.Leaves[1]

	if len(assignees.Leaves) > 1 && len(exprlist.Leaves) > 1 {
		genMultiAssign(M, c, assignees, exprlist)
		return
	}

	if len(assignees.Leaves) > 1 && len(exprlist.Leaves) == 1 {
		genMultiProcAssign(M, c, assignees, exprlist.Leaves[0])
		return
	}

	if len(assignees.Leaves) == 1 && len(exprlist.Leaves) == 1 {
		genSingleAssign(M, c, assignees.Leaves[0], exprlist.Leaves[0])
		return
	}
}

func genMultiProcAssign(M *ir.Module, c *context, assignees, call *ir.Node) {
	if call.Lex != lex.CALL {
		panic("must be CALL:\n" + ir.FmtNode(call))
	}
	proc := call.Leaves[1]
	args := call.Leaves[0]

	procOp := genExprID(M, c, proc)

	genInitFrame(M, c, procOp)
	genLoadArgs(M, c, args)
	genCallInstr(c, call, procOp)

	genLoadAssignRets(M, c, assignees)
}

func genInitFrame(M *ir.Module, c *context, procOp *ir.Operand) {
	sy := M.Globals[procOp.Label]
	numArgs := genIntOp(len(sy.Proc.Args))
	numRets := genIntOp(len(sy.Proc.Rets))
	init := &ir.Instr{
		T: IT.InitFrame,
		Operands: []*ir.Operand{numArgs, numRets},
	}
	c.CurrBlock.AddInstr(init)
}

func genIntOp(num int) *ir.Operand {
	return &ir.Operand{
		T:   OT.Lit,
		Num: num,
	}
}

func genLoadAssignRets(M *ir.Module, c *context, assignees *ir.Node) {
	for i, ass := range assignees.Leaves {
		if ass.Lex == lex.IDENTIFIER {
			genCallAssign(M, c, ass, i)
			continue
		}
		if ass.Lex == lex.LEFTBRACKET {
			genCallAssignMem(M, c, ass, i)
			continue
		}
	}
}

func genCallInstr(c *context, call *ir.Node, procOp *ir.Operand) {
	iCall := &ir.Instr{
		T: IT.Call,
		Type: call.T,
		Operands: []*ir.Operand{procOp},
	}
	c.CurrBlock.AddInstr(iCall)
}

func genLoadArgs(M *ir.Module, c *context, args *ir.Node) {
	for i, arg := range args.Leaves {
		res := genExpr(M, c, arg)
		dest := genCallOp(arg.T, i, OT.Argument)
		storeArg := &ir.Instr{
			T: IT.Store,
			Type: arg.T,
			Operands: []*ir.Operand{res},
			Destination: dest,
		}
		c.CurrBlock.AddInstr(storeArg)
	}
}

func genCallOp(t T.Type, i int, ot OT.OperandType) *ir.Operand {
	return &ir.Operand {
		T: ot,
		Type: t,
		Num: i,
	}
}

func genCallAssign(M *ir.Module, c *context, ass *ir.Node, i int) {
	dest := genExprID(M, c, ass)
	op := genCallOp(ass.T, i, OT.Return)
	loadRet := &ir.Instr{
		T: IT.Load,
		Type: ass.T,
		Operands: []*ir.Operand{op},
		Destination: dest,
	}
	c.CurrBlock.AddInstr(loadRet)
}

func genCallAssignMem(M *ir.Module, c *context, ass *ir.Node, i int) {
	basePtrOp := genExprID(M, c, ass.Leaves[2]) // CHECK
	offsetOp := genExpr(M, c, ass.Leaves[0])

	newPtr := c.AllocTemp(T.Ptr)
	offset := &ir.Instr {
		T: IT.Offset,
		Type: ass.T,
		Operands: []*ir.Operand{basePtrOp, offsetOp},
		Destination: newPtr,
	}
	c.CurrBlock.AddInstr(offset)

	op := genCallOp(ass.T, i, OT.Return)
	temp := c.AllocTemp(ass.T)
	loadRet := &ir.Instr{
		T: IT.Load,
		Type: ass.T,
		Operands: []*ir.Operand{op},
		Destination: temp,
	}
	c.CurrBlock.AddInstr(loadRet)

	loadPtr := &ir.Instr{
		T: IT.StorePtr,
		Type: ass.T,
		Operands: []*ir.Operand{temp},
		Destination: newPtr,
	}
	c.CurrBlock.AddInstr(loadPtr)
}
	
func genMultiAssign(M *ir.Module, c *context, assignees, exprlist *ir.Node)  {
	for i := range assignees.Leaves {
		ass := assignees.Leaves[i]
		exp := exprlist.Leaves[i]
		genSingleAssign(M, c, ass, exp)
	}
}

func genSingleAssign(M *ir.Module, c *context, assignee, expr *ir.Node)  {
	if assignee.Lex == lex.IDENTIFIER {
		genNormalAssign(M, c, assignee, expr)
		return
	}
	genMemAssign(M, c, assignee, expr)
	return
}

func genNormalAssign(M *ir.Module, c *context, assignee, expr *ir.Node)  {
	op := genExprID(M, c, assignee)
	exp := genExpr(M, c, expr)
	store := &ir.Instr {
		T: IT.Store,
		Type: op.Type,
		Operands: []*ir.Operand{exp},
		Destination: op,
	}
	c.CurrBlock.AddInstr(store)
}

func genMemAssign(M *ir.Module, c *context, assignee, expr *ir.Node)  {
	assID := assignee.Leaves[0]
	indexExp := assignee.Leaves[1]

	indexOp := genExpr(M, c, indexExp)
	idOp := genExprID(M, c, assID)
	expOp := genExpr(M, c, expr)
	store := &ir.Instr {
		T: IT.StorePtr,
		Type: expOp.Type,
		Operands: []*ir.Operand{expOp, indexOp},
		Destination: idOp,
	}
	c.CurrBlock.AddInstr(store)
}

func genExpr(M *ir.Module, c *context, exp *ir.Node) *ir.Operand {
	switch exp.Lex {
	case lex.IDENTIFIER:
		return genExprID(M, c, exp)
	case lex.INT, lex.FALSE, lex.TRUE:
		return genLit(M, c, exp)
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
		return genCall(M, c, exp)
	case lex.LEFTBRACKET:
		return genMemAccess(M, c, exp)
	case lex.NOT:
		return genUnaryOp(M, c, exp)
	}
	return nil
}

// assume a single return
func genCall(M *ir.Module, c *context, call *ir.Node) *ir.Operand {
	proc := call.Leaves[1]
	args := call.Leaves[0]

	procOp := genExprID(M, c, proc)

	genInitFrame(M, c, procOp)
	genLoadArgs(M, c, args)
	genCallInstr(c, call, procOp)

	ret := c.AllocTemp(call.T)
	opRet := genCallOp(call.T, 0, OT.Return)
	loadRet := &ir.Instr{
		T: IT.Load,
		Type: call.T,
		Operands: []*ir.Operand{opRet},
		Destination: ret,
	}
	c.CurrBlock.AddInstr(loadRet)

	return ret
}

func genMemAccess(M *ir.Module, c *context, memAccess *ir.Node) *ir.Operand {
	exp := memAccess.Leaves[0]
	mem := memAccess.Leaves[2]

	basePtrOp := genExprID(M, c, mem)
	offsetOp := genExpr(M, c, exp)

	temp := c.AllocTemp(T.Ptr)
	load := &ir.Instr {
		T: IT.Offset,
		Type: mem.T,
		Operands:    []*ir.Operand{basePtrOp, offsetOp},
		Destination: temp,
	}
	c.CurrBlock.AddInstr(load)

	return temp
}

func genExprID(M *ir.Module, c *context, id *ir.Node) *ir.Operand {
	_, ok := c.Proc.Names[id.Text]
	if ok {
		return &ir.Operand{
			T:     OT.Local,
			Label: id.Text,
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
			T:     OT.Proc,
			Label: id.Text,
		}
	case ST.Mem:
		return &ir.Operand{
			T:     OT.Mem,
			Label: id.Text,
		}
	}
	// Const
	return &ir.Operand{
		T:     OT.Lit,
		Label: global.N.Leaves[1].Text, // zero fucks
	}
}

func genConversion(M *ir.Module, c *context, colon *ir.Node) *ir.Operand {
	it := IT.Convert
	a := genExpr(M, c, colon.Leaves[1])
	dest := c.AllocTemp(colon.T)
	instr := &ir.Instr{
		T:           it,
		Type:        colon.T,
		Operands:    []*ir.Operand{a},
		Destination: dest,
	}
	c.CurrBlock.AddInstr(instr)
	return dest
}

func genLit(M *ir.Module, c *context, lit *ir.Node) *ir.Operand {
	return &ir.Operand{
		T:     OT.Lit,
		Label: lit.Text,
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
		Type:        op.T,
		Operands:    []*ir.Operand{a, b},
		Destination: dest,
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
	panic("lexToBinaryOp: unexpected binOp: "+lex.FmtTypes(op))
}

func genUnaryOp(M *ir.Module, c *context, op *ir.Node) *ir.Operand {
	it := lexToUnaryOp(op.Lex)
	a := genExpr(M, c, op.Leaves[0])
	dest := c.AllocTemp(op.T)
	instr := &ir.Instr{
		T:           it,
		Type:        op.T,
		Operands:    []*ir.Operand{a},
		Destination: dest,
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
