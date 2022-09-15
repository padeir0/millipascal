package gen

import (
	"mpc/frontend/ast"
	T "mpc/frontend/enums/Type"
	IT "mpc/frontend/enums/instrType"
	lex "mpc/frontend/enums/lexType"
	OT "mpc/frontend/enums/operandType"
	ST "mpc/frontend/enums/symbolType"
	"mpc/frontend/errors"
	"strconv"
)

type context struct {
	Proc *ast.Proc

	CurrBlock *ast.BasicBlock

	LabelCounter int
	TempCounter  int
}

func newContext(proc *ast.Proc) *context {
	return &context{
		Proc:         proc,
		LabelCounter: 0,
		TempCounter:  0,
	}
}

func (c *context) NewBlock() *ast.BasicBlock {
	counter := strconv.Itoa(c.LabelCounter)
	b := &ast.BasicBlock{
		Label: ".L" + counter,
		Code:  []*ast.Instr{},
	}
	c.LabelCounter++
	return b
}

func (c *context) AllocTemp(t T.Type) *ast.Operand {
	op := &ast.Operand{
		T:   OT.Temp,
		Type: t,
		Num: c.TempCounter,
	}
	c.TempCounter++
	return op
}

func Generate(M *ast.Module) *errors.CompilerError {
	for _, sy := range M.Globals {
		switch sy.T {
		case ST.Proc:
			genProc(M, sy.Proc)
		case ST.Mem:
			genMem(M, sy.Mem)
		}
	}
	return nil
}

func genProc(M *ast.Module, proc *ast.Proc) *errors.CompilerError {
	c := newContext(proc)
	start := c.NewBlock()
	proc.Code = start
	c.CurrBlock = start

	body := proc.N.Leaves[4]
	return genBlock(M, c, body)
}

func genBlock(M *ast.Module, c *context, body *ast.Node) *errors.CompilerError {
	for _, code := range body.Leaves {
		var err *errors.CompilerError
		switch code.Lex {
		case lex.IF:
			err = genIf(M, c, code)
		case lex.WHILE:
			err = genWhile(M, c, code)
		case lex.RETURN:
			err = genReturn(M, c, code)
		case lex.COPY:
			err = genCopy(M, c, code)
		case lex.SET:
			err = genSet(M, c, code)
		default:
			genExpr(M, c, code)
		}
		if err != nil {
			return err
		}
	}
	return nil
}

func genIf(M *ast.Module, c *context, if_ *ast.Node) *errors.CompilerError {
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
	err := genBlock(M, c, block)
	if err != nil {
		return err
	}
	c.CurrBlock.Jmp(outbl)

	c.CurrBlock = falsebl
	if elseifchain != nil {
		err := genElseIfChain(M, c, elseifchain, outbl)
		if err != nil {
			return err
		}
	}
	if else_ != nil {
		err := genBlock(M, c, else_.Leaves[0])
		if err != nil {
			return err
		}
	}
	c.CurrBlock.Jmp(outbl)
	c.CurrBlock = outbl

	return nil
}

func genElseIfChain(M *ast.Module, c *context, elseifchain *ast.Node, outbl *ast.BasicBlock) *errors.CompilerError {
	for _, elseif := range elseifchain.Leaves {
		exp := elseif.Leaves[0]
		block := elseif.Leaves[1]

		op := genExpr(M, c, exp)
		truebl  := c.NewBlock()
		falsebl := c.NewBlock()
		c.CurrBlock.Branch(op, truebl, falsebl)

		c.CurrBlock = truebl
		err := genBlock(M, c, block)
		if err != nil {
			return err
		}
		c.CurrBlock.Jmp(outbl)
		c.CurrBlock = falsebl
	}
	return nil
}

func genWhile(M *ast.Module, c *context, while *ast.Node) *errors.CompilerError {
	loop_start := c.NewBlock()
	loop_body := c.NewBlock()
	loop_end := c.NewBlock()

	c.CurrBlock.Jmp(loop_start)
	c.CurrBlock = loop_start

	op := genExpr(M, c, while.Leaves[0])
	c.CurrBlock.Branch(op, loop_body, loop_end)

	c.CurrBlock = loop_body
	err := genBlock(M, c, while.Leaves[1])
	if err != nil {
		return err
	}
	c.CurrBlock.Jmp(loop_start)

	c.CurrBlock = loop_end

	return nil
}

func genReturn(M *ast.Module, c *context, return_ *ast.Node) *errors.CompilerError {
	for _, ret := range return_.Leaves {
		op := genExpr(M, c, ret)
		storeRet := &ast.Instr {
			T: IT.StoreRet,
			Type: op.Type,
			Operands: []*ast.Operand{op},
		}
		c.CurrBlock.AddInstr(storeRet)
	}
	c.CurrBlock.Return()
	return nil
}

func genCopy(M *ast.Module, c *context, body *ast.Node) *errors.CompilerError {
	return nil
}

func genSet(M *ast.Module, c *context, set *ast.Node) *errors.CompilerError {
	assignees := set.Leaves[0]
	exprlist := set.Leaves[1]

	if len(assignees.Leaves) > 1 && len(exprlist.Leaves) > 1 {
		return genMultiAssign(M, c, assignees, exprlist)
	}

	if len(assignees.Leaves) > 1 && len(exprlist.Leaves) == 1 {
		return genMultiProcAssign(M, c, assignees, exprlist.Leaves[0])
	}

	if len(assignees.Leaves) == 1 && len(exprlist.Leaves) == 1 {
		err := genSingleAssign(M, c, assignees.Leaves[0], exprlist.Leaves[0])
		if err != nil {
			return err
		}
	}

	return nil
}

func genMultiProcAssign(M *ast.Module, c *context, assignees, call *ast.Node) *errors.CompilerError {
	if call.Lex != lex.CALL {
		panic("must be CALL:\n" + ast.FmtNode(call))
	}
	proc := call.Leaves[1]
	args := call.Leaves[0]

	procOp := genExprID(M, c, proc)

	for _, arg := range args.Leaves {
		res := genExpr(M, c, arg)
		storeArg := &ast.Instr{
			T: IT.StoreArg,
			Type: arg.T,
			Operands: []*ast.Operand{res},
		}
		c.CurrBlock.AddInstr(storeArg)
	}

	iCall := &ast.Instr{
		T: IT.Call,
		Type: call.T,
		Operands: []*ast.Operand{procOp},
	}
	c.CurrBlock.AddInstr(iCall)

	for _, ass := range assignees.Leaves {
		if ass.Lex == lex.IDENTIFIER {
			genCallAssign(M, c, ass)
			continue
		}
		if ass.Lex == lex.LEFTBRACKET {
			genCallAssignMem(M, c, ass)
			continue
		}
	}
	return nil
}

func genCallAssign(M *ast.Module, c *context, ass *ast.Node) {
	dest := genExprID(M, c, ass)
	loadRet := &ast.Instr{
		T: IT.LoadRet,
		Type: ass.T,
		Destination: []*ast.Operand{dest},
	}
	c.CurrBlock.AddInstr(loadRet)
}

func genCallAssignMem(M *ast.Module, c *context, ass *ast.Node) {
	destOp := genExprID(M, c, ass.Leaves[0]) // CHECK
	indexOp := genExpr(M, c, ass.Leaves[1])
	temp := c.AllocTemp(ass.T)
	loadRet := &ast.Instr{
		T: IT.LoadRet,
		Type: ass.T,
		Destination: []*ast.Operand{temp},
	}
	c.CurrBlock.AddInstr(loadRet)
	memLoad := &ast.Instr{
		T: IT.MemStore,
		Type: ass.T,
		Operands: []*ast.Operand{temp},
		Destination: []*ast.Operand{destOp, indexOp},
	}
	c.CurrBlock.AddInstr(memLoad)
}
	
func genMultiAssign(M *ast.Module, c *context, assignees, exprlist *ast.Node) *errors.CompilerError {
	for i := range assignees.Leaves {
		ass := assignees.Leaves[i]
		exp := exprlist.Leaves[i]
		err := genSingleAssign(M, c, ass, exp)
		if err != nil {
			return err
		}
	}
	return nil
}

func genSingleAssign(M *ast.Module, c *context, assignee, expr *ast.Node) *errors.CompilerError {
	if assignee.Lex == lex.IDENTIFIER {
		return genNormalAssign(M, c, assignee, expr)
	}
	return genMemAssign(M, c, assignee, expr)
}

func genNormalAssign(M *ast.Module, c *context, assignee, expr *ast.Node) *errors.CompilerError {
	op := genExprID(M, c, assignee)
	exp := genExpr(M, c, expr)
	store := &ast.Instr {
		T: IT.LocalStore,
		Type: op.Type,
		Operands: []*ast.Operand{exp},
		Destination: []*ast.Operand{op},
	}
	c.CurrBlock.AddInstr(store)
	return nil
}

func genMemAssign(M *ast.Module, c *context, assignee, expr *ast.Node) *errors.CompilerError {
	assID := assignee.Leaves[0]
	indexExp := assignee.Leaves[1]

	indexOp := genExpr(M, c, indexExp)
	idOp := genExprID(M, c, assID)
	expOp := genExpr(M, c, expr)
	store := &ast.Instr {
		T: IT.LocalStore,
		Type: expOp.Type,
		Operands: []*ast.Operand{expOp},
		Destination: []*ast.Operand{idOp, indexOp},
	}
	c.CurrBlock.AddInstr(store)
	return nil
}

func genExpr(M *ast.Module, c *context, exp *ast.Node) *ast.Operand {
	switch exp.Lex {
	case lex.IDENTIFIER:
		return genExprID(M, c, exp)
	case lex.INT, lex.FALSE, lex.TRUE, lex.CHAR:
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
func genCall(M *ast.Module, c *context, call *ast.Node) *ast.Operand {
	proc := call.Leaves[1]
	args := call.Leaves[0]

	procOp := genExprID(M, c, proc)

	for _, arg := range args.Leaves {
		res := genExpr(M, c, arg)
		storeArg := &ast.Instr{
			T: IT.StoreArg,
			Type: arg.T,
			Operands: []*ast.Operand{res},
		}
		c.CurrBlock.AddInstr(storeArg)
	}
	iCall := &ast.Instr{
		T: IT.Call,
		Type: call.T,
		Operands: []*ast.Operand{procOp},
	}
	c.CurrBlock.AddInstr(iCall)

	ret := c.AllocTemp(call.T)
	loadRet := &ast.Instr{
		T: IT.LoadRet,
		Type: call.T,
		Destination: []*ast.Operand{ret},
	}
	c.CurrBlock.AddInstr(loadRet)

	return ret
}

func genMemAccess(M *ast.Module, c *context, memAccess *ast.Node) *ast.Operand {
	exp := memAccess.Leaves[0]
	mem := memAccess.Leaves[1]

	memOp := genExprID(M, c, mem)
	expOp := genExpr(M, c, exp)

	boundscheck := &ast.Instr {
		T: IT.BoundsCheck,
		Type: mem.T,
		Operands: []*ast.Operand{memOp, expOp},
	}

	temp := c.AllocTemp(mem.T)
	load := &ast.Instr {
		T: IT.MemLoad,
		Type: mem.T,
		Operands:    []*ast.Operand{memOp, expOp},
		Destination: []*ast.Operand{temp},
	}

	c.CurrBlock.AddInstr(boundscheck)
	c.CurrBlock.AddInstr(load)

	return temp
}

func genExprID(M *ast.Module, c *context, id *ast.Node) *ast.Operand {
	_, ok := c.Proc.Names[id.Text]
	if ok {
		return &ast.Operand{
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

func globalToOperand(id *ast.Node, global *ast.Symbol) *ast.Operand {
	switch global.T {
	case ST.Proc:
		return &ast.Operand{
			T:     OT.Proc,
			Label: id.Text,
		}
	case ST.Mem:
		return &ast.Operand{
			T:     OT.Mem,
			Label: id.Text,
		}
	}
	// Const
	return &ast.Operand{
		T:     OT.Lit,
		Label: global.N.Leaves[1].Text, // zero fucks
	}
}

func genConversion(M *ast.Module, c *context, colon *ast.Node) *ast.Operand {
	it := IT.Convert
	a := genExpr(M, c, colon.Leaves[0])
	dest := c.AllocTemp(colon.T)
	instr := &ast.Instr{
		T:           it,
		Type:        colon.T,
		Operands:    []*ast.Operand{a},
		Destination: []*ast.Operand{dest},
	}
	c.CurrBlock.AddInstr(instr)
	return dest
}

func genLit(M *ast.Module, c *context, lit *ast.Node) *ast.Operand {
	return &ast.Operand{
		T:     OT.Lit,
		Label: lit.Text,
	}
}

func genPlusMinus(M *ast.Module, c *context, op *ast.Node) *ast.Operand {
	if len(op.Leaves) == 2 {
		return genBinaryOp(M, c, op)
	}
	return genUnaryOp(M, c, op)
}

func genBinaryOp(M *ast.Module, c *context, op *ast.Node) *ast.Operand {
	it := lexToBinaryOp(op.Lex)
	a := genExpr(M, c, op.Leaves[0])
	b := genExpr(M, c, op.Leaves[1])
	dest := c.AllocTemp(op.T)
	instr := &ast.Instr{
		T:           it,
		Type:        op.T,
		Operands:    []*ast.Operand{a, b},
		Destination: []*ast.Operand{dest},
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

func genUnaryOp(M *ast.Module, c *context, op *ast.Node) *ast.Operand {
	it := lexToUnaryOp(op.Lex)
	a := genExpr(M, c, op.Leaves[0])
	dest := c.AllocTemp(op.T)
	instr := &ast.Instr{
		T:           it,
		Type:        op.T,
		Operands:    []*ast.Operand{a},
		Destination: []*ast.Operand{dest},
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

func genMem(M *ast.Module, mem *ast.Mem) *errors.CompilerError {
	return nil
}
