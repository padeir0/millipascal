package gen

import (
	"fmt"
	T "mpc/frontend/Type"
	hirc "mpc/frontend/enums/HIRClass"
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

	LabelCounter int64
	TempCounter  int64
}

func newContext(proc *ir.Proc) *context {
	return &context{
		Proc:         proc,
		LabelCounter: 0,
		TempCounter:  0,
	}
}

func (c *context) NewBlock() *ir.BasicBlock {
	counter := strconv.FormatInt(c.LabelCounter, 10)
	b := &ir.BasicBlock{
		Label: ".L" + counter,
		Code:  []*ir.Instr{},
	}
	c.LabelCounter++
	return b
}

func (c *context) AllocTemp(t *T.Type) *ir.Operand {
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
		case lex.EXIT:
			genExit(M, c, code)
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

func genExit(M *ir.Module, c *context, exit_ *ir.Node) {
	ret := exit_.Leaves[0]
	op := genExpr(M, c, ret)
	c.CurrBlock.Exit(op)
}

func genSet(M *ir.Module, c *context, set *ir.Node) {
	assignees := set.Leaves[0]
	op := set.Leaves[1]
	expr := set.Leaves[2]

	if len(assignees.Leaves) > 1 && len(expr.Leaves) == 1 {
		genMultiProcAssign(M, c, assignees, expr.Leaves[0])
		return
	}

	genSingleAssign(M, c, assignees.Leaves[0], expr, op.Lex)
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

func genIntOp(num int64) *ir.Operand {
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

func genRets(M *ir.Module, c *context, proc *ir.Operand) []*ir.Operand {
	output := []*ir.Operand{}
	for _, ret := range proc.Type.Proc.Rets {
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
		genSingleAssign(M, c, ass, exp, lex.ASSIGNMENT)
	}
}

func genSingleAssign(M *ir.Module, c *context, assignee, expr *ir.Node, op lex.TkType) {
	if assignee.Lex == lex.IDENTIFIER {
		genNormalAssign(M, c, assignee, expr, op)
		return
	}
	genDerefAssign(M, c, assignee, expr, op)
	return
}

func genNormalAssign(M *ir.Module, c *context, assignee, expr *ir.Node, op lex.TkType) {
	dest := genExprID(M, c, assignee)
	exp := genExpr(M, c, expr)
	if op == lex.ASSIGNMENT {
		// TODO: OPT: Try to avoid this COPY instruction
		cp := RIU.Copy(exp, dest)
		c.CurrBlock.AddInstr(cp)
		return
	}
	instrT := mapOpToInstr(op)
	instr := &ir.Instr{
		T: instrT,
		Type: dest.Type,
		Operands: []*ir.Operand{dest, exp},
		Destination: []*ir.Operand{dest},
	}
	c.CurrBlock.AddInstr(instr)
}

func mapOpToInstr(l lex.TkType) IT.InstrType {
	switch l {
		case lex.PLUS_ASSIGN:
			return IT.Add
		case lex.MINUS_ASSIGN:
			return IT.Sub
		case lex.MULTIPLICATION_ASSIGN:
			return IT.Mult
		case lex.DIVISION_ASSIGN:
			return IT.Div
		case lex.REMAINDER_ASSIGN:
			return IT.Rem
	}
	panic(l)
}

func genDerefAssign(M *ir.Module, c *context, left, right *ir.Node, op lex.TkType) {
	leftExpr := left.Leaves[1]
	leftOp := genExpr(M, c, leftExpr)
	rightOp := genExpr(M, c, right)
	if op == lex.ASSIGNMENT {
		store := RIU.StorePtr(rightOp, leftOp)
		c.CurrBlock.AddInstr(store)
		return
	}

	temp := c.AllocTemp(leftExpr.T)
	load := RIU.LoadPtr(leftOp, temp)
	c.CurrBlock.AddInstr(load) // load left -> temp

	instrT := mapOpToInstr(op)
	temp2 := c.AllocTemp(leftExpr.T)
	instr := &ir.Instr{
		T: instrT,
		Type: temp.Type,
		Operands: []*ir.Operand{temp, rightOp},
		Destination: []*ir.Operand{temp2},
	}
	c.CurrBlock.AddInstr(instr) // op temp, right -> temp2

	store := RIU.StorePtr(temp2, leftOp)
	c.CurrBlock.AddInstr(store) // store temp2 -> left
}

func genExpr(M *ir.Module, c *context, exp *ir.Node) *ir.Operand {
	if T.IsInvalid(exp.T) {
		panic("invalid type at: " + exp.String())
	}
	switch exp.Lex {
	case lex.IDENTIFIER:
		return genExprID(M, c, exp)
	case lex.FALSE, lex.TRUE:
		return genBoolLit(M, c, exp)
	case lex.PTR_LIT, lex.I64_LIT, lex.I32_LIT, lex.I16_LIT, lex.I8_LIT:
		return genNumLit(M, c, exp)
	case lex.CHAR_LIT:
		return genCharLit(M, c, exp)
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
		out := genCall(M, c, exp)
		if len(out) == 1 {
			return out[0]
		}
		return nil
	case lex.AT:
		return genDeref(M, c, exp)
	case lex.NOT:
		return genUnaryOp(M, c, exp)
	}
	panic("invalid or unimplemented expression type: " + exp.String())
}

// assume a single return
func genCall(M *ir.Module, c *context, call *ir.Node) []*ir.Operand {
	proc := call.Leaves[1]
	args := call.Leaves[0]

	procOp := genExpr(M, c, proc)

	argOps := genArgs(M, c, args)
	retOps := genRets(M, c, procOp)
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
			Num:    int64(posSy.Position),
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
			Type:   global.Proc.T,
		}
	case ST.Mem:
		return &ir.Operand{
			Hirc:   hirc.Global,
			Symbol: global,
			Type:   T.T_Ptr,
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

func genNumLit(M *ir.Module, c *context, lit *ir.Node) *ir.Operand {
	text := lit.Text
	if lit.Lex != lex.I64_LIT {
		text = lit.Text[0 : len(lit.Text)-1]
	}
	value := convertNum(text)
	return &ir.Operand{
		Hirc: hirc.Lit,
		Type: lit.T,
		Num:  value,
	}
}

func convertNum(text string) int64 {
	if len(text) == 0 {
		panic(text)
	}
	if len(text) < 3 {
		return parseNormal(text)
	}
	if text[0] == '0' && text[1] == 'x' {
		return parseHex(text[2:])
	}
	if text[0] == '0' && text[1] == 'b' {
		return parseBin(text[2:])
	}
	return parseNormal(text)
}

func parseNormal(text string) int64 {
	var output int64 = 0
	for i := range text {
		output *= 10
		char := text[i]
		if char >= '0' || char <= '9' {
			output += int64(char - '0')
		} else {
			panic(text)
		}
	}
	return output
}

func parseHex(text string) int64 {
	var output int64 = 0
	for i := range text {
		output *= 16
		char := text[i]
		if (char >= '0' && char <= '9') {
			output += int64(char - '0')
		} else if (char >= 'a' && char <= 'f') {
			output += int64(char - 'a') + 10
		} else if (char >= 'A' && char <= 'F') {
			output += int64(char - 'A') + 10
		} else {
			panic(text)
		}
	}
	return output
}

func parseBin(text string) int64 {
	var output int64 = 0
	for i := range text {
		output *= 2
		char := text[i]
		if (char == '0' || char == '1') {
			output += int64(char - '0')
		} else {
			panic(text)
		}
	}
	return output
}

func genCharLit(M *ir.Module, c *context, lit *ir.Node) *ir.Operand {
	text := lit.Text[1 : len(lit.Text)-1]
	value := int64(text[0])
	if len(text) > 1 {
		if text[0] != '\\' {
		}
		switch text {
		case "\\n":
			value = '\n'
		case "\\t":
			value = '\t'
		case "\\r":
			value = '\r'
		case "\\'":
			value = '\''
		case "\\\"":
			value = '"'
		default:
			fmt.Println(ir.FmtNode(lit))
			panic("too many chars in char :C")
		}
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
		Num:  int64(value),
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
