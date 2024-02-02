package linearization

import (
	. "mpc/core"

	//https://github.com/padeir0/pir
	"github.com/padeir0/pir"
	pirc "github.com/padeir0/pir/class"
	IT "github.com/padeir0/pir/instrkind"
	T "github.com/padeir0/pir/types"
	RIU "github.com/padeir0/pir/util"

	ir "mpc/core/module"
	lex "mpc/core/module/lexkind"
	ST "mpc/core/module/symbolkind"
	msg "mpc/messages"
	"strconv"
)

type context struct {
	Program   *pir.Program
	symbolMap map[string]pir.SymbolID

	PirProc *pir.Procedure
	ModProc *ir.Proc

	CurrBlock *pir.BasicBlock

	TempCounter uint64
}

func newContext(M *ir.Module) *context {
	return &context{
		Program:     pir.NewProgram(),
		symbolMap:   map[string]pir.SymbolID{},
		TempCounter: 0,
	}
}

func (c *context) NewBlock() (pir.BlockID, *pir.BasicBlock) {
	id := len(c.PirProc.AllBlocks)
	v := strconv.FormatInt(int64(id), 10)
	b := &pir.BasicBlock{
		Label: ".L" + v,
		Code:  []pir.Instr{},
	}
	c.PirProc.AllBlocks = append(c.PirProc.AllBlocks, b)
	return pir.BlockID(id), b
}

func (c *context) AllocTemp(t *T.Type) pir.Operand {
	op := pir.Operand{
		Class: pirc.Temp,
		Type:  t,
		Num:   c.TempCounter,
	}
	c.TempCounter++
	return op
}

func (c *context) GetSymbolID(modName string, name string) pir.SymbolID {
	return c.symbolMap[modName+"_"+name]
}

func (c *context) GetSymbol(modName string, name string) *pir.Symbol {
	i := c.symbolMap[modName+"_"+name]
	return c.Program.Symbols[i]
}

func Generate(M *ir.Module) (*pir.Program, *Error) {
	c := newContext(M)

	declAll(c, M)
	M.ResetVisited()

	err := genAll(c, M)
	if err != nil {
		return nil, err
	}

	M.ResetVisited()
	sy, ok := M.Globals["main"]
	if !ok {
		panic("UERES MAIN")
	}

	c.Program.Entry = c.GetSymbolID(sy.ModuleName, sy.Name)
	c.Program.Name = sy.ModuleName

	return c.Program, nil
}

func declAll(c *context, M *ir.Module) {
	if M.Visited {
		return
	}
	M.Visited = true
	for _, dep := range M.Dependencies {
		declAll(c, dep.M)
	}
	for _, sy := range M.Globals {
		if !sy.External {
			if sy.T == ST.Proc {
				p := newPirProc(M.Name, sy.Proc)
				i := c.Program.AddProc(p)
				c.symbolMap[p.Label] = pir.SymbolID(i)
			} else if sy.T == ST.Mem {
				m := newPirMem(M.Name, sy.Mem)
				i := c.Program.AddMem(m)
				c.symbolMap[m.Label] = pir.SymbolID(i)
			} else if sy.T == ST.Builtin {
				p := newBuiltin(sy)
				i := c.Program.AddBuiltin(p)
				c.symbolMap[p.Label] = pir.SymbolID(i)
			}
		}
	}
}

func genAll(c *context, M *ir.Module) *Error {
	if M.Visited {
		return nil
	}
	M.Visited = true
	for _, dep := range M.Dependencies {
		err := genAll(c, dep.M)
		if err != nil {
			return err
		}
	}
	for _, sy := range M.Globals {
		if !sy.External {
			if sy.T == ST.Proc {
				err := genProc(c, M, sy.Proc)
				if err != nil {
					return err
				}
			}
		}
	}
	return nil
}

func genProc(c *context, M *ir.Module, proc *ir.Proc) *Error {
	c.ModProc = proc
	c.PirProc = c.GetSymbol(M.Name, proc.Name).Proc

	startID, start := c.NewBlock()
	c.PirProc.Start = startID
	c.CurrBlock = start

	body := proc.N.Leaves[4]
	genBlock(M, c, body)
	if !pir.ProperlyTerminates(c.PirProc) {
		if proc.DoesReturnSomething() {
			return msg.NotAllCodePathsReturnAValue(M, proc)
		}
		for _, bb := range c.PirProc.AllBlocks {
			if !bb.HasFlow() {
				setReturn(bb)
			}
		}
	}
	return nil
}

func setReturn(b *pir.BasicBlock) {
	b.Return([]pir.Operand{})
}

func genBlock(M *ir.Module, c *context, body *ir.Node) {
	for _, code := range body.Leaves {
		switch code.Lex {
		case lex.IF:
			genIf(M, c, code)
		case lex.WHILE:
			genWhile(M, c, code)
		case lex.SET:
			genSet(M, c, code)
		case lex.RETURN:
			genReturn(M, c, code)
			return
		case lex.EXIT:
			genExit(M, c, code)
			return
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
	trueblID, truebl := c.NewBlock()
	falseblID, falsebl := c.NewBlock()
	var outblID pir.BlockID
	var outbl *pir.BasicBlock // we just generate an out block if it's reachable

	c.CurrBlock.Branch(op, trueblID, falseblID)

	c.CurrBlock = truebl
	genBlock(M, c, block)
	if c.CurrBlock != nil && !c.CurrBlock.HasFlow() {
		outblID, outbl = c.NewBlock()
		c.CurrBlock.Jmp(outblID)
	}

	c.CurrBlock = falsebl
	if elseifchain != nil {
		genElseIfChain(M, c, elseifchain, outblID)
	}
	if else_ != nil {
		genBlock(M, c, else_.Leaves[0])
	}
	if c.CurrBlock != nil && !c.CurrBlock.HasFlow() {
		if outbl == nil {
			outblID, outbl = c.NewBlock()
		}
		c.CurrBlock.Jmp(outblID)
	}
	c.CurrBlock = outbl
}

func genElseIfChain(M *ir.Module, c *context, elseifchain *ir.Node, outblID pir.BlockID) {
	for _, elseif := range elseifchain.Leaves {
		exp := elseif.Leaves[0]
		block := elseif.Leaves[1]

		op := genExpr(M, c, exp)
		trueblID, truebl := c.NewBlock()
		falseblID, falsebl := c.NewBlock()
		c.CurrBlock.Branch(op, trueblID, falseblID)

		c.CurrBlock = truebl
		genBlock(M, c, block)
		if c.CurrBlock != nil && !c.CurrBlock.HasFlow() {
			c.CurrBlock.Jmp(outblID)
		}
		c.CurrBlock = falsebl
	}
}

func genWhile(M *ir.Module, c *context, while *ir.Node) {
	loop_startID, loop_start := c.NewBlock()
	loop_bodyID, loop_body := c.NewBlock()
	loop_endID, loop_end := c.NewBlock()

	c.CurrBlock.Jmp(loop_startID)
	c.CurrBlock = loop_start

	op := genExpr(M, c, while.Leaves[0])
	c.CurrBlock.Branch(op, loop_bodyID, loop_endID)

	c.CurrBlock = loop_body
	genBlock(M, c, while.Leaves[1])
	if !c.CurrBlock.HasFlow() {
		c.CurrBlock.Jmp(loop_startID)
	}

	c.CurrBlock = loop_end
}

func genReturn(M *ir.Module, c *context, return_ *ir.Node) {
	if c.CurrBlock.IsTerminal() {
		return
	}
	operands := []pir.Operand{}
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

	if len(assignees.Leaves) > 1 {
		genMultiProcAssign(M, c, assignees, expr)
		return
	}

	genSingleAssign(M, c, assignees.Leaves[0], expr, op.Lex)
	return
}

func genMultiProcAssign(M *ir.Module, c *context, assignees, call *ir.Node) {
	if call.Lex != lex.CALL {
		panic("must be CALL:\n" + call.String())
	}
	// TODO: OPT: pass assignees to genCall so that no copying needs to happen
	rets := genCall(M, c, call)

	genLoadAssignRets(M, c, assignees, rets)
}

func genIntOp(num uint64) *pir.Operand {
	return &pir.Operand{
		Class: pirc.Lit,
		Num:   num,
	}
}

func genLoadAssignRets(M *ir.Module, c *context, assignees *ir.Node, ops []pir.Operand) {
	for i, ass := range assignees.Leaves {
		op := ops[i]
		if ass.Lex == lex.IDENTIFIER {
			genCallAssign(M, c, ass, op)
			continue
		}
		if ass.Lex == lex.AT {
			genCallAssignMem(M, c, ass, op)
			continue
		}
	}
}

func genCallInstr(c *context, proc pir.Operand, args, rets []pir.Operand) {
	operands := []pir.Operand{proc}
	operands = append(operands, args...)
	iCall := pir.Instr{
		T:           IT.Call,
		Operands:    operands,
		Destination: rets,
	}
	c.CurrBlock.AddInstr(iCall)
}

func genArgs(M *ir.Module, c *context, args *ir.Node) []pir.Operand {
	output := []pir.Operand{}
	for _, arg := range args.Leaves {
		res := genExpr(M, c, arg)
		output = append(output, res)
	}
	return output
}

func genRets(M *ir.Module, c *context, proc pir.Operand) []pir.Operand {
	output := []pir.Operand{}
	for _, ret := range proc.Type.Proc.Rets {
		op := c.AllocTemp(ret)
		output = append(output, op)
	}
	return output
}

func genCallAssign(M *ir.Module, c *context, ass *ir.Node, op pir.Operand) {
	dest := genExprID(M, c, ass)
	// TODO: OPT: try to avoid this COPY instruction
	loadRet := RIU.Copy(op, dest)
	c.CurrBlock.AddInstr(loadRet)
}

func genCallAssignMem(M *ir.Module, c *context, ass *ir.Node, op pir.Operand) {
	ptrOp := genExpr(M, c, ass.Leaves[1])
	loadPtr := RIU.StorePtr(op, ptrOp)
	c.CurrBlock.AddInstr(loadPtr)
}

func genSingleAssign(M *ir.Module, c *context, assignee, expr *ir.Node, op lex.LexKind) {
	if assignee.Lex == lex.IDENTIFIER {
		genNormalAssign(M, c, assignee, expr, op)
		return
	}
	genDerefAssign(M, c, assignee, expr, op)
	return
}

func genNormalAssign(M *ir.Module, c *context, assignee, expr *ir.Node, op lex.LexKind) {
	dest := genExprID(M, c, assignee)
	exp := genExpr(M, c, expr)
	if op == lex.ASSIGNMENT {
		// TODO: OPT: Try to avoid this COPY instruction
		cp := RIU.Copy(exp, dest)
		c.CurrBlock.AddInstr(cp)
		return
	}
	instrT := mapOpToInstr(op)
	instr := pir.Instr{
		T:           instrT,
		Type:        dest.Type,
		Operands:    []pir.Operand{dest, exp},
		Destination: []pir.Operand{dest},
	}
	c.CurrBlock.AddInstr(instr)
}

func mapOpToInstr(l lex.LexKind) IT.InstrKind {
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

func genDerefAssign(M *ir.Module, c *context, left, right *ir.Node, op lex.LexKind) {
	leftExpr := left.Leaves[1]
	leftType := left.Leaves[0]
	rightOp := genExpr(M, c, right)
	leftOp := genExpr(M, c, leftExpr)
	if op == lex.ASSIGNMENT {
		store := RIU.StorePtr(rightOp, leftOp)
		c.CurrBlock.AddInstr(store)
		return
	}

	temp := c.AllocTemp(leftType.T)
	load := RIU.LoadPtr(leftOp, temp)
	c.CurrBlock.AddInstr(load) // load left -> temp

	instrT := mapOpToInstr(op)
	temp2 := c.AllocTemp(leftType.T)
	instr := pir.Instr{
		T:           instrT,
		Type:        temp.Type,
		Operands:    []pir.Operand{temp, rightOp},
		Destination: []pir.Operand{temp2},
	}
	c.CurrBlock.AddInstr(instr) // op temp, right -> temp2

	store := RIU.StorePtr(temp2, leftOp)
	c.CurrBlock.AddInstr(store) // store temp2 -> left
}

func genExpr(M *ir.Module, c *context, exp *ir.Node) pir.Operand {
	if T.IsInvalid(exp.T) {
		panic("invalid type at: " + exp.Text)
	}
	switch exp.Lex {
	case lex.IDENTIFIER:
		return genExprID(M, c, exp)
	case lex.SIZEOF:
		return genSizeOfNum(exp)
	case lex.DOUBLECOLON:
		return genExternalID(c, M, exp)
	case lex.FALSE, lex.TRUE:
		return genBoolLit(M, c, exp)
	case lex.PTR_LIT, lex.I64_LIT, lex.I32_LIT, lex.I16_LIT, lex.I8_LIT,
		lex.U64_LIT, lex.U32_LIT, lex.U16_LIT, lex.U8_LIT, lex.CHAR_LIT:
		return genNumLit(M, c, exp)
	case lex.PLUS, lex.MINUS:
		return genBinaryOp(M, c, exp)
	case lex.MULTIPLICATION, lex.DIVISION, lex.REMAINDER,
		lex.EQUALS, lex.DIFFERENT,
		lex.MORE, lex.MOREEQ, lex.LESS, lex.LESSEQ,
		lex.AND, lex.OR, lex.SHIFTLEFT, lex.SHIFTRIGHT,
		lex.BITWISEAND, lex.BITWISEOR, lex.BITWISEXOR:
		return genBinaryOp(M, c, exp)
	case lex.COLON:
		return genConversion(M, c, exp)
	case lex.CALL:
		out := genCall(M, c, exp)
		if len(out) == 1 {
			return out[0]
		}
		return pir.Operand{}
	case lex.AT:
		return genDeref(M, c, exp)
	case lex.NOT, lex.NEG, lex.BITWISENOT:
		return genUnaryOp(M, c, exp)
	case lex.DOT:
		return genDot(M, c, exp)
	}
	panic("invalid or unimplemented expression type: " + exp.Text)
}

// assume a single return
func genCall(M *ir.Module, c *context, call *ir.Node) []pir.Operand {
	proc := call.Leaves[1]
	args := call.Leaves[0]

	procOp := genExpr(M, c, proc)

	argOps := genArgs(M, c, args)
	retOps := genRets(M, c, procOp)
	genCallInstr(c, procOp, argOps, retOps)

	return retOps
}

func genDeref(M *ir.Module, c *context, memAccess *ir.Node) pir.Operand {
	t := memAccess.Leaves[0].T
	exp := memAccess.Leaves[1]

	ptrOp := genExpr(M, c, exp)

	dest := c.AllocTemp(t)
	loadPtr := RIU.LoadPtr(ptrOp, dest)
	c.CurrBlock.AddInstr(loadPtr)

	return dest
}

func genExternalID(c *context, M *ir.Module, dcolon *ir.Node) pir.Operand {
	mod := dcolon.Leaves[0].Text
	id := dcolon.Leaves[1].Text
	otherM := M.Dependencies[mod].M
	sy := otherM.Exported[id]
	return globalToOperand(c, M, sy)
}

func genExprID(M *ir.Module, c *context, id *ir.Node) pir.Operand {
	decl, ok := c.ModProc.Vars[id.Text]
	if ok {
		return pir.Operand{
			Class: pirc.Local,
			Type:  id.T,
			Num:   uint64(decl.Position),
		}
	}
	posSy, ok := c.ModProc.ArgMap[id.Text]
	if ok {
		return pir.Operand{
			Class: pirc.Arg,
			Type:  id.T,
			Num:   uint64(posSy.Position),
		}
	}
	global, ok := M.Globals[id.Text]
	if ok {
		return globalToOperand(c, M, global)
	}
	panic("genExprID: global not found")
}

func globalToOperand(c *context, M *ir.Module, global *ir.Symbol) pir.Operand {
	if global.External {
		global = M.GetSymbol(global.Name)
	}
	i := uint64(c.symbolMap[global.ModuleName+"_"+global.Name])
	switch global.T {
	case ST.Builtin:
		return pir.Operand{
			Class: pirc.Global,
			Type:  global.Type,
			Num:   i,
		}
	case ST.Proc:
		return pir.Operand{
			Class: pirc.Global,
			Type:  global.N.T,
			Num:   i,
		}
	case ST.Mem:
		return pir.Operand{
			Class: pirc.Global,
			Type:  T.T_Ptr,
			Num:   i,
		}
	case ST.Const:
		return pir.Operand{
			Class: pirc.Lit,
			Type:  global.Type,
			Num:   global.Const.Value,
		}
	}
	panic("wht jus heppn?")
}

func genSizeOfNum(sizeof *ir.Node) pir.Operand {
	t := sizeof.Leaves[0]
	size := t.T.Size()
	return pir.Operand{
		Class: pirc.Lit,
		Type:  sizeof.T,
		Num:   uint64(size),
	}
}

func genConversion(M *ir.Module, c *context, colon *ir.Node) pir.Operand {
	a := genExpr(M, c, colon.Leaves[1])
	dest := c.AllocTemp(colon.T)
	instr := RIU.Convert(a, dest)
	c.CurrBlock.AddInstr(instr)
	return dest
}

func genNumLit(M *ir.Module, c *context, lit *ir.Node) pir.Operand {
	return pir.Operand{
		Class: pirc.Lit,
		Type:  lit.T,
		Num:   lit.Value,
	}
}

func genBoolLit(M *ir.Module, c *context, lit *ir.Node) pir.Operand {
	value := 0
	if lit.Lex == lex.TRUE {
		value = 1
	}
	return pir.Operand{
		Class: pirc.Lit,
		Type:  lit.T,
		Num:   uint64(value),
	}
}

func genBinaryOp(M *ir.Module, c *context, op *ir.Node) pir.Operand {
	it := lexToBinaryOp(op.Lex)
	a := genExpr(M, c, op.Leaves[0])
	b := genExpr(M, c, op.Leaves[1])
	dest := c.AllocTemp(op.T)
	instr := pir.Instr{
		T:           it,
		Type:        a.Type,
		Operands:    []pir.Operand{a, b},
		Destination: []pir.Operand{dest},
	}
	c.CurrBlock.AddInstr(instr)
	return dest
}

func lexToBinaryOp(op lex.LexKind) IT.InstrKind {
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
	case lex.SHIFTLEFT:
		return IT.ShiftLeft
	case lex.SHIFTRIGHT:
		return IT.ShiftRight
	case lex.BITWISEAND:
		return IT.And
	case lex.BITWISEOR:
		return IT.Or
	case lex.BITWISEXOR:
		return IT.Xor
	}
	panic("lexToBinaryOp: unexpected binOp: " + lex.FmtTypes(op))
}

func genUnaryOp(M *ir.Module, c *context, op *ir.Node) pir.Operand {
	it := lexToUnaryOp(op.Lex)
	a := genExpr(M, c, op.Leaves[0])
	dest := c.AllocTemp(op.T)
	instr := pir.Instr{
		T:           it,
		Type:        op.T,
		Operands:    []pir.Operand{a},
		Destination: []pir.Operand{dest},
	}
	c.CurrBlock.AddInstr(instr)
	return dest
}

func lexToUnaryOp(op lex.LexKind) IT.InstrKind {
	switch op {
	case lex.NEG:
		return IT.Neg
	case lex.NOT:
		return IT.Not
	case lex.BITWISENOT:
		return IT.Not
	}
	panic("lexToUnaryOp: unexpected unaryOp")
}

func genDot(M *ir.Module, c *context, dot *ir.Node) pir.Operand {
	mem := dot.Leaves[1]
	s := M.GetSymbol(mem.Text)
	return pir.Operand{
		Class: pirc.Lit,
		Type:  T.T_I64,
		Num:   uint64(s.Mem.Size),
	}
}

func newBuiltin(sy *ir.Symbol) *pir.Procedure {
	rets := sy.Type.Proc.Rets
	args := sy.Type.Proc.Args
	return &pir.Procedure{
		Label: "_" + sy.Name,
		Vars:  nil,
		Rets:  rets,
		Args:  args,
	}
}

func newPirProc(modName string, P *ir.Proc) *pir.Procedure {
	args := []*T.Type{}
	for _, arg := range P.Args {
		args = append(args, arg.Type)
	}
	vars := make([]*T.Type, len(P.Vars))
	for _, ps := range P.Vars {
		vars[ps.Position] = ps.Symbol.Type
	}
	return &pir.Procedure{
		Label: modName + "_" + P.Name,
		Vars:  vars,
		Rets:  P.Rets,
		Args:  args,
	}
}

func newPirMem(modName string, M *ir.Mem) *pir.MemoryDecl {
	return &pir.MemoryDecl{
		Label: modName + "_" + M.Name,
		Size:  M.Size,
		Data:  M.Contents,
	}
}
