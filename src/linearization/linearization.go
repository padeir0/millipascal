package linearization

import (
	. "mpc/core"

	"mpc/pir"
	pirc "mpc/pir/class"
	IT "mpc/pir/instrkind"
	T "mpc/pir/types"
	RIU "mpc/pir/util"

	mod "mpc/core/module"
	lk "mpc/core/module/lexkind"
	ST "mpc/core/module/symbolkind"
	msg "mpc/messages"

	"fmt"
	"math/big"
	"strconv"
)

type context struct {
	Program   *pir.Program
	symbolMap map[string]pir.SymbolID

	PirProc *pir.Procedure
	ModProc *mod.Proc

	CurrBlock *pir.BasicBlock

	TempCounter int64
}

func newContext(M *mod.Module) *context {
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
		ID:    c.TempCounter,
	}
	c.TempCounter++
	return op
}

func (c *context) GetSymbolID(modName string, name string) pir.SymbolID {
	return c.symbolMap[modName+"_"+name]
}

func (c *context) GetSymbol(modName string, name string) *pir.Symbol {
	i, ok := c.symbolMap[modName+"_"+name]
	if !ok {
		fmt.Println(modName + "_" + name)
		fmt.Println(c.symbolMap)
		panic("name not found in symbol map")
	}
	return c.Program.Symbols[i]
}

func Generate(M *mod.Module) (*pir.Program, *Error) {
	c := newContext(M)

	M.ResetVisited()
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

func declAll(c *context, M *mod.Module) {
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
			} else if sy.T == ST.Data {
				m := newPirMem(c, M.Name, sy.Data)
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

func genAll(c *context, M *mod.Module) *Error {
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

func genProc(c *context, M *mod.Module, proc *mod.Proc) *Error {
	c.ModProc = proc
	c.PirProc = c.GetSymbol(M.Name, proc.Name).Proc

	startID, start := c.NewBlock()
	c.PirProc.Start = startID
	c.CurrBlock = start

	body := proc.N.Leaves[5]
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

func genBlock(M *mod.Module, c *context, body *mod.Node) {
	for _, code := range body.Leaves {
		switch code.Lex {
		case lk.IF:
			genIf(M, c, code)
		case lk.WHILE:
			genWhile(M, c, code)
		case lk.SET:
			genSet(M, c, code)
		case lk.RETURN:
			genReturn(M, c, code)
			return
		case lk.EXIT:
			genExit(M, c, code)
			return
		default:
			genExpr(M, c, code)
		}
	}
}

func genIf(M *mod.Module, c *context, if_ *mod.Node) {
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

func genElseIfChain(M *mod.Module, c *context, elseifchain *mod.Node, outblID pir.BlockID) {
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

func genWhile(M *mod.Module, c *context, while *mod.Node) {
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

func genReturn(M *mod.Module, c *context, return_ *mod.Node) {
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

func genExit(M *mod.Module, c *context, exit_ *mod.Node) {
	ret := exit_.Leaves[1]
	op := genExpr(M, c, ret)
	c.CurrBlock.Exit(op)
}

func genSet(M *mod.Module, c *context, set *mod.Node) {
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

func genMultiProcAssign(M *mod.Module, c *context, assignees, call *mod.Node) {
	if call.Lex != lk.CALL {
		panic("must be CALL:\n" + call.String())
	}
	// TODO: OPT: pass assignees to genCall so that no copying needs to happen
	rets := genCall(M, c, call)

	genLoadAssignRets(M, c, assignees, rets)
}

func genLoadAssignRets(M *mod.Module, c *context, assignees *mod.Node, ops []pir.Operand) {
	for i, ass := range assignees.Leaves {
		op := ops[i]
		if ass.Lex == lk.IDENTIFIER {
			genCallAssign(M, c, ass, op)
			continue
		}
		if ass.Lex == lk.AT {
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

func genArgs(M *mod.Module, c *context, args *mod.Node) []pir.Operand {
	output := []pir.Operand{}
	for _, arg := range args.Leaves {
		res := genExpr(M, c, arg)
		output = append(output, res)
	}
	return output
}

func genRets(M *mod.Module, c *context, proc pir.Operand) []pir.Operand {
	output := []pir.Operand{}
	for _, ret := range proc.Type.Proc.Rets {
		op := c.AllocTemp(ret)
		output = append(output, op)
	}
	return output
}

func genCallAssign(M *mod.Module, c *context, ass *mod.Node, op pir.Operand) {
	dest := genExprID(M, c, ass)
	// TODO: OPT: try to avoid this COPY instruction
	loadRet := RIU.Copy(op, dest)
	c.CurrBlock.AddInstr(loadRet)
}

func genCallAssignMem(M *mod.Module, c *context, ass *mod.Node, op pir.Operand) {
	ptrOp := genExpr(M, c, ass.Leaves[1])
	loadPtr := RIU.StorePtr(op, ptrOp)
	c.CurrBlock.AddInstr(loadPtr)
}

func genSingleAssign(M *mod.Module, c *context, assignee, expr *mod.Node, op lk.LexKind) {
	if assignee.Lex == lk.IDENTIFIER {
		genNormalAssign(M, c, assignee, expr, op)
		return
	}
	genDerefAssign(M, c, assignee, expr, op)
	return
}

func genNormalAssign(M *mod.Module, c *context, assignee, expr *mod.Node, op lk.LexKind) {
	dest := genExprID(M, c, assignee)
	exp := genExpr(M, c, expr)
	if op == lk.ASSIGNMENT {
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

func mapOpToInstr(l lk.LexKind) IT.InstrKind {
	switch l {
	case lk.PLUS_ASSIGN:
		return IT.Add
	case lk.MINUS_ASSIGN:
		return IT.Sub
	case lk.MULTIPLICATION_ASSIGN:
		return IT.Mult
	case lk.DIVISION_ASSIGN:
		return IT.Div
	case lk.REMAINDER_ASSIGN:
		return IT.Rem
	}
	panic(l)
}

func genDerefAssign(M *mod.Module, c *context, left, right *mod.Node, op lk.LexKind) {
	leftExpr := left.Leaves[1]
	leftType := left.Leaves[0]
	rightOp := genExpr(M, c, right)
	leftOp := genExpr(M, c, leftExpr)
	if op == lk.ASSIGNMENT {
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

func genExpr(M *mod.Module, c *context, exp *mod.Node) pir.Operand {
	if T.IsInvalid(exp.T) {
		panic("invalid type at: " + exp.Text)
	}
	switch exp.Lex {
	case lk.IDENTIFIER:
		return genExprID(M, c, exp)
	case lk.SIZEOF:
		return genSizeOfNum(M, c, exp)
	case lk.DOUBLECOLON:
		return genExternalID(c, M, exp)
	case lk.FALSE, lk.TRUE:
		return genBoolLit(M, c, exp)
	case lk.PTR_LIT, lk.I64_LIT, lk.I32_LIT, lk.I16_LIT, lk.I8_LIT,
		lk.U64_LIT, lk.U32_LIT, lk.U16_LIT, lk.U8_LIT, lk.CHAR_LIT:
		return genNumLit(M, c, exp)
	case lk.PLUS, lk.MINUS:
		return genBinaryOp(M, c, exp)
	case lk.MULTIPLICATION, lk.DIVISION, lk.REMAINDER,
		lk.EQUALS, lk.DIFFERENT,
		lk.MORE, lk.MOREEQ, lk.LESS, lk.LESSEQ,
		lk.AND, lk.OR, lk.SHIFTLEFT, lk.SHIFTRIGHT,
		lk.BITWISEAND, lk.BITWISEOR, lk.BITWISEXOR:
		return genBinaryOp(M, c, exp)
	case lk.COLON:
		return genConversion(M, c, exp)
	case lk.CALL:
		out := genCall(M, c, exp)
		if len(out) == 1 {
			return out[0]
		}
		return pir.Operand{}
	case lk.AT:
		return genDeref(M, c, exp)
	case lk.NOT, lk.NEG, lk.BITWISENOT:
		return genUnaryOp(M, c, exp)
	case lk.DOT:
		panic("unimplemented")
	}
	panic("invalid or unimplemented expression type: " + exp.Text)
}

// assume a single return
func genCall(M *mod.Module, c *context, call *mod.Node) []pir.Operand {
	proc := call.Leaves[1]
	args := call.Leaves[0]

	procOp := genExpr(M, c, proc)

	argOps := genArgs(M, c, args)
	retOps := genRets(M, c, procOp)
	genCallInstr(c, procOp, argOps, retOps)

	return retOps
}

func genDeref(M *mod.Module, c *context, memAccess *mod.Node) pir.Operand {
	t := memAccess.Leaves[0].T
	exp := memAccess.Leaves[1]

	ptrOp := genExpr(M, c, exp)

	dest := c.AllocTemp(t)
	loadPtr := RIU.LoadPtr(ptrOp, dest)
	c.CurrBlock.AddInstr(loadPtr)

	return dest
}

func genExternalID(c *context, M *mod.Module, dcolon *mod.Node) pir.Operand {
	mod := dcolon.Leaves[0].Text
	id := dcolon.Leaves[1].Text
	sy := M.GetExternalSymbol(mod, id)
	return globalToOperand(c, M, sy)
}

func genExprID(M *mod.Module, c *context, id *mod.Node) pir.Operand {
	decl, ok := c.ModProc.Vars[id.Text]
	if ok {
		return pir.Operand{
			Class: pirc.Local,
			Type:  id.T,
			ID:    int64(decl.Position),
		}
	}
	posSy, ok := c.ModProc.ArgMap[id.Text]
	if ok {
		return pir.Operand{
			Class: pirc.Arg,
			Type:  id.T,
			ID:    int64(posSy.Position),
		}
	}
	global := M.GetSymbol(id.Text)
	if global != nil {
		return globalToOperand(c, M, global)
	}
	panic("genExprID: global not found")
}

func globalToOperand(c *context, M *mod.Module, global *mod.Symbol) pir.Operand {
	i := int64(c.GetSymbolID(global.ModuleName, global.Name))
	switch global.T {
	case ST.Builtin:
		return pir.Operand{
			Class: pirc.Global,
			Type:  global.Type,
			ID:    i,
		}
	case ST.Proc:
		return pir.Operand{
			Class: pirc.Global,
			Type:  global.N.T,
			ID:    i,
		}
	case ST.Data:
		return pir.Operand{
			Class: pirc.Global,
			Type:  T.T_Ptr,
			ID:    i,
		}
	case ST.Const:
		return pir.Operand{
			Class: pirc.Lit,
			Type:  global.Type,
			ID:    -1,
			Num:   global.Const.Value,
		}
	}
	panic("wht jus heppn?")
}

func genSizeOfNum(M *mod.Module, c *context, sizeof *mod.Node) pir.Operand {
	op := sizeof.Leaves[1]
	switch op.Lex {
	case lk.IDENTIFIER:
		// TODO: if inside a struct, this identifier might be a field whithin scope
		sy := M.GetSymbol(op.Text)
		return newNumLit(sy.Data.Size, sizeof.T)
	case lk.DOUBLECOLON:
		module := op.Leaves[0].Text
		id := op.Leaves[1].Text
		sy := M.GetExternalSymbol(module, id)
		return newNumLit(sy.Data.Size, sizeof.T)
	case lk.DOT:
		panic("unimplemented")
	default:
		size := big.NewInt(int64(op.T.Size()))
		return newNumLit(size, sizeof.T)
	}
}

func newNumLit(num *big.Int, t *T.Type) pir.Operand {
	return pir.Operand{
		Class: pirc.Lit,
		Type:  t,
		ID:    -1,
		Num:   num,
	}
}

func genConversion(M *mod.Module, c *context, colon *mod.Node) pir.Operand {
	a := genExpr(M, c, colon.Leaves[1])
	dest := c.AllocTemp(colon.T)
	instr := RIU.Convert(a, dest)
	c.CurrBlock.AddInstr(instr)
	return dest
}

func genNumLit(M *mod.Module, c *context, lit *mod.Node) pir.Operand {
	return pir.Operand{
		Class: pirc.Lit,
		Type:  lit.T,
		ID:    -1,
		Num:   lit.Value,
	}
}

func genBoolLit(M *mod.Module, c *context, lit *mod.Node) pir.Operand {
	value := big.NewInt(0)
	if lit.Lex == lk.TRUE {
		value.SetInt64(1)
	}
	return pir.Operand{
		Class: pirc.Lit,
		Type:  lit.T,
		ID:    -1,
		Num:   value,
	}
}

func genBinaryOp(M *mod.Module, c *context, op *mod.Node) pir.Operand {
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

func lexToBinaryOp(op lk.LexKind) IT.InstrKind {
	switch op {
	case lk.MINUS:
		return IT.Sub
	case lk.PLUS:
		return IT.Add
	case lk.MULTIPLICATION:
		return IT.Mult
	case lk.DIVISION:
		return IT.Div
	case lk.REMAINDER:
		return IT.Rem
	case lk.EQUALS:
		return IT.Eq
	case lk.DIFFERENT:
		return IT.Diff
	case lk.MORE:
		return IT.More
	case lk.MOREEQ:
		return IT.MoreEq
	case lk.LESS:
		return IT.Less
	case lk.LESSEQ:
		return IT.LessEq
	case lk.AND:
		return IT.And
	case lk.OR:
		return IT.Or
	case lk.SHIFTLEFT:
		return IT.ShiftLeft
	case lk.SHIFTRIGHT:
		return IT.ShiftRight
	case lk.BITWISEAND:
		return IT.And
	case lk.BITWISEOR:
		return IT.Or
	case lk.BITWISEXOR:
		return IT.Xor
	}
	panic("lexToBinaryOp: unexpected binOp: " + lk.FmtTypes(op))
}

func genUnaryOp(M *mod.Module, c *context, op *mod.Node) pir.Operand {
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

func lexToUnaryOp(op lk.LexKind) IT.InstrKind {
	switch op {
	case lk.NEG:
		return IT.Neg
	case lk.NOT:
		return IT.Not
	case lk.BITWISENOT:
		return IT.Not
	}
	panic("lexToUnaryOp: unexpected unaryOp")
}

func newBuiltin(sy *mod.Symbol) *pir.Procedure {
	rets := sy.Type.Proc.Rets
	args := sy.Type.Proc.Args
	return &pir.Procedure{
		Label: "_" + sy.Name,
		Vars:  nil,
		Rets:  rets,
		Args:  args,
	}
}

func newPirProc(modName string, P *mod.Proc) *pir.Procedure {
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

func newPirMem(c *context, modName string, dt *mod.Data) *pir.DataDecl {
	return &pir.DataDecl{
		Label:    modName + "_" + dt.Name,
		Size:     dt.Size,
		Data:     dt.Contents,
		DataSize: dt.DataType.Size(),
		Nums:     dt.Nums,
		Symbols:  mapSymbols(c, dt.Symbols),
	}
}

func mapSymbols(c *context, symbols []*mod.Symbol) []pir.SymbolID {
	out := make([]pir.SymbolID, len(symbols))
	for i, sy := range symbols {
		mod := sy.ModuleName
		name := sy.Name
		id := c.GetSymbolID(mod, name)
		out[i] = id
	}
	return out
}
