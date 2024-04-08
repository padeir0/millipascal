package linearization

import (
	. "mpc/core"
	T "mpc/core/types"

	"mpc/core/pir"
	pirc "mpc/core/pir/class"
	IK "mpc/core/pir/instrkind"
	RIU "mpc/core/pir/util"

	mod "mpc/core/module"
	GK "mpc/core/module/globalkind"
	LK "mpc/core/module/lexkind"
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
			if sy.Kind == GK.Proc {
				p := newPirProc(M.Name, sy.Proc)
				i := c.Program.AddProc(p)
				c.symbolMap[p.Label] = pir.SymbolID(i)
			} else if sy.Kind == GK.Data {
				m := newPirMem(c, M.Name, sy.Data)
				i := c.Program.AddMem(m)
				c.symbolMap[m.Label] = pir.SymbolID(i)
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
			if sy.Kind == GK.Proc {
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

	body := proc.N.Leaves[4]
	if body.Lex == LK.ASM {
		if proc.Asm == nil {
			panic("empty asm procedure")
		}
		c.PirProc.Asm = proc.Asm
	} else {
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
	}
	return nil
}

func setReturn(b *pir.BasicBlock) {
	b.Return([]pir.Operand{})
}

func genBlock(M *mod.Module, c *context, body *mod.Node) {
	for _, code := range body.Leaves {
		switch code.Lex {
		case LK.IF:
			genIf(M, c, code)
		case LK.WHILE:
			genWhile(M, c, code)
		case LK.DO:
			genDoWhile(M, c, code)
		case LK.SET:
			genSet(M, c, code)
		case LK.RETURN:
			genReturn(M, c, code)
			return
		case LK.EXIT:
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

func genDoWhile(M *mod.Module, c *context, while *mod.Node) {
	loop_bodyID, loop_body := c.NewBlock()
	loop_endID, loop_end := c.NewBlock()

	c.CurrBlock.Jmp(loop_bodyID)
	c.CurrBlock = loop_body
	genBlock(M, c, while.Leaves[0])

	op := genExpr(M, c, while.Leaves[1])
	c.CurrBlock.Branch(op, loop_bodyID, loop_endID)

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

	switch op.Lex {
	case LK.PLUS_PLUS, LK.MINUS_MINUS:
		genIncDec(M, c, assignees.Leaves[0], op)
		return
	case LK.SWAP:
		genSwap(M, c, assignees, expr)
		return
	case LK.ASSIGNMENT:
		if len(assignees.Leaves) > 1 {
			genMultiProcAssign(M, c, assignees, expr)
			return
		}
		genNormalSingleAssign(M, c, assignees.Leaves[0], expr, op.Lex)
		return
	case LK.PLUS_ASSIGN, LK.MINUS_ASSIGN,
		LK.MULTIPLICATION_ASSIGN, LK.DIVISION_ASSIGN,
		LK.REMAINDER_ASSIGN:
		genOpSingleAssign(M, c, assignees.Leaves[0], expr, op.Lex)
		return
	}
	panic("unreachable 329")
}

func genMultiProcAssign(M *mod.Module, c *context, assignees, call *mod.Node) {
	if call.Lex != LK.CALL {
		panic("must be CALL:\n" + call.String())
	}
	// TODO: OPT: pass assignees to genCall so that no copying needs to happen
	rets := genCall(M, c, call)

	genLoadAssignRets(M, c, assignees, rets)
}

func genLoadAssignRets(M *mod.Module, c *context, assignees *mod.Node, ops []pir.Operand) {
	for i, ass := range assignees.Leaves {
		op := ops[i]
		dest, direct := genLValue(M, c, ass)
		if direct {
			copyRet := RIU.Copy(op, dest)
			c.CurrBlock.AddInstr(copyRet)
		} else {
			loadPtr := RIU.StorePtr(op, dest)
			c.CurrBlock.AddInstr(loadPtr)
		}
	}
}

func genCallInstr(c *context, proc pir.Operand, args, rets []pir.Operand) {
	operands := []pir.Operand{proc}
	operands = append(operands, args...)
	iCall := pir.Instr{
		T:           IK.Call,
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

func genCallDerefAssign(M *mod.Module, c *context, ass *mod.Node, op pir.Operand) {
}

func genCallArrowAssign(M *mod.Module, c *context, ass *mod.Node, op pir.Operand) {
}

func genIncDec(M *mod.Module, c *context, ass, op *mod.Node) {
	instrKind := incDecInstr(op)
	size := incDecSize(ass)
	a, direct := genLValue(M, c, ass)
	if direct {
		instr := RIU.Bin(instrKind, a, size, a)
		c.CurrBlock.AddInstr(instr)
	} else {
		genLoadOpStore(M, c, instrKind, a, size)
	}
}

var one = big.NewInt(1)

func incDecSize(ass *mod.Node) pir.Operand {
	if T.IsStruct(ass.Type) {
		return newNumLit(ass.Type.Sizeof(), T.T_I32)
	}
	return newNumLit(one, ass.Type)
}

func incDecInstr(op *mod.Node) IK.InstrKind {
	switch op.Lex {
	case LK.PLUS_PLUS:
		return IK.Add
	case LK.MINUS_MINUS:
		return IK.Sub
	}
	panic("unreachable 419")
}

func genSwap(M *mod.Module, c *context, assignees, expr *mod.Node) {
	LHS := assignees.Leaves[0]
	RHS := expr
	lhs, lhsDirect := genLValue(M, c, LHS)
	rhs, rhsDirect := genLValue(M, c, RHS)

	if lhsDirect && rhsDirect {
		// copy rhs -> t1
		// copy lhs -> rhs
		// copy t1 -> lhs
		t1 := c.AllocTemp(RHS.Type)
		c.CurrBlock.AddInstr(RIU.Copy(rhs, t1))
		c.CurrBlock.AddInstr(RIU.Copy(lhs, rhs))
		c.CurrBlock.AddInstr(RIU.Copy(t1, lhs))
	} else if lhsDirect && !rhsDirect {
		// load rhs -> t1
		// store lhs, rhs
		// copy t1 -> lhs
		t1 := c.AllocTemp(RHS.Type)
		c.CurrBlock.AddInstr(RIU.LoadPtr(rhs, t1))
		c.CurrBlock.AddInstr(RIU.StorePtr(lhs, rhs))
		c.CurrBlock.AddInstr(RIU.Copy(t1, lhs))
	} else if !lhsDirect && rhsDirect {
		// load lhs -> t1
		// store rhs, lhs
		// copy t1 -> rhs
		t1 := c.AllocTemp(LHS.Type)
		c.CurrBlock.AddInstr(RIU.LoadPtr(lhs, t1))
		c.CurrBlock.AddInstr(RIU.StorePtr(rhs, lhs))
		c.CurrBlock.AddInstr(RIU.Copy(t1, rhs))
	} else { // both indirect
		// load lhs -> t1
		// load rhs -> t2
		// store t2, lhs
		// store t1, rhs
		t1 := c.AllocTemp(LHS.Type)
		t2 := c.AllocTemp(RHS.Type)
		c.CurrBlock.AddInstr(RIU.LoadPtr(lhs, t1))
		c.CurrBlock.AddInstr(RIU.LoadPtr(rhs, t2))
		c.CurrBlock.AddInstr(RIU.StorePtr(t2, lhs))
		c.CurrBlock.AddInstr(RIU.StorePtr(t1, rhs))
	}
}

// returns Operand and whether that operand
// only points to a memory location (ie, is an lvalue)
// or is directly assignable
// basically, if it returns (op, true) we can just "copy value -> op"
// but if it returns (op, false), we need to "storeptr value, op"
func genLValue(M *mod.Module, c *context, left *mod.Node) (pir.Operand, bool) {
	switch left.Lex {
	case LK.IDENTIFIER: // guaranteed to be a local
		return genExprID(M, c, left), true
	case LK.AT:
		// we take the inner expression inside the deref
		leftExpr := left.Leaves[1]
		return genExpr(M, c, leftExpr), false
	case LK.ARROW:
		leftExpr := left.Leaves[1] // we take the inner expression inside the deref
		leftOp := genExpr(M, c, leftExpr)
		field := left.Leaves[0].Text
		return genOffset(M, c, leftOp, field), false
	default:
		fmt.Println("\n", left)
		panic("unreachable 486")
	}
}

func genNormalSingleAssign(M *mod.Module, c *context, assignee, expr *mod.Node, op LK.LexKind) {
	RHS := genExpr(M, c, expr)
	LHS, direct := genLValue(M, c, assignee)
	if direct {
		cp := RIU.Copy(RHS, LHS)
		c.CurrBlock.AddInstr(cp)
		return
	} else {
		store := RIU.StorePtr(RHS, LHS)
		c.CurrBlock.AddInstr(store)
		return
	}
}

// order of evaluation is RIGHT then LEFT
func genOpSingleAssign(M *mod.Module, c *context, assignee, expr *mod.Node, op LK.LexKind) {
	RHS := genExpr(M, c, expr)
	instrT := mapOpToInstr(op)
	LHS, direct := genLValue(M, c, assignee)
	if direct {
		instr := RIU.Bin(instrT, LHS, RHS, LHS)
		c.CurrBlock.AddInstr(instr)
		return
	} else {
		genLoadOpStore(M, c, instrT, LHS, RHS)
		return
	}
}

func mapOpToInstr(l LK.LexKind) IK.InstrKind {
	switch l {
	case LK.PLUS_ASSIGN:
		return IK.Add
	case LK.MINUS_ASSIGN:
		return IK.Sub
	case LK.MULTIPLICATION_ASSIGN:
		return IK.Mult
	case LK.DIVISION_ASSIGN:
		return IK.Div
	case LK.REMAINDER_ASSIGN:
		return IK.Rem
	}
	panic(l)
}

func genLoadOpStore(M *mod.Module, c *context, kind IK.InstrKind, target, rightop pir.Operand) {
	// load target -> t1;
	// add t1, size -> t2;
	// store t2 -> target;
	t1 := c.AllocTemp(rightop.Type)
	loadI := RIU.LoadPtr(target, t1)
	c.CurrBlock.AddInstr(loadI)

	t2 := c.AllocTemp(t1.Type)
	addI := RIU.Bin(kind, t1, rightop, t2)
	c.CurrBlock.AddInstr(addI)

	storeI := RIU.StorePtr(t2, target)
	c.CurrBlock.AddInstr(storeI)
}

func genExpr(M *mod.Module, c *context, exp *mod.Node) pir.Operand {
	if T.IsInvalid(exp.Type) {
		panic("invalid type at: " + exp.Text)
	}
	switch exp.Lex {
	case LK.IDENTIFIER:
		return genExprID(M, c, exp)
	case LK.SIZEOF:
		return genSizeOfNum(M, c, exp)
	case LK.DOUBLECOLON:
		return genExternalID(c, M, exp)
	case LK.FALSE, LK.TRUE:
		return genBoolLit(M, c, exp)
	case LK.PTR_LIT, LK.I64_LIT, LK.I32_LIT, LK.I16_LIT, LK.I8_LIT,
		LK.U64_LIT, LK.U32_LIT, LK.U16_LIT, LK.U8_LIT, LK.CHAR_LIT:
		return genNumLit(exp)
	case LK.MULTIPLICATION, LK.DIVISION, LK.REMAINDER,
		LK.SHIFTLEFT, LK.SHIFTRIGHT,
		LK.BITWISEAND, LK.BITWISEOR, LK.BITWISEXOR, LK.PLUS, LK.MINUS:
		return genArithOp(M, c, exp)
	case LK.AND, LK.OR:
		return genLogicalOp(M, c, exp)
	case LK.EQUALS, LK.DIFFERENT,
		LK.MORE, LK.MOREEQ, LK.LESS, LK.LESSEQ:
		return genCompOp(M, c, exp)
	case LK.COLON:
		return genConversion(M, c, exp)
	case LK.CALL:
		callee := exp.Leaves[1]
		if T.IsStruct(callee.Type) {
			return genIndexing(M, c, exp)
		} else if T.IsProc(callee.Type) {
			out := genCall(M, c, exp)
			if len(out) == 1 {
				return out[0]
			}
			return pir.Operand{}
		} else {
			panic("unreachable 589")
		}
	case LK.AT:
		return genDeref(M, c, exp)
	case LK.NOT, LK.NEG, LK.BITWISENOT:
		return genUnaryOp(M, c, exp)
	case LK.DOT:
		return genDotAccess(M, c, exp)
	case LK.ARROW:
		return genArrowAccess(M, c, exp)
	}
	panic("invalid or unimplemented expression type: " + exp.Text)
}

// when inside expressions, assume a single return
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
	t := memAccess.Leaves[0].Type
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
			Class: pirc.Variable,
			Type:  id.Type,
			ID:    int64(decl.Position),
		}
	}
	posSy, ok := c.ModProc.ArgMap[id.Text]
	if ok {
		return pir.Operand{
			Class: pirc.Arg,
			Type:  id.Type,
			ID:    int64(posSy.Position),
		}
	}
	global := M.GetSymbol(id.Text)
	if global != nil {
		return globalToOperand(c, M, global)
	}
	panic("genExprID: global not found")
}

func globalToOperand(c *context, M *mod.Module, global *mod.Global) pir.Operand {
	i := int64(c.GetSymbolID(global.ModuleName, global.Name))
	switch global.Kind {
	case GK.Proc:
		return pir.Operand{
			Class: pirc.Global,
			Type:  global.GetType(),
			ID:    i,
		}
	case GK.Data:
		return pir.Operand{
			Class: pirc.Global,
			Type:  global.GetType(),
			ID:    i,
		}
	case GK.Const:
		return pir.Operand{
			Class: pirc.Lit,
			Type:  global.GetType(),
			ID:    -1,
			Num:   global.Const.Value,
		}
	}
	panic("wht jus heppn?")
}

func genSizeOfNum(M *mod.Module, c *context, sizeof *mod.Node) pir.Operand {
	if sizeof.Type == nil {
		panic("size was nil")
	}
	op := sizeof.Leaves[0]
	dot := sizeof.Leaves[1]
	if dot != nil {
		var sy *mod.Global
		switch op.Lex {
		case LK.IDENTIFIER: // struct
			sy = M.GetSymbol(op.Text)
		case LK.DOUBLECOLON: // external struct
			mod := op.Leaves[0].Text
			id := op.Leaves[1].Text
			sy = M.GetExternalSymbol(mod, id)
		}
		t := sy.Struct.Type
		fieldName := dot.Leaves[0].Text
		field, ok := t.Struct.Field(fieldName)
		if !ok {
			panic("impossible 586")
		}
		size := int64(field.Type.Size()) // we don't look at struct sizes here
		return newNumLit(big.NewInt(size), sizeof.Type)
	}
	var sy *mod.Global
	switch op.Lex {
	case LK.IDENTIFIER:
		sy = M.GetSymbol(op.Text)
	case LK.DOUBLECOLON:
		module := op.Leaves[0].Text
		id := op.Leaves[1].Text
		sy = M.GetExternalSymbol(module, id)
	default:
		if op.Type.Sizeof() == nil {
			panic("type size was nil")
		}
		return newNumLit(op.Type.Sizeof(), sizeof.Type)
	}
	switch sy.Kind {
	case GK.Data:
		if sy.Data.Size == nil {
			panic("data size was nil")
		}
		return newNumLit(sy.Data.Size, sizeof.Type)
	case GK.Struct:
		if sy.Struct.Type.Sizeof() == nil {
			panic("struct size was nil")
		}
		return newNumLit(sy.Struct.Type.Sizeof(), sizeof.Type)
	default:
		panic("unreachable 738")
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
	dest := c.AllocTemp(colon.Type)
	instr := RIU.Convert(a, dest)
	c.CurrBlock.AddInstr(instr)
	return dest
}

func genNumLit(lit *mod.Node) pir.Operand {
	return pir.Operand{
		Class: pirc.Lit,
		Type:  lit.Type,
		ID:    -1,
		Num:   lit.Value,
	}
}

func genBoolLit(M *mod.Module, c *context, lit *mod.Node) pir.Operand {
	value := big.NewInt(0)
	if lit.Lex == LK.TRUE {
		value.SetInt64(1)
	}
	return pir.Operand{
		Class: pirc.Lit,
		Type:  lit.Type,
		ID:    -1,
		Num:   value,
	}
}

func genArithOp(M *mod.Module, c *context, op *mod.Node) pir.Operand {
	it := lexToBinaryOp(op.Lex)
	a := genExpr(M, c, op.Leaves[0])
	b := genExpr(M, c, op.Leaves[1])
	dest := c.AllocTemp(op.Type)
	instr := RIU.Bin(it, a, b, dest)
	c.CurrBlock.AddInstr(instr)
	return dest
}

func genCompOp(M *mod.Module, c *context, op *mod.Node) pir.Operand {
	it := lexToBinaryOp(op.Lex)
	a := genExpr(M, c, op.Leaves[0])
	b := genExpr(M, c, op.Leaves[1])
	dest := c.AllocTemp(op.Type)
	instr := RIU.BinOut(it, a, b, dest)
	c.CurrBlock.AddInstr(instr)
	return dest
}

func genLogicalOp(M *mod.Module, c *context, op *mod.Node) pir.Operand {
	it := lexToBinaryOp(op.Lex)
	a := genExpr(M, c, op.Leaves[0])
	b := genExpr(M, c, op.Leaves[1])
	dest := c.AllocTemp(op.Type)
	instr := RIU.Bin(it, a, b, dest)
	c.CurrBlock.AddInstr(instr)
	return dest
}

// (p+i*sizeof[STRUCT])
func genIndexing(M *mod.Module, c *context, op *mod.Node) pir.Operand {
	callee := op.Leaves[1]
	index := op.Leaves[0].Leaves[0] // should be ok
	a := genExpr(M, c, callee)

	iOp := genExpr(M, c, index)
	size := newNumLit(callee.Type.Sizeof(), index.Type)

	dest1 := c.AllocTemp(index.Type)
	mult := RIU.Bin(IK.Mult, iOp, size, dest1)
	c.CurrBlock.AddInstr(mult)

	dest2 := c.AllocTemp(op.Type)
	instr := RIU.Bin(IK.Add, a, dest1, dest2)
	c.CurrBlock.AddInstr(instr)
	return dest2
}

// (p+STRUCT.FIELD)
func genDotAccess(M *mod.Module, c *context, op *mod.Node) pir.Operand {
	obj := op.Leaves[1]
	id := op.Leaves[0].Text

	var sy *mod.Global
	switch obj.Lex {
	case LK.IDENTIFIER: // data declaration
		sy = M.GetSymbol(obj.Text)
	case LK.DOUBLECOLON: // external data declaration
		modName := obj.Leaves[0].Text
		symName := obj.Leaves[1].Text
		sy = M.GetExternalSymbol(modName, symName)
	default:
		a := genExpr(M, c, obj)
		return genOffset(M, c, a, id)
	}

	if sy.Kind == GK.Struct {
		t := sy.Struct.Type
		field, ok := t.Struct.Field(id)
		if !ok {
			panic("should be safe 852")
		}
		return newNumLit(field.Offset, T.T_I32)
	} else { // data
		a := genExpr(M, c, obj)
		return genOffset(M, c, a, id)
	}
}

// (p+STRUCT.FIELD)@FIELDTYPE
func genArrowAccess(M *mod.Module, c *context, op *mod.Node) pir.Operand {
	obj := op.Leaves[1]
	id := op.Leaves[0].Text
	a := genExpr(M, c, obj)

	fieldOffset := genOffset(M, c, a, id)
	field, _ := a.Type.Struct.Field(id)

	dest2 := c.AllocTemp(field.Type)
	loadPtr := RIU.LoadPtr(fieldOffset, dest2)
	c.CurrBlock.AddInstr(loadPtr)

	return dest2
}

func genOffset(M *mod.Module, c *context, op pir.Operand, fieldID string) pir.Operand {
	if !T.IsStruct(op.Type) {
		fmt.Println(op.Type)
		panic("should be struct!!123")
	}
	field, ok := op.Type.Struct.Field(fieldID)
	if !ok {
		panic("should be safe!!2!")
	}
	b := newNumLit(field.Offset, T.T_I32)

	dest := c.AllocTemp(T.T_Ptr)
	instr := RIU.Bin(IK.Add, op, b, dest)
	c.CurrBlock.AddInstr(instr)
	return dest
}

func lexToBinaryOp(op LK.LexKind) IK.InstrKind {
	switch op {
	case LK.MINUS:
		return IK.Sub
	case LK.PLUS:
		return IK.Add
	case LK.MULTIPLICATION:
		return IK.Mult
	case LK.DIVISION:
		return IK.Div
	case LK.REMAINDER:
		return IK.Rem
	case LK.EQUALS:
		return IK.Eq
	case LK.DIFFERENT:
		return IK.Diff
	case LK.MORE:
		return IK.More
	case LK.MOREEQ:
		return IK.MoreEq
	case LK.LESS:
		return IK.Less
	case LK.LESSEQ:
		return IK.LessEq
	case LK.AND:
		return IK.And
	case LK.OR:
		return IK.Or
	case LK.SHIFTLEFT:
		return IK.ShiftLeft
	case LK.SHIFTRIGHT:
		return IK.ShiftRight
	case LK.BITWISEAND:
		return IK.And
	case LK.BITWISEOR:
		return IK.Or
	case LK.BITWISEXOR:
		return IK.Xor
	}
	panic("lexToBinaryOp: unexpected binOp: " + LK.FmtTypes(op))
}

func genUnaryOp(M *mod.Module, c *context, op *mod.Node) pir.Operand {
	it := lexToUnaryOp(op.Lex)
	a := genExpr(M, c, op.Leaves[0])
	dest := c.AllocTemp(op.Type)
	instr := RIU.Un(it, a, dest)
	c.CurrBlock.AddInstr(instr)
	return dest
}

func lexToUnaryOp(op LK.LexKind) IK.InstrKind {
	switch op {
	case LK.NEG:
		return IK.Neg
	case LK.NOT:
		return IK.Not
	case LK.BITWISENOT:
		return IK.Not
	}
	panic("lexToUnaryOp: unexpected unaryOp")
}

func newPirProc(modName string, P *mod.Proc) *pir.Procedure {
	args := []*T.Type{}
	for _, arg := range P.Args {
		args = append(args, arg.T)
	}
	vars := make([]*T.Type, len(P.Vars))
	for _, ps := range P.Vars {
		vars[ps.Position] = ps.Local.T
	}
	return &pir.Procedure{
		Label: modName + "_" + P.Name,
		CC:    P.Type.Proc.CC,
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
		DataSize: dt.Type.Size(),
		Nums:     dt.Nums,
	}
}

func mapSymbols(c *context, symbols []*mod.Global) []pir.SymbolID {
	out := make([]pir.SymbolID, len(symbols))
	for i, sy := range symbols {
		mod := sy.ModuleName
		name := sy.Name
		id := c.GetSymbolID(mod, name)
		out[i] = id
	}
	return out
}
