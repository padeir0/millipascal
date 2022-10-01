package codegen

/*
import (
	"mpc/frontend/ir"
	T "mpc/frontend/enums/Type"
	ST "mpc/frontend/enums/symbolType"
	OT "mpc/frontend/enums/operandType"
	IT "mpc/frontend/enums/instrType"

	"strconv"
)

// if operand is
// 	Stack    -> use Num as offset after vars
//	Register -> use label as register
//	Local    -> use label for load/store
//	Mem      -> use label for load/store
//
// Assume RDX and RAX are never used as general purpose

type register struct {
	QWord string
	DWord string
	Word  string
	Byte  string
}

// we use this two as scratch space, IDIV already needs these, so no matter
var RAX = &register{QWord: "rax", DWord: "eax", Word: "ax", Byte: "al"}
var RDX = &register{QWord: "rdx", DWord: "edx", Word: "dx", Byte: "dl"}

var X64Registers = []*register{
	{QWord: "r15", DWord: "r15d", Word: "r15w", Byte: "r15b"},
	{QWord: "r14", DWord: "r14d", Word: "r14w", Byte: "r14b"},
	{QWord: "r13", DWord: "r13d", Word: "r13w", Byte: "r13b"},
	{QWord: "r12", DWord: "r12d", Word: "r12w", Byte: "r12b"},
	{QWord: "r11", DWord: "r11d", Word: "r11w", Byte: "r11b"},
	{QWord: "r10", DWord: "r10d", Word: "r10w", Byte: "r10b"},
	{QWord: "r9", DWord: "r9d", Word: "r9w", Byte: "r9b"},
	{QWord: "r8", DWord: "r8d", Word: "r8w", Byte: "r8b"},
	{QWord: "rdi", DWord: "edi", Word: "di", Byte: "dil"},
	{QWord: "rsi", DWord: "esi", Word: "si", Byte: "sil"},
	{QWord: "rcx", DWord: "ecx", Word: "cx", Byte: "cl"},
	{QWord: "rbx", DWord: "ebx", Word: "bx", Byte: "bl"},
}

func Generate(M *ir.Module) string {
	return genHeader() +
		genReadableRegion(M) + "\n\n\n" +
		genWritableRegion(M) + "\n\n\n" +
		genExecutableRegion(M)
}

func genHeader() string {
	return "format ELF64 executable 3\n"
}

func genReadableRegion(M *ir.Module) string {
	output := "segment readable\n"
	for _, sy := range M.Globals {
		if sy.T == ST.Const {
			//output += genConst(M, sy)
		}
	}
	return output
}

func genWritableRegion(M *ir.Module) string {
	output := "segment readable writeable\n"
	for _, sy := range M.Globals {
		if sy.T == ST.Mem{
			//output += genMem(M, sy)
		}
	}
	return output
}

func genExecutableRegion(M *ir.Module) string {
	output := "segment readable executable\n"
	for _, sy := range M.Globals {
		if sy.T == ST.Proc {
			output += genProc(M, sy.Proc)
		}
	}
	return output
}

type procState struct {
	VarAddr map[string]string
	SpillBaseOffset int
	P *ir.Proc
	M *ir.Module
}

func genProc(M *ir.Module, P *ir.Proc) string {
	ps := &procState{P: P, M: M}
	output := genProcHeader(M, P)
	output += genLocalEnv(ps)

	ps.VarAddr = genVars(P)

	bbs := ir.FlattenGraph(P.Code)
	for _, bb := range bbs {
		output += genBlock(ps, bb)
	}

	return output
}

func genProcHeader(M *ir.Module, P *ir.Proc) string {
	return P.Name + `:
	push 	rbp
	mov	rbp, rsp
`
}

func genLocalEnv(ps *procState) string {
	vars := len(ps.P.Vars) * 8
	spills := ps.P.NumOfSpills * 8
	ps.SpillBaseOffset = vars

	size := strconv.Itoa(vars + spills)
	return "	sub	rsp, " + size + "\n"
}

func genVars(P *ir.Proc) map[string]string {
	output := map[string]string{}
	offset := 0
	for _, v := range P.Vars {
		output[v.Name] = genVarAddr(v, offset)
		offset += 8
	}
	return output
}

func genVarAddr(d *ir.Decl, offset int) string {
	return typeToAsm(d.Type) + " [rbp - " + strconv.Itoa(offset) + "]"
}

func typeToAsm(t T.Type) string {
	switch t {
	case T.I8:  return "byte"
	case T.I16:  return "word"
	case T.I32: return "dword"
	case T.I64: return "qword"
	}
	panic("invalid type: "+ t.String())
}

func genBlock(ps *procState, bb *ir.BasicBlock) string {
	output := bb.Label + "\n"
	for _, instr := range bb.Code {
		output += genInstr(ps, instr)
	}
	//output += genJump(ps, bb)
	return output
}

func genInstr(ps *procState, instr *ir.Instr) string {
	switch instr.T {
	case IT.Add:
		return genArith(ps, instr, "add")
	case IT.Sub:
		return genArith(ps, instr, "sub")
	case IT.Mult:
		return genArith(ps, instr, "imul")
	case IT.Div:
		return genDiv(ps, instr)
	case IT.Rem:
		return genRem(ps, instr)

	case IT.UnaryPlus:
	case IT.UnaryMinus:

	case IT.Eq:
		return genCompOp(ps, instr, "sete")
	case IT.Diff:
		return genCompOp(ps, instr, "setne")
	case IT.Less:
		return genCompOp(ps, instr, "setl")
	case IT.More:
		return genCompOp(ps, instr, "setg")
	case IT.LessEq:
		return genCompOp(ps, instr, "setle")
	case IT.MoreEq:
		return genCompOp(ps, instr, "setge")

	case IT.Or:
		return genBoolOR(ps, instr)
	case IT.And:
		return genBoolAND(ps, instr)
	case IT.Not:
		return genBoolNot(ps, instr)

	case IT.Convert:
	case IT.Call:

	case IT.LoadPtr:
	case IT.StorePtr:
	case IT.Load:
	case IT.Store:
	}
	return "UNIMPLEMENTED\n"
}

func genMov(ps *procState, dest, source *ir.Operand) string {
	return "\tmov\t" + genOperand(ps, dest) + ", " + genOperand(ps, source) + "\n"
}

func genBinAsmInstr(ps *procState, instr string, dest, source *ir.Operand) string {
	return "\t" + instr + "\t" + genRegister(dest) + ", " + genRegOrLit(source) + "\n"
}

func genUnaryAsmInstr(ps *procState, instr string, op *ir.Operand) string {
	return "\t" + instr + "\t" + genRegister(op) + "\n"
}

func genRegOrLit(op *ir.Operand) string {
	switch op.HirC {
	case OT.Lit:
		return op.Label
	case OT.Register:
		return genRegister(op)
	}
	panic("genRegOrLit: invalid operand type")
}

func genOperand(ps *procState, op *ir.Operand) string {
	switch op.HirC {
	case OT.Lit:
		return op.Label
	case OT.Local:
		addr, ok := ps.VarAddr[op.Label]
		if !ok {
			panic("not found in VarAddr: "+op.String())
		}
		return addr
	case OT.Proc:
		return op.Label
	case OT.Register:
		return genRegister(op)
	case OT.Spill:
		return genSpill(ps, op)
	}
	panic("genOperand: unimplemented OperandType")
}

func genRegister(op *ir.Operand) string {
	if op.HirC != OT.Register {
		panic("genRegister: expected register operand")
	}
	r := X64Registers[op.Num]
	switch op.Type {
	case T.I8:
		return r.Byte
	case T.I16:
		return r.Word
	case T.I32:
		return r.DWord
	case T.I64:
		return r.QWord
	}
	panic("genRegister: invalid type")
}

func genSpill(ps *procState, op *ir.Operand) string {
	baseOffset := strconv.Itoa(ps.SpillBaseOffset)
	spillOffset := strconv.Itoa(op.Num * 8)
	return typeToAsm(op.Type) +" [rbp -" + baseOffset + "-" + spillOffset + "]"
}

func genArith(ps *procState, instr *ir.Instr, asmInstr string) string {
	a, b := convertToTwoAddr(instr)
	if a == nil || b == nil {
		return genThreeAddrArith(ps, instr, asmInstr)
	}
	return genBinAsmInstr(ps, asmInstr, a, b)
}

func genThreeAddrArith(ps *procState, instr *ir.Instr, asmInstr string) string {
	a := instr.Operands[0]
	b := instr.Operands[1]
	c := instr.Destination[0]
	output := genMov(ps, c, a)
	output += genBinAsmInstr(ps, asmInstr, c, b)
	return output
}

func genDiv(ps *procState, instr *ir.Instr) string {
	a := instr.Operands[0]
	b := instr.Operands[1]
	c := instr.Destination[0]

	output := "	xor	rdx, rdx\n"
	output += genMovToReg(ps, RAX, a)
	output += genUnaryAsmInstr(ps, "idiv", b)
	output += genMovFromReg(ps, c, RAX)
	return output
}

func genRem(ps *procState, instr *ir.Instr) string {
	a := instr.Operands[0]
	b := instr.Operands[1]
	c := instr.Destination[0]

	output := "	xor	rdx, rdx\n"
	output += genMovToReg(ps, RAX, a)
	output += genUnaryAsmInstr(ps, "idiv", b)
	output += genMovFromReg(ps, c, RDX)
	return output
}

func genMovToReg(ps *procState, r *register, op *ir.Operand) string {
	return "\tmov\t" + getRegAsm(r, op.Type) + ", " + genOperand(ps, op) + "\n"
}

func genMovFromReg(ps *procState, op *ir.Operand, r *register) string {
	return "\tmov\t" + genOperand(ps, op) + ", " + getRegAsm(r, op.Type) + "\n"
}

func genCompOp(ps *procState, instr *ir.Instr, asmInstr string) string {
	a := instr.Operands[0]
	b := instr.Operands[1]
	c := instr.Destination[0]
	output := genBinAsmInstr(ps, "cmp", a, b)
	output += "\t"+asmInstr+"\t" + genRegister(c) + "\n"
	return output
}

var opZERO = &ir.Operand{
	HirC: OT.Lit,
	Label: "0",
}

func genBoolOR(ps *procState, instr *ir.Instr) string {
	aOp := instr.Operands[0]
	bOp := instr.Operands[1]
	cDest := instr.Destination[0]

	rax := getRegAsm(RAX, instr.Type)
	rdx := getRegAsm(RDX, instr.Type)
	a := genRegOrLit(aOp)
	b := genRegOrLit(bOp)
	c := genRegOrLit(cDest)

	output := "\tcmp\t" + a + ", 0\n"
	output += "\tsetne\t" + rax + "\n"
	output += "\tcmp\t" + b + ", 0\n"
	output += "\tsetne\t" + rdx + "\n"
	output += "\tadd\t" + rax + ", " + rdx + "\n"
	output += "\tcmp\t" + rax + ", 1\n"
	output += "\tsetge\t" + c + "\n"
	return output
}

func genBoolAND(ps *procState, instr *ir.Instr) string {
	aOp := instr.Operands[0]
	bOp := instr.Operands[1]
	cDest := instr.Destination[0]

	rax := getRegAsm(RAX, instr.Type)
	rdx := getRegAsm(RDX, instr.Type)
	a := genRegOrLit(aOp)
	b := genRegOrLit(bOp)
	c := genRegOrLit(cDest)

	output := "\tcmp\t" + a + ", 0\n"
	output += "\tsetne\t" + rax + "\n"
	output += "\tcmp\t" + b + ", 0\n"
	output += "\tsetne\t" + rdx + "\n"
	output += "\tadd\t" + rax + ", " + rdx + "\n"
	output += "\tcmp\t" + rax + ", 2\n"
	output += "\tsete\t" + c + "\n"
	return output
}

func genBoolNot(ps *procState, instr *ir.Instr) string {
	return "UNIMPLEMENTED"
}

func getRegAsm(r *register, t T.Type) string {
	switch t {
	case T.I8:
		return r.Byte
	case T.I16:
		return r.Word
	case T.I32:
		return r.DWord
	case T.I64:
		return r.QWord
	}
	panic("getRegAsm: invalid type")
}

func convertToTwoAddr(instr *ir.Instr) (dest *ir.Operand, op *ir.Operand) {
	a := instr.Operands[0]
	b := instr.Operands[1]
	c := instr.Destination[0]

	if c == nil {
		return nil, nil
	}

	if areOpEqual(a, c) {
		return c, b
	}
	if areOpEqual(b, c) {
		return c, a
	}

	return nil, nil
}

func areOpEqual(a, b *ir.Operand) bool {
	return a.HirC == b.HirC && a.Num == b.Num
}
*/
