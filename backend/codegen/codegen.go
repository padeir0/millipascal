package codegen

import (
	"mpc/frontend/ast"
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

func Generate(M *ast.Module) string {
	return genHeader() +
		genReadableRegion(M) + "\n\n\n" +
		genWritableRegion(M) + "\n\n\n" +
		genExecutableRegion(M)
}

func genHeader() string {
	return "format ELF64 executable 3\n"
}

func genReadableRegion(M *ast.Module) string {
	output := "segment readable\n"
	for _, sy := range M.Globals {
		if sy.T == ST.Const {
			//output += genConst(M, sy)
		}
	}
	return output
}

func genWritableRegion(M *ast.Module) string {
	output := "segment readable writeable\n"
	for _, sy := range M.Globals {
		if sy.T == ST.Mem{
			//output += genMem(M, sy)
		}
	}
	return output
}

func genExecutableRegion(M *ast.Module) string {
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
	P *ast.Proc
	M *ast.Module
}

func genProc(M *ast.Module, P *ast.Proc) string {
	ps := &procState{P: P, M: M}
	output := genProcHeader(M, P)
	output += genLocalEnv(ps)

	ps.VarAddr = genVars(P)

	bbs := ast.FlattenGraph(P.Code)
	for _, bb := range bbs {
		output += genBlock(ps, bb)
	}

	return output
}

func genProcHeader(M *ast.Module, P *ast.Proc) string {
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

func genVars(P *ast.Proc) map[string]string {
	output := map[string]string{}
	offset := 0
	for _, v := range P.Vars {
		output[v.Name] = genVarAddr(v, offset)
		offset += 8
	}
	return output
}

func genVarAddr(d *ast.Decl, offset int) string {
	return typeToAsm(d.Type) + " [rbp - " + strconv.Itoa(offset) + "]"
}

func typeToAsm(t T.Type) string {
	switch t {
	case T.Byte:  return "byte"
	case T.Word:  return "word"
	case T.DWord: return "dword"
	case T.QWord: return "qword"
	}
	panic("invalid type: "+ t.String())
}

func genBlock(ps *procState, bb *ast.BasicBlock) string {
	output := bb.Label + "\n"
	for _, instr := range bb.Code {
		output += genInstr(ps, instr)
	}
	//output += genJump(ps, bb)
	return output
}

func genInstr(ps *procState, instr *ast.Instr) string {
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
	case IT.Diff:
	case IT.Less:
	case IT.More:
	case IT.LessEq:
	case IT.MoreEq:

	case IT.Or:
	case IT.And:
	case IT.Not:

	case IT.Convert:
	case IT.Call:

	case IT.LoadLocal:
	case IT.StoreLocal:
	case IT.LoadMem:
	case IT.StoreMem:
	case IT.LoadSpill:
	case IT.StoreSpill:

	case IT.BoundsCheck:

	case IT.PushRet:
	case IT.PopRet:
	case IT.PushArg:
	case IT.PopArg:
	}
	return "UNIMPLEMENTED\n"
}

func genMov(ps *procState, dest, source *ast.Operand) string {
	return "\tmov\t" + genOperand(ps, dest) + ", " + genOperand(ps, source) + "\n"
}

func genBinAsmInstr(ps *procState, instr string, dest, source *ast.Operand) string {
	return "\t" + instr + "\t" + genRegister(dest) + ", " + genRegOrLit(source) + "\n"
}

func genUnaryAsmInstr(ps *procState, instr string, op *ast.Operand) string {
	return "\t" + instr + "\t" + genRegister(op) + "\n"
}

func genRegOrLit(op *ast.Operand) string {
	switch op.T {
	case OT.Lit:
		return op.Label
	case OT.Register:
		return genRegister(op)
	}
	panic("genRegOrLit: invalid operand type")
}

func genOperand(ps *procState, op *ast.Operand) string {
	switch op.T {
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

func genRegister(op *ast.Operand) string {
	if op.T != OT.Register {
		panic("genRegister: expected register operand")
	}
	r := X64Registers[op.Num]
	switch op.Type {
	case T.Byte:
		return r.Byte
	case T.Word:
		return r.Word
	case T.DWord:
		return r.DWord
	case T.QWord:
		return r.QWord
	}
	panic("genRegister: invalid type")
}

func genSpill(ps *procState, op *ast.Operand) string {
	baseOffset := strconv.Itoa(ps.SpillBaseOffset)
	spillOffset := strconv.Itoa(op.Num * 8)
	return typeToAsm(op.Type) +" [rbp -" + baseOffset + "-" + spillOffset + "]"
}

func genArith(ps *procState, instr *ast.Instr, asmInstr string) string {
	a, b := convertToTwoAddr(instr)
	if a == nil || b == nil {
		return genThreeAddrArith(ps, instr, asmInstr)
	}
	return genBinAsmInstr(ps, asmInstr, a, b)
}

func genThreeAddrArith(ps *procState, instr *ast.Instr, asmInstr string) string {
	a := instr.Operands[0]
	b := instr.Operands[1]
	c := instr.Destination
	output := genMov(ps, c, a)
	output += genBinAsmInstr(ps, asmInstr, c, b)
	return output
}

func genDiv(ps *procState, instr *ast.Instr) string {
	a := instr.Operands[0]
	b := instr.Operands[1]
	c := instr.Destination

	output := "	xor	rdx, rdx\n"
	output += genMovToReg(ps, RAX, a)
	output += genUnaryAsmInstr(ps, "idiv", b)
	output += genMovFromReg(ps, c, RAX)
	return output
}

func genRem(ps *procState, instr *ast.Instr) string {
	a := instr.Operands[0]
	b := instr.Operands[1]
	c := instr.Destination

	output := "	xor	rdx, rdx\n"
	output += genMovToReg(ps, RAX, a)
	output += genUnaryAsmInstr(ps, "idiv", b)
	output += genMovFromReg(ps, c, RDX)
	return output
}

func genMovToReg(ps *procState, r *register, op *ast.Operand) string {
	return "\tmov\t" + getRegAsm(r, op.Type) + ", " + genOperand(ps, op) + "\n"
}

func genMovFromReg(ps *procState, op *ast.Operand, r *register) string {
	return "\tmov\t" + genOperand(ps, op) + ", " + getRegAsm(r, op.Type) + "\n"
}

func getRegAsm(r *register, t T.Type) string {
	switch t {
	case T.Byte:
		return r.Byte
	case T.Word:
		return r.Word
	case T.DWord:
		return r.DWord
	case T.QWord:
		return r.QWord
	}
	panic("getRegAsm: invalid type")
}

func convertToTwoAddr(instr *ast.Instr) (dest *ast.Operand, op *ast.Operand) {
	a := instr.Operands[0]
	b := instr.Operands[1]
	c := instr.Destination

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

func areOpEqual(a, b *ast.Operand) bool {
	return a.T == b.T && a.Num == b.Num
}
