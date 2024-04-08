package gen

import (
	"mpc/core/asm"
	. "mpc/core/asm/instrkind"
	. "mpc/core/asm/util"

	cc "mpc/core/cc/stack"
	T "mpc/core/types"

	"mpc/backend0/mir"
	mirc "mpc/backend0/mir/class"
	FT "mpc/backend0/mir/flowkind"
	IT "mpc/backend0/mir/instrkind"

	"fmt"
	"math/big"
)

func Generate(P *mir.Program) *asm.Program {
	output := &asm.Program{
		Writable:   []asm.Data{},
		Readonly:   []asm.Data{},
		Executable: genEntry(P),
	}

	for _, sy := range P.Symbols {
		if sy.Proc != nil {
			proc := genProc(P, sy.Proc)
			output.Executable = append(output.Executable, proc...)
		}
		if sy.Mem != nil {
			mem := genMem(sy.Mem)
			output.Writable = append(output.Writable, mem)
		}
	}
	return output
}

// for now this is ok
func genEntry(P *mir.Program) []asm.Line {
	entry := P.Symbols[P.Entry]
	if entry == nil || entry.Proc == nil {
		panic("nil entrypoint")
	}
	return []asm.Line{
		Unary(Call, LabelOp(entry.Proc.Label)),
		Bin(Xor, RDI.QWord, RDI.QWord),    // EXIT CODE 0
		Bin(Mov, RAX.QWord, ConstInt(60)), // EXIT
		Plain(Syscall),
	}
}

func genMem(mem *mir.DataDecl) asm.Data {
	if mem.Data != "" {
		return asm.Data{
			Label: mem.Label,
			Str:   mem.Data,
		}
	}
	if mem.Nums != nil {
		return asm.Data{
			Label: mem.Label,
			Blob:  mem.Nums,
		}
	}
	if mem.Size == nil {
		panic("size was nil")
	}
	if !mem.Size.IsInt64() {
		panic("not int!!!!!!!")
	}
	size := int(mem.Size.Int64())
	return asm.Data{
		Label: mem.Label,
		Size:  size,
	}
}

func genProc(P *mir.Program, proc *mir.Procedure) []asm.Line {
	if proc.Asm != nil {
		op := LabelLine(proc.Label)
		return append([]asm.Line{op}, proc.Asm...)
	}
	stackReserve := 8 * (proc.NumOfVars + proc.NumOfSpills + proc.NumOfMaxCalleeArguments)
	output := []asm.Line{
		LabelLine(proc.Label),
		Unary(Push, RBP),
		Bin(Mov, RBP, RSP),
		Bin(Sub, RSP, ConstInt(stackReserve)),
	}
	proc.ResetBlocks()
	body := genBlocks(P, proc, proc.FirstBlock())
	output = append(output, body...)
	return output
}

func genBlocks(P *mir.Program, proc *mir.Procedure, start *mir.BasicBlock) []asm.Line {
	trueBranches := []*mir.BasicBlock{}
	falseBlocks := genFalseBranches(P, proc, start, &trueBranches)
	for _, tBlock := range trueBranches {
		out := genBlocks(P, proc, tBlock)
		falseBlocks = append(falseBlocks, out...)
	}
	return falseBlocks
}

func genFalseBranches(P *mir.Program, proc *mir.Procedure, block *mir.BasicBlock, trueBranches *[]*mir.BasicBlock) []asm.Line {
	if block.Visited {
		panic("no blocks should be already visited: " + block.Label)
	}
	block.Visited = true
	fb := genCode(P, proc, block)

	// should generate Jmp only for true branches and Jmps that point to
	// already visited blocks
	switch block.Out.T {
	case FT.Jmp:
		t := proc.GetBlock(block.Out.True)
		if t.Visited {
			jmp := Unary(Jmp, LabelOp(t.Label))
			fb = append(fb, jmp)
			return fb
		}
		out := genFalseBranches(P, proc, t, trueBranches)
		out = append(fb, out...)
		return out
	case FT.If:
		t := proc.GetBlock(block.Out.True)
		if !t.Visited {
			*trueBranches = append(*trueBranches, t)
		}
		jmp := genCondJmp(P, proc, t, block.Out.V[0])
		fb = append(fb, jmp...)
		f := proc.GetBlock(block.Out.False)
		out := genFalseBranches(P, proc, f, trueBranches)
		out = append(fb, out...)
		return out
	case FT.Exit:
		exit := genExit(P, proc, block.Out.V[0])
		fb = append(fb, exit...)
		return fb
	case FT.Return:
		ret := genRet()
		fb = append(fb, ret...)
		return fb
	}
	panic("Invalid flow: " + block.Out.String())
}

func genCode(P *mir.Program, proc *mir.Procedure, block *mir.BasicBlock) []asm.Line {
	output := make([]asm.Line, len(block.Code))[:0]
	output = append(output, LabelLine(block.Label))
	for _, instr := range block.Code {
		output = append(output, genInstr(P, proc, instr)...)
	}
	return output
}

func genCondJmp(P *mir.Program, proc *mir.Procedure, block *mir.BasicBlock, op mir.Operand) []asm.Line {
	newOp := convertOperandProc(P, proc, op)
	if op.Class == mirc.Lit || op.Class == mirc.Static {
		rcx := _genReg(RCX, op.Type)
		return []asm.Line{
			Bin(Mov, rcx, newOp),
			Bin(Cmp, rcx, ConstInt(1)),
			Unary(Je, LabelOp(block.Label)),
		}
	}
	return []asm.Line{
		Bin(Cmp, newOp, ConstInt(1)),
		Unary(Je, LabelOp(block.Label)),
	}
}

func genRet() []asm.Line {
	return []asm.Line{
		Bin(Mov, RSP, RBP),
		Unary(Pop, RBP),
		Plain(Ret),
	}
}

func genExit(P *mir.Program, proc *mir.Procedure, op mir.Operand) []asm.Line {
	exitCode := convertOperandProc(P, proc, op)
	return []asm.Line{
		Bin(Xor, RDI.QWord, RDI.QWord),
		Bin(Mov, RDI.Byte, exitCode),      // EXIT CODE
		Bin(Mov, RAX.QWord, ConstInt(60)), // EXIT SYSCALL NUMBER
		Plain(Syscall),
	}
}
func genInstr(P *mir.Program, proc *mir.Procedure, instr mir.Instr) []asm.Line {
	switch instr.T {
	case IT.Call:
		return genCall(P, proc, instr)
	case IT.Load, IT.Store, IT.Copy:
		return genLoadStore(P, proc, instr)
	case IT.LoadPtr:
		return genLoadPtr(P, proc, instr)
	case IT.StorePtr:
		return genStorePtr(P, proc, instr)
	case IT.Neg:
		return genNeg(P, proc, instr)
	case IT.Not:
		return genNot(P, proc, instr)
	case IT.Eq, IT.Diff, IT.Less, IT.More, IT.LessEq, IT.MoreEq:
		return genComp(P, proc, instr)
	case IT.Or, IT.And, IT.Xor:
		return genSimpleBin(P, proc, instr)
	case IT.Div:
		return genDiv(P, proc, instr)
	case IT.Rem:
		return genRem(P, proc, instr)
	case IT.ShiftLeft, IT.ShiftRight:
		return genShift(P, proc, instr)
	case IT.Mult:
		return genMul(P, proc, instr)
	case IT.Add:
		return genAdd(P, proc, instr)
	case IT.Sub:
		return genSub(P, proc, instr)
	case IT.Convert:
		return genConvert(P, proc, instr)
	default:
		panic("unimplemented: " + instr.String())
	}
}

func genCall(P *mir.Program, proc *mir.Procedure, instr mir.Instr) []asm.Line {
	newA := convertOperandProc(P, proc, instr.A.Op())
	return []asm.Line{
		Unary(Call, newA),
	}
}

func genLoadStore(P *mir.Program, proc *mir.Procedure, instr mir.Instr) []asm.Line {
	out, newA := resolveOperand(P, proc, instr.A.Operand)
	newDest := convertOperandProc(P, proc, instr.Dest.Op())
	out = append(out, Bin(Mov, newDest, newA))
	return out
}

func genLoadPtr(P *mir.Program, proc *mir.Procedure, instr mir.Instr) []asm.Line {
	newA := convertOperandProc(P, proc, instr.A.Op())
	newDest := convertOperandProc(P, proc, instr.Dest.Op())
	a := Bin(Mov, newDest, AddrSimple(newA, TypeToTsize(instr.Type)))
	return []asm.Line{a}
}

func genStorePtr(P *mir.Program, proc *mir.Procedure, instr mir.Instr) []asm.Line {
	out, newA := resolveOperand(P, proc, instr.A.Operand)
	newDest := convertOperandProc(P, proc, instr.B.Op())
	addr := AddrSimple(newDest, TypeToTsize(instr.Type))
	a := Bin(Mov, addr, newA)
	out = append(out, a)
	return out
}

func genNeg(P *mir.Program, proc *mir.Procedure, instr mir.Instr) []asm.Line {
	out, newA := resolveOperand(P, proc, instr.A.Operand)
	newDest := convertOperandProc(P, proc, instr.Dest.Op())
	out = append(out, []asm.Line{
		Bin(Mov, newDest, newA),
		Unary(Neg, newDest),
	}...)
	return out
}

func genNot(P *mir.Program, proc *mir.Procedure, instr mir.Instr) []asm.Line {
	if instr.Type.Basic == T.Bool {
		newA := convertOperandProc(P, proc, instr.A.Op())
		newDest := convertOperandProc(P, proc, instr.Dest.Op())
		return []asm.Line{
			Bin(Cmp, newA, ConstInt(0)),
			Unary(Sete, newDest),
		}
	}
	// bitwise not :)
	out, newA := resolveOperand(P, proc, instr.A.Operand)
	newDest := convertOperandProc(P, proc, instr.Dest.Op())
	out = append(out, []asm.Line{
		Bin(Mov, newDest, newA),
		Unary(Not, newDest),
	}...)
	return out
}

func genComp(P *mir.Program, proc *mir.Procedure, instr mir.Instr) []asm.Line {
	newOp1 := convertOperandProc(P, proc, instr.A.Op())
	out, newOp2 := resolveOperand(P, proc, instr.B.Operand)
	newDest := convertOperandProc(P, proc, instr.Dest.Op())

	newInstr := genInstrName(instr)
	if instr.A.Class == mirc.Lit || instr.A.Class == mirc.Static {
		rax := _genReg(RAX, instr.A.Type)
		out = append(out, []asm.Line{
			Bin(Mov, rax, newOp1),
			Bin(Cmp, rax, newOp2),
			Unary(newInstr, newDest),
		}...)
		return out
	}
	out = append(out, []asm.Line{
		Bin(Cmp, newOp1, newOp2),
		Unary(newInstr, newDest),
	}...)
	return out
}

func genSimpleBin(P *mir.Program, proc *mir.Procedure, instr mir.Instr) []asm.Line {
	dest, op, ok := convertToTwoAddr(instr)
	newInstr := genInstrName(instr)
	if ok {
		out, newOp1 := resolveOperand(P, proc, op)
		newDest := convertOperandProc(P, proc, dest)
		out = append(out, []asm.Line{
			Bin(newInstr, newDest, newOp1),
		}...)
		return out
	}
	newOp1 := convertOperandProc(P, proc, instr.A.Op())
	// we only need to resolve it for the second one, the first is already going in a register
	out, newOp2 := resolveOperand(P, proc, instr.B.Operand)
	newDest := convertOperandProc(P, proc, instr.Dest.Op())
	out = append(out, []asm.Line{
		Bin(Mov, newDest, newOp1),
		Bin(newInstr, newDest, newOp2),
	}...)
	return out
}

func genDiv(P *mir.Program, proc *mir.Procedure, instr mir.Instr) []asm.Line {
	instrName := genInstrName(instr)
	newOp1 := convertOperandProc(P, proc, instr.A.Op())
	newOp2 := convertOperandProc(P, proc, instr.B.Op())
	newDest := convertOperandProc(P, proc, instr.Dest.Op())
	if instr.B.Class == mirc.Lit || instr.B.Class == mirc.Static {
		rcx, op := signExtendRCX(instr)
		return []asm.Line{
			Bin(Xor, RDX.QWord, RDX.QWord),
			Bin(Mov, _genReg(RAX, instr.Type), newOp1),
			Bin(Mov, _genReg(RCX, instr.Type), newOp2),
			Plain(op),
			Unary(instrName, rcx),
			Bin(Mov, newDest, _genReg(RAX, instr.Type)),
		}
	}
	newOp2, op := signExtend(P, proc, instr, instr.B)
	return []asm.Line{
		Bin(Xor, RDX.QWord, RDX.QWord),
		Bin(Mov, _genReg(RAX, instr.Type), newOp1),
		Plain(op),
		Unary(instrName, newOp2),
		Bin(Mov, newDest, _genReg(RAX, instr.Type)),
	}
}

func genRem(P *mir.Program, proc *mir.Procedure, instr mir.Instr) []asm.Line {
	instrName := genInstrName(instr)

	newOp1 := convertOperandProc(P, proc, instr.A.Op())
	newOp2 := convertOperandProc(P, proc, instr.B.Op())
	newDest := convertOperandProc(P, proc, instr.Dest.Op())
	if mirc.IsImmediate(instr.B.Class) {
		rcx, op := signExtendRCX(instr)
		return []asm.Line{
			Bin(Xor, RDX.QWord, RDX.QWord),
			Bin(Mov, _genReg(RAX, instr.Type), newOp1),
			Bin(Mov, _genReg(RCX, instr.Type), newOp2),
			Plain(op), // may god help me
			Unary(instrName, rcx),
			Bin(Mov, newDest, _genReg(RDX, instr.Type)),
		}
	}
	newOp2, op := signExtend(P, proc, instr, instr.B)
	return []asm.Line{
		Bin(Xor, RDX.QWord, RDX.QWord),
		Bin(Mov, _genReg(RAX, instr.Type), newOp1),
		Plain(op),
		Unary(instrName, newOp2),
		Bin(Mov, newDest, _genReg(RDX, instr.Type)),
	}
}

// shifts only accept CL as registers, yay amd64!
func genShift(P *mir.Program, proc *mir.Procedure, instr mir.Instr) []asm.Line {
	dest, op, ok := convertToTwoAddr(instr)
	newInstr := genInstrName(instr)
	if ok {
		out, newOp1 := resolveOperand(P, proc, op)
		if mirc.IsRegister(op.Class) {
			out = append(out, mov_opToReg(RCX, 1, false, op))
			newOp1 = RCX.Byte
		}
		newDest := convertOperandProc(P, proc, dest)
		out = append(out, []asm.Line{
			Bin(newInstr, newDest, newOp1),
		}...)
		return out
	}
	newOp1 := convertOperandProc(P, proc, instr.A.Op())
	// we only need to resolve it for the second one, the first is already going in a register
	out, newOp2 := resolveOperand(P, proc, instr.B.Operand)
	if mirc.IsRegister(instr.B.Class) {
		out = append(out, mov_opToReg(RCX, 1, false, instr.B.Op()))
		newOp2 = RCX.Byte
	}
	newDest := convertOperandProc(P, proc, instr.Dest.Op())
	out = append(out, []asm.Line{
		Bin(Mov, newDest, newOp1),
		Bin(newInstr, newDest, newOp2),
	}...)
	return out
}

func genMul(P *mir.Program, proc *mir.Procedure, instr mir.Instr) []asm.Line {
	newInstr := genInstrName(instr)
	if instr.Type.Size() == 1 {
		newOp1 := convertOperandProc(P, proc, instr.A.Op())
		newOp2 := convertOperandProc(P, proc, instr.B.Op())
		newDest := convertOperandProc(P, proc, instr.Dest.Op())
		rax := _genReg(RAX, instr.Type)
		return []asm.Line{
			Bin(Xor, RAX.QWord, RAX.QWord),
			Bin(Mov, rax, newOp1),
			Unary(newInstr, newOp2),
			Bin(Mov, newDest, rax),
		}
	}
	dest, op, ok := convertToTwoAddr(instr)
	if ok {
		out, newOp1 := resolveOperand(P, proc, op)
		newDest := convertOperandProc(P, proc, dest)
		out = append(out, []asm.Line{
			Bin(newInstr, newDest, newOp1),
		}...)
		return out
	}
	newOp1 := convertOperandProc(P, proc, instr.A.Op())
	// we only need to resolve it for the second one, the first is already going in a register
	out, newOp2 := resolveOperand(P, proc, instr.B.Operand)
	newDest := convertOperandProc(P, proc, instr.Dest.Op())
	out = append(out, []asm.Line{
		Bin(Mov, newDest, newOp1),
		Bin(newInstr, newDest, newOp2),
	}...)
	return out
}

// given P+N:
//      if N is i8~i32 we sign-extend
//      if N is i64 we add normally
//      if N is u8~u16 we zero-extend
//      if N is u32 we mov to higher register
//      if N is u64 we add normally
func genAdd(P *mir.Program, proc *mir.Procedure, instr mir.Instr) []asm.Line {
	if !T.IsPtr(instr.A.Type) && !T.IsPtr(instr.B.Type) {
		return genSimpleBin(P, proc, instr)
	}
	out := []asm.Line{}
	if T.IsPtr(instr.A.Type) {
		ptrOp := convertOperandProc(P, proc, instr.A.Op())
		intOp := _genReg(RAX, T.T_Ptr)

		switch instr.B.Class {
		case mirc.Lit, mirc.Static:
			op := convertOperandProc(P, proc, instr.B.Op())
			mv := Bin(Mov, intOp, op)
			out = append(out, mv)
		case mirc.Register:
			breg := getReg(instr.B.Op())
			mv := mov_t(RAX, breg, T.T_Ptr.Size(), instr.B.Type.Size(), true, T.IsSigned(instr.B.Type))
			out = append(out, mv)
		}

		if areOpEqual(instr.A, instr.Dest) { // the output must be a pointer
			add := Bin(Add, ptrOp, intOp)
			out = append(out, add)
			return out
		} else {
			destOp := convertOperandProc(P, proc, instr.Dest.Op())
			mv := Bin(Mov, destOp, ptrOp)
			out = append(out, mv)
			add := Bin(Add, destOp, intOp)
			out = append(out, add)
			return out
		}
	}
	if T.IsPtr(instr.B.Type) {
		ptrOp := convertOperandProc(P, proc, instr.B.Op())
		intOp := _genReg(RAX, T.T_Ptr)

		switch instr.A.Class {
		case mirc.Lit, mirc.Static:
			op := convertOperandProc(P, proc, instr.A.Op())
			mv := Bin(Mov, intOp, op)
			out = append(out, mv)
		case mirc.Register:
			areg := getReg(instr.A.Op())
			mv := mov_t(RAX, areg, T.T_Ptr.Size(), instr.A.Type.Size(), true, T.IsSigned(instr.A.Type))
			out = append(out, mv)
		}
		if areOpEqual(instr.B, instr.Dest) { // the output must be a pointer
			add := Bin(Add, ptrOp, intOp)
			out = append(out, add)
			return out
		} else {
			destOp := convertOperandProc(P, proc, instr.Dest.Op())
			mv := Bin(Mov, destOp, ptrOp)
			out = append(out, mv)
			add := Bin(Add, destOp, intOp)
			out = append(out, add)
			return out
		}
	}
	panic("unreachable")
}

// given P+N, where P is a ptr and N is an integer
//      if N is i8~i32 we sign-extend, neg and add
//      if N is i64 we neg and add
//      if N is u8~u16 we zero-extend, neg and add
//      if N is u32 we mov to higher register, neg and add
//      if N is u64 we neg and add
func genSub(P *mir.Program, proc *mir.Procedure, instr mir.Instr) []asm.Line {
	if T.IsPtr(instr.A.Type) {
		out := []asm.Line{}
		ptrOp := convertOperandProc(P, proc, instr.A.Op())
		intOp := _genReg(RAX, T.T_Ptr)

		switch instr.B.Class {
		case mirc.Lit, mirc.Static:
			op := convertOperandProc(P, proc, instr.B.Op())
			mv := Bin(Mov, intOp, op)
			out = append(out, mv)
		case mirc.Register:
			breg := getReg(instr.B.Op())
			mv := mov_t(RAX, breg, T.T_Ptr.Size(), instr.B.Type.Size(), true, T.IsSigned(instr.B.Type))
			out = append(out, mv)
		}

		neg := Unary(Neg, _genReg(RAX, T.T_Ptr))
		out = append(out, neg)

		if areOpEqual(instr.A, instr.Dest) { // the output must be a pointer
			add := Bin(Add, ptrOp, intOp)
			out = append(out, add)
			return out
		} else {
			destOp := convertOperandProc(P, proc, instr.Dest.Op())
			mv := Bin(Mov, destOp, ptrOp)
			out = append(out, mv)
			add := Bin(Add, destOp, intOp)
			out = append(out, add)
			return out
		}
	} else {
		if areOpEqual(instr.A, instr.Dest) {
			out, newOp1 := resolveOperand(P, proc, instr.B.Operand)
			newDest := convertOperandProc(P, proc, instr.Dest.Op())
			out = append(out, []asm.Line{
				Bin(Sub, newDest, newOp1),
			}...)
			return out
		}

		if areOpEqual(instr.B, instr.Dest) {
			rax := _genReg(RAX, instr.Type)
			newOp1 := convertOperandProc(P, proc, instr.A.Op())
			out, newOp2 := resolveOperand(P, proc, instr.B.Operand)
			newDest := convertOperandProc(P, proc, instr.Dest.Op())
			out = append(out, []asm.Line{
				Bin(Xor, RAX.QWord, RAX.QWord),
				Bin(Mov, rax, newOp1),
				Bin(Sub, rax, newOp2),
				Bin(Mov, newDest, rax),
			}...)
			return out
		}

		newOp1 := convertOperandProc(P, proc, instr.A.Op())
		out, newOp2 := resolveOperand(P, proc, instr.B.Operand)
		newDest := convertOperandProc(P, proc, instr.Dest.Op())
		out = append(out, []asm.Line{
			Bin(Mov, newDest, newOp1),
			Bin(Sub, newDest, newOp2),
		}...)
		return out
	}
}

// convert immediate -> reg
func genConvert(P *mir.Program, proc *mir.Procedure, instr mir.Instr) []asm.Line {
	if instr.A.Class == mirc.Lit || instr.A.Class == mirc.Static {
		out, newA := resolveOperand(P, proc, instr.A.Operand)
		rcx := _genReg(RCX, instr.A.Type)
		out = append(out, Bin(Mov, rcx, newA))

		dest := getReg(instr.Dest.Op())
		out = append(out, mov_t(
			dest, RCX,
			instr.Dest.Type.Size(), instr.A.Type.Size(),
			T.IsSigned(instr.Dest.Type), T.IsSigned(instr.A.Type),
		))

		return out
	}
	// here instr.A is a register
	if areOpEqual(instr.A, instr.Dest) &&
		instr.A.Type.Equals(instr.Dest.Type) {
		return []asm.Line{}
	}
	dest := getReg(instr.Dest.Op())
	source := getReg(instr.A.Op())
	return []asm.Line{
		mov_t(
			dest, source,
			instr.Dest.Type.Size(), instr.A.Type.Size(),
			T.IsSigned(instr.Dest.Type), T.IsSigned(instr.A.Type),
		),
	}
}

// UTILITARIES

type RegMap struct {
	QWord asm.Operand
	DWord asm.Operand
	Word  asm.Operand
	Byte  asm.Operand
}

var RAX = RegMap{QWord: Reg(0, asm.QuadWord), DWord: Reg(0, asm.DoubleWord), Word: Reg(0, asm.Word), Byte: Reg(0, asm.Byte)}
var RCX = RegMap{QWord: Reg(1, asm.QuadWord), DWord: Reg(1, asm.DoubleWord), Word: Reg(1, asm.Word), Byte: Reg(1, asm.Byte)}
var RDX = RegMap{QWord: Reg(2, asm.QuadWord), DWord: Reg(2, asm.DoubleWord), Word: Reg(2, asm.Word), Byte: Reg(2, asm.Byte)}
var RDI = RegMap{QWord: Reg(7, asm.QuadWord), DWord: Reg(7, asm.DoubleWord), Word: Reg(7, asm.Word), Byte: Reg(7, asm.Byte)}

// yep, sorry
var Registers = []RegMap{
	{QWord: Reg(15, asm.QuadWord), DWord: Reg(15, asm.DoubleWord), Word: Reg(15, asm.Word), Byte: Reg(15, asm.Byte)},
	{QWord: Reg(14, asm.QuadWord), DWord: Reg(14, asm.DoubleWord), Word: Reg(14, asm.Word), Byte: Reg(14, asm.Byte)},
	{QWord: Reg(13, asm.QuadWord), DWord: Reg(13, asm.DoubleWord), Word: Reg(13, asm.Word), Byte: Reg(13, asm.Byte)},
	{QWord: Reg(12, asm.QuadWord), DWord: Reg(12, asm.DoubleWord), Word: Reg(12, asm.Word), Byte: Reg(12, asm.Byte)},

	{QWord: Reg(11, asm.QuadWord), DWord: Reg(11, asm.DoubleWord), Word: Reg(11, asm.Word), Byte: Reg(11, asm.Byte)},
	{QWord: Reg(10, asm.QuadWord), DWord: Reg(10, asm.DoubleWord), Word: Reg(10, asm.Word), Byte: Reg(10, asm.Byte)},
	{QWord: Reg(9, asm.QuadWord), DWord: Reg(9, asm.DoubleWord), Word: Reg(9, asm.Word), Byte: Reg(9, asm.Byte)},
	{QWord: Reg(8, asm.QuadWord), DWord: Reg(8, asm.DoubleWord), Word: Reg(8, asm.Word), Byte: Reg(8, asm.Byte)},

	{QWord: Reg(7, asm.QuadWord), DWord: Reg(7, asm.DoubleWord), Word: Reg(7, asm.Word), Byte: Reg(7, asm.Byte)},
	{QWord: Reg(6, asm.QuadWord), DWord: Reg(6, asm.DoubleWord), Word: Reg(6, asm.Word), Byte: Reg(6, asm.Byte)},
	{QWord: Reg(3, asm.QuadWord), DWord: Reg(3, asm.DoubleWord), Word: Reg(3, asm.Word), Byte: Reg(3, asm.Byte)},
}

func genInstrName(instr mir.Instr) InstrKind {
	if T.IsSigned(instr.Type) {
		switch instr.T {
		case IT.Add:
			return Add
		case IT.Sub:
			return Sub
		case IT.Mult:
			return IMul // use imul for both signed and unsigned :)
		case IT.Div, IT.Rem:
			return IDiv
		case IT.And:
			return And
		case IT.Or:
			return Or
		case IT.Xor:
			return Xor
		case IT.ShiftLeft:
			return Sal
		case IT.ShiftRight:
			return Sar
		case IT.Eq:
			return Sete
		case IT.Diff:
			return Setne
		case IT.Less:
			return Setl
		case IT.More:
			return Setg
		case IT.MoreEq:
			return Setge
		case IT.LessEq:
			return Setle
		}
	} else {
		switch instr.T {
		case IT.Add:
			return Add
		case IT.Sub:
			return Sub
		case IT.Mult:
			return IMul // use imul for both signed and unsigned :)
		case IT.Div, IT.Rem:
			return Div
		case IT.And:
			return And
		case IT.Or:
			return Or
		case IT.Xor:
			return Xor
		case IT.ShiftLeft:
			return Shl
		case IT.ShiftRight:
			return Shr
		case IT.Eq:
			return Sete
		case IT.Diff:
			return Setne
		case IT.More:
			return Seta
		case IT.MoreEq:
			return Setae
		case IT.Less:
			return Setb
		case IT.LessEq:
			return Setbe
		}
	}
	fmt.Println(instr)
	panic("unimplemented")
}

var max = big.NewInt((1 << 31) - 1)
var min = big.NewInt(-(1 << 31))

/* in amd64, you can only load an imm64 to a register,
so if the value is beyond the 32bit range, you need to
use a register before moving things to memory.

this should really be part of the register allocator i think,
but for now it suffices to resolve it here.
*/
func resolveOperand(P *mir.Program, proc *mir.Procedure, op mir.Operand) ([]asm.Line, asm.Operand) {
	opstr := convertOperandProc(P, proc, op)
	if op.Class == mirc.Lit &&
		(op.Num.Cmp(min) == -1 || op.Num.Cmp(max) == 1) {
		out := _genReg(RCX, op.Type)
		mv := Bin(Mov, out, opstr)
		return []asm.Line{mv}, out
	}
	return nil, opstr
}

func convertOperandProc(P *mir.Program, proc *mir.Procedure, op mir.Operand) asm.Operand {
	return convertOperand(P, op, proc.NumOfVars, proc.NumOfSpills, proc.NumOfMaxCalleeArguments)
}

func convertOperand(P *mir.Program, op mir.Operand, NumOfVars, NumOfSpills, NumOfMaxCalleeArguments int) asm.Operand {
	switch op.Class {
	case mirc.Register:
		return genReg(op.ID, op.Type)
	case mirc.CallerInterproc:
		offset := cc.Arg(int(op.ID))
		return AddrFrame(offset, TypeToTsize(op.Type))
	case mirc.Local:
		offset := cc.Var(int(op.ID))
		return AddrFrame(offset, TypeToTsize(op.Type))
	case mirc.Spill:
		offset := cc.Spill(NumOfVars, int(op.ID))
		return AddrFrame(offset, TypeToTsize(op.Type))
	case mirc.CalleeInterproc:
		offset := cc.CallArg(NumOfVars, NumOfSpills, NumOfMaxCalleeArguments, int(op.ID))
		return AddrFrame(offset, TypeToTsize(op.Type))
	case mirc.Lit:
		if op.Num == nil {
			panic("bignum was nil")
		}
		return Const(op.Num)
	case mirc.Static:
		sy := P.FindSymbol(mir.SymbolID(op.ID))
		if sy == nil {
			panic("symbol was nil")
		}
		if sy.Proc != nil {
			return LabelOp(sy.Proc.Label)
		}
		if sy.Mem == nil && sy.Proc == nil {
			panic("Mem and Proc were nil")
		}
		return LabelOp(sy.Mem.Label)
	}
	panic("unimplemented: " + op.String())
}

func genReg(num int64, t *T.Type) asm.Operand {
	if num > int64(len(Registers)) || num < 0 {
		panic("oh no")
	}
	r := Registers[num]
	return _genReg(r, t)
}

func _genReg(r RegMap, t *T.Type) asm.Operand {
	if T.IsBasic(t) {
		switch t.Basic {
		case T.Ptr:
			return r.QWord
		case T.I64, T.U64:
			return r.QWord
		case T.I32, T.U32:
			return r.DWord
		case T.I16, T.U16:
			return r.Word
		case T.I8, T.U8:
			return r.Byte
		case T.Bool:
			return r.Byte
		}
	} else {
		if !T.IsProc(t) {
			panic(t.String())
		}
		return r.QWord
	}
	panic(t.String())
}

func convertString(original string) string {
	s := original[1 : len(original)-1] // removes quotes
	output := " db '"
	for i := 0; i < len(s); i++ {
		r := s[i]
		if r == '\\' {
			i++
			r = s[i]
			switch r {
			case 'n':
				output += "', 0xA, '"
			case 't':
				output += "', 0x9, '"
			case 'r':
				output += "', 0xD, '"
			case '\'':
				output += "', 0x27, '"
			case '\\':
				output += "', 0x5C, '"
			default:
				output += string(r)
			}
		} else {
			output += string(r)
		}
	}
	output += "'"
	return output
}

func convertToTwoAddr(instr mir.Instr) (dest mir.Operand, op mir.Operand, ok bool) {
	if !instr.Dest.Valid {
		return mir.Operand{}, mir.Operand{}, false
	}
	if instr.T == IT.Sub {
		panic("subtraction is not comutative")
	}

	if areOpEqual(instr.A, instr.Dest) {
		return instr.Dest.Operand, instr.B.Operand, true
	}
	if areOpEqual(instr.B, instr.Dest) {
		return instr.Dest.Operand, instr.A.Operand, true
	}

	return mir.Operand{}, mir.Operand{}, false
}

func areOpEqual(a, b mir.OptOperand) bool {
	if !a.Valid || !b.Valid {
		panic("invalid operand")
	}
	return a.Class == b.Class &&
		a.ID == b.ID &&
		a.Num == b.Num
}

func signExtendRCX(instr mir.Instr) (rcx asm.Operand, op InstrKind) {
	rcx = _genReg(RCX, instr.Type)
	op = Nop
	switch instr.Type.Size() {
	case 1, 2, 4:
		rcx = RCX.DWord
		op = Cdq
	case 8:
		if T.IsSigned(instr.Type) {
			rcx = RCX.QWord
			op = Cqo
		}
	}
	return rcx, op
}

func signExtend(P *mir.Program, proc *mir.Procedure, instr mir.Instr, opnd mir.OptOperand) (operand asm.Operand, operation InstrKind) {
	operand = convertOperandProc(P, proc, instr.B.Op())
	operation = Nop
	switch instr.Type.Size() {
	case 1, 2, 4:
		opnd.Type = T.T_I32
		operand = convertOperandProc(P, proc, opnd.Op())
		operation = Cdq
	case 8:
		if T.IsSigned(instr.Type) {
			operand = convertOperandProc(P, proc, instr.B.Op())
			operation = Cqo
		}
	}
	return operand, operation
}

func getReg(op mir.Operand) RegMap {
	num := op.ID
	if num > int64(len(Registers)) || num < 0 {
		panic("oh no")
	}
	return Registers[num]
}

func mov_opToReg(
	dest RegMap, destSize int, destSigned bool,
	op mir.Operand,
) asm.Line {
	source := getReg(op)
	sourceSize := op.Type.Size()
	sourceSigned := T.IsSigned(op.Type)
	return mov_t(dest, source, destSize, sourceSize, destSigned, sourceSigned)
}

func mov_t(
	dest, source RegMap,
	destSize, sourceSize int,
	destSigned, sourceSigned bool,
) asm.Line {
	if destSize == sourceSize {
		return Bin(Mov, genReg_s(dest, destSize), genReg_s(source, sourceSize))
	}
	if destSize > sourceSize {
		d := genReg_s(dest, destSize)
		s := genReg_s(source, sourceSize)
		if destSize == 8 && sourceSize == 4 {
			if destSigned && sourceSigned {
				return Bin(Movsxd, d, s)
			} else {
				// when mov'ing 32 bit regiters, the upper 32bits are set to zero
				// https://stackoverflow.com/questions/51387571/movzx-missing-32-bit-register-to-64-bit-register
				return Bin(Mov, genReg_s(dest, sourceSize), genReg_s(source, sourceSize))
			}
		} else {
			if destSigned && sourceSigned {
				return Bin(Movsx, d, s)
			} else {
				return Bin(Movzx, d, s)
			}
		}
	}
	return Bin(Mov, genReg_s(dest, destSize), genReg_s(source, destSize)) // truncates the value :)
}

func genReg_s(r RegMap, size int) asm.Operand {
	switch size {
	case 8:
		return r.QWord
	case 4:
		return r.DWord
	case 2:
		return r.Word
	case 1:
		return r.Byte
	default:
		panic("invalid register size")
	}
}
