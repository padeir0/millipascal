package amd64

import (
	mir "mpc/core/mir"
	mirc "mpc/core/mir/class"
	FT "mpc/core/mir/flowkind"
	IT "mpc/core/mir/instrkind"
	T "mpc/core/types"
	"strconv"
)

func Generate(P *mir.Program) *FasmProgram {
	output := &fasmProgram{
		executable: []*fasmProc{genWrite(P), genRead(P), genError(P)},
		data:       []*fasmData{},
		entry:      genEntry(P),
	}
	for _, sy := range P.Symbols {
		if !sy.Builtin {
			if sy.Proc != nil {
				proc := genProc(P, sy.Proc)
				output.executable = append(output.executable, proc)
			}
			if sy.Mem != nil {
				mem := genMem(sy.Mem)
				output.data = append(output.data, mem)
			}
		}
	}
	return &FasmProgram{
		Contents: output.String(),
		Name:     P.Name,
	}
}

type FasmProgram struct {
	Name     string
	Contents string
}

type fasmProgram struct {
	entry      []*amd64Instr
	executable []*fasmProc
	data       []*fasmData
}

func (this *fasmProgram) String() string {
	output := "format ELF64 executable 3\n"
	output += "\nsegment readable writable\n"
	for _, decData := range this.data {
		output += decData.String()
	}
	output += "\nsegment readable executable\n"
	output += "entry $\n"
	for _, instr := range this.entry {
		output += "\t" + instr.String() + "\n"
	}
	output += "; ---- end \n"
	for _, fproc := range this.executable {
		output += fproc.String()
	}
	return output
}

type fasmData struct {
	label    string
	content  string
	declared bool
}

func (this *fasmData) String() string {
	if this.declared {
		return this.label + " db " + this.content + "\n"
	}
	return this.label + " rb " + this.content + "\n"
}

type fasmProc struct {
	label  string
	blocks []*fasmBlock // order matters
}

func (this *fasmProc) String() string {
	output := ""
	for _, b := range this.blocks {
		output += b.String() + "\n"
	}
	return output
}

type fasmBlock struct {
	label string
	code  []*amd64Instr // order matters ofc
}

func (this *fasmBlock) String() string {
	output := this.label + ":\n"
	for _, instr := range this.code {
		output += "\t" + instr.String() + "\n"
	}
	return output
}

type InstrType int

type amd64Instr struct {
	Instr string
	Op1   string
	Op2   string
}

func (this *amd64Instr) String() string {
	if this.Instr == "" {
		return "???"
	}
	if this.Op1 == "" {
		return this.Instr
	}
	if this.Op2 == "" {
		return this.Instr + "\t" + this.Op1
	}
	return this.Instr + "\t" + this.Op1 + ", " + this.Op2
}

const (
	Add  = "add"
	Sub  = "sub"
	Neg  = "neg"
	IMul = "imul"
	IDiv = "idiv"
	Xor  = "xor"

	Mov   = "mov"
	Movsx = "movsx"
	Push  = "push"
	Pop   = "pop"

	And = "and"
	Or  = "or"

	Cmp = "cmp"

	Sete  = "sete"
	Setne = "setne"
	Setg  = "setg"
	Setge = "setge"
	Setl  = "setl"
	Setle = "setle"

	Jmp = "jmp"
	Je  = "je"
	Jne = "jne"
	Jg  = "jg"
	Jge = "jge"
	Jl  = "jl"
	Jle = "jle"

	Call    = "call"
	Syscall = "syscall"
	Ret     = "ret"
)

type register struct {
	QWord string
	DWord string
	Word  string
	Byte  string
}

// we use this three as scratch space, IDIV already needs rax and rdx,
// and very rarely will a block need more than 3~5 registers
var RAX = &register{QWord: "rax", DWord: "eax", Word: "ax", Byte: "al"}
var RDX = &register{QWord: "rdx", DWord: "edx", Word: "dx", Byte: "dl"}
var RBX = &register{QWord: "rbx", DWord: "ebx", Word: "bx", Byte: "bl"}

var Registers = []*register{
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
}

// read[ptr, int] int
func genRead(P *mir.Program) *fasmProc {
	ptrArg := &mir.Operand{Class: mirc.CallerInterproc, Num: 0, Type: T.T_Ptr}
	sizeArg := &mir.Operand{Class: mirc.CallerInterproc, Num: 1, Type: T.T_I64}
	ptr := convertOperand(P, ptrArg, 0, 0, 0)
	size := convertOperand(P, sizeArg, 0, 0, 0)
	amountRead := ptr
	return &fasmProc{
		label: "_read",
		blocks: []*fasmBlock{
			{label: "_read", code: []*amd64Instr{
				genUnaryInstr(Push, "rbp"),
				genBinInstr(Mov, "rbp", "rsp"),

				genBinInstr(Mov, "rdx", size),
				genBinInstr(Mov, "rsi", ptr),
				genBinInstr(Mov, "rdi", "0"), // STDERR
				genBinInstr(Mov, "rax", "0"), // WRITE
				{Instr: Syscall},

				genBinInstr(Mov, amountRead, "rax"),

				genBinInstr(Mov, "rsp", "rbp"),
				genUnaryInstr(Pop, "rbp"),
				{Instr: Ret},
			}},
		},
	}
}

// write[ptr, int]
func genWrite(P *mir.Program) *fasmProc {
	ptrArg := &mir.Operand{Class: mirc.CallerInterproc, Num: 0, Type: T.T_Ptr}
	sizeArg := &mir.Operand{Class: mirc.CallerInterproc, Num: 1, Type: T.T_I64}
	ptr := convertOperand(P, ptrArg, 0, 0, 0)
	size := convertOperand(P, sizeArg, 0, 0, 0)
	return &fasmProc{
		label: "_write",
		blocks: []*fasmBlock{
			{label: "_write", code: []*amd64Instr{
				genUnaryInstr(Push, "rbp"),
				genBinInstr(Mov, "rbp", "rsp"),

				genBinInstr(Mov, "rdx", size),
				genBinInstr(Mov, "rsi", ptr),
				genBinInstr(Mov, "rdi", "1"), // STDOUT
				genBinInstr(Mov, "rax", "1"), // WRITE
				{Instr: Syscall},

				genBinInstr(Mov, "rsp", "rbp"),
				genUnaryInstr(Pop, "rbp"),
				{Instr: Ret},
			}},
		},
	}
}

// error[ptr, int]
func genError(P *mir.Program) *fasmProc {
	ptrArg := &mir.Operand{Class: mirc.CallerInterproc, Num: 0, Type: T.T_Ptr}
	sizeArg := &mir.Operand{Class: mirc.CallerInterproc, Num: 1, Type: T.T_I64}
	ptr := convertOperand(P, ptrArg, 0, 0, 0)
	size := convertOperand(P, sizeArg, 0, 0, 0)
	return &fasmProc{
		label: "_error",
		blocks: []*fasmBlock{
			{label: "_error", code: []*amd64Instr{
				genUnaryInstr(Push, "rbp"),
				genBinInstr(Mov, "rbp", "rsp"),

				genBinInstr(Mov, "rdx", size),
				genBinInstr(Mov, "rsi", ptr),
				genBinInstr(Mov, "rdi", "2"), // STDERR
				genBinInstr(Mov, "rax", "1"), // WRITE
				{Instr: Syscall},

				genBinInstr(Mov, "rsp", "rbp"),
				genUnaryInstr(Pop, "rbp"),
				{Instr: Ret},
			}},
		},
	}
}

func genMem(mem *mir.MemoryDecl) *fasmData {
	if mem.Data == "" {
		return &fasmData{
			label:    mem.Label,
			content:  strconv.FormatInt(mem.Size, 10),
			declared: false,
		}
	}
	return &fasmData{
		label:    mem.Label,
		content:  convertString(mem.Data),
		declared: true,
	}
}

func convertString(original string) string {
	s := original[1 : len(original)-1] // removes quotes
	output := "'"
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

func genEntry(P *mir.Program) []*amd64Instr {
	entry := P.Symbols[P.Entry]
	if entry == nil || entry.Proc == nil {
		panic("nil entrypoint")
	}
	return []*amd64Instr{
		genUnaryInstr(Call, entry.Proc.Label),
		genBinInstr(Xor, "rdi", "rdi"), // EXIT CODE 0
		genBinInstr(Mov, "rax", "60"),  // EXIT
		{Instr: Syscall},
	}
}

func genProc(P *mir.Program, proc *mir.Procedure) *fasmProc {
	stackReserve := 8 * (proc.NumOfVars + proc.NumOfSpills + proc.NumOfMaxCalleeArguments)
	init := &fasmBlock{
		label: proc.Label,
		code: []*amd64Instr{
			genUnaryInstr(Push, "rbp"),
			genBinInstr(Mov, "rbp", "rsp"),
			genBinInstr(Sub, "rsp", strconv.FormatInt(int64(stackReserve), 10)),
		},
	}
	fproc := &fasmProc{label: proc.Label, blocks: []*fasmBlock{init}}
	proc.ResetBlocks()
	fproc.blocks = append(fproc.blocks, genBlocks(P, proc, proc.FirstBlock())...)
	return fproc
}

func genBlocks(P *mir.Program, proc *mir.Procedure, start *mir.BasicBlock) []*fasmBlock {
	trueBranches := []*mir.BasicBlock{}
	falseBlocks := genFalseBranches(P, proc, start, &trueBranches)
	for _, tBlock := range trueBranches {
		out := genBlocks(P, proc, tBlock)
		falseBlocks = append(falseBlocks, out...)
	}
	return falseBlocks
}

func genFalseBranches(P *mir.Program, proc *mir.Procedure, block *mir.BasicBlock, trueBranches *[]*mir.BasicBlock) []*fasmBlock {
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
			jmp := genUnaryInstr(Jmp, t.Label)
			fb.code = append(fb.code, jmp)
			return []*fasmBlock{fb}
		}
		out := genFalseBranches(P, proc, t, trueBranches)
		out = append([]*fasmBlock{fb}, out...)
		return out
	case FT.If:
		t := proc.GetBlock(block.Out.True)
		if !t.Visited {
			*trueBranches = append(*trueBranches, t)
		}
		jmp := genCondJmp(P, proc, t, block.Out.V[0])
		fb.code = append(fb.code, jmp...)
		f := proc.GetBlock(block.Out.False)
		out := genFalseBranches(P, proc, f, trueBranches)
		out = append([]*fasmBlock{fb}, out...)
		return out
	case FT.Exit:
		exit := genExit(P, proc, block.Out.V[0])
		fb.code = append(fb.code, exit...)
		return []*fasmBlock{fb}
	case FT.Return:
		ret := genRet()
		fb.code = append(fb.code, ret...)
		return []*fasmBlock{fb}
	}
	panic("Invalid flow: " + block.Out.String())
}

func genCondJmp(P *mir.Program, proc *mir.Procedure, block *mir.BasicBlock, op *mir.Operand) []*amd64Instr {
	newOp := convertOperandProc(P, proc, op)
	if op.Class == mirc.Lit || op.Class == mirc.Static {
		rbx := genReg(RBX, op.Type)
		return []*amd64Instr{
			genBinInstr(Mov, rbx, newOp),
			genBinInstr(Cmp, rbx, "1"),
			genUnaryInstr(Je, block.Label),
		}
	}
	return []*amd64Instr{
		genBinInstr(Cmp, newOp, "1"),
		genUnaryInstr(Je, block.Label),
	}
}

func genRet() []*amd64Instr {
	return []*amd64Instr{
		genBinInstr(Mov, "rsp", "rbp"),
		genUnaryInstr(Pop, "rbp"),
		{Instr: Ret},
	}
}

func genExit(P *mir.Program, proc *mir.Procedure, op *mir.Operand) []*amd64Instr {
	exitCode := convertOperandProc(P, proc, op)
	return []*amd64Instr{
		genBinInstr(Xor, "rdi", "rdi"),
		genBinInstr(Mov, "dil", exitCode), // EXIT CODE
		genBinInstr(Mov, "rax", "60"),     // EXIT
		{Instr: Syscall},
	}
}

func genCode(P *mir.Program, proc *mir.Procedure, block *mir.BasicBlock) *fasmBlock {
	output := make([]*amd64Instr, len(block.Code))[:0]
	for _, instr := range block.Code {
		output = append(output, genInstr(P, proc, instr)...)
	}
	return &fasmBlock{label: block.Label, code: output}
}

func genInstr(P *mir.Program, proc *mir.Procedure, instr *mir.Instr) []*amd64Instr {
	switch instr.T {
	case IT.Add, IT.Mult, IT.Or, IT.And:
		return genBin(P, proc, instr)
	case IT.Sub:
		return genSub(P, proc, instr)
	case IT.Eq, IT.Diff, IT.Less, IT.More, IT.LessEq, IT.MoreEq:
		return genComp(P, proc, instr)
	case IT.Load, IT.Store, IT.Copy:
		return genLoadStore(P, proc, instr)
	case IT.LoadPtr:
		return genLoadPtr(P, proc, instr)
	case IT.StorePtr:
		return genStorePtr(P, proc, instr)
	case IT.Neg:
		return genUnaryMinus(P, proc, instr)
	case IT.Div:
		return genDiv(P, proc, instr)
	case IT.Rem:
		return genRem(P, proc, instr)
	case IT.Not:
		return genNot(P, proc, instr)
	case IT.Convert:
		return genConvert(P, proc, instr)
	case IT.Call:
		return genCall(P, proc, instr)
	default:
		panic("unimplemented: " + instr.String())
	}
}

func genConvert(P *mir.Program, proc *mir.Procedure, instr *mir.Instr) []*amd64Instr {
	if instr.Dest.Type.Size() > instr.A.Type.Size() {
		newA := convertOperandProc(P, proc, instr.A)
		newDest := convertOperandProc(P, proc, instr.Dest)
		return []*amd64Instr{
			genBinInstr(Movsx, newDest, newA),
		}
	}
	if instr.A.Class == mirc.Lit {
		newA := convertOperandProc(P, proc, instr.A)
		newDest := convertOperandProc(P, proc, instr.Dest)
		return []*amd64Instr{
			genBinInstr(Mov, newDest, newA),
		}
	}
	if instr.A.Class == mirc.Static {
		newA := convertOperandProc(P, proc, instr.A)
		newDest := convertOperandProc(P, proc, instr.Dest)
		res := genReg(RAX, instr.Dest.Type)
		return []*amd64Instr{
			genBinInstr(Mov, "rax", newA),
			genBinInstr(Mov, newDest, res),
		}
	}
	if !areOpEqual(instr.A, instr.Dest) {
		newA := getReg(instr.A.Num, instr.Dest.Type)
		newDest := convertOperandProc(P, proc, instr.Dest)
		return []*amd64Instr{
			genBinInstr(Mov, newDest, newA),
		}
	}
	return []*amd64Instr{}
}

func genCall(P *mir.Program, proc *mir.Procedure, instr *mir.Instr) []*amd64Instr {
	newA := convertOperandProc(P, proc, instr.A)
	return []*amd64Instr{
		genUnaryInstr(Call, newA),
	}
}

func genLoadStore(P *mir.Program, proc *mir.Procedure, instr *mir.Instr) []*amd64Instr {
	newA := convertOperandProc(P, proc, instr.A)
	newDest := convertOperandProc(P, proc, instr.Dest)
	return []*amd64Instr{
		genMov(newDest, newA),
	}
}

func genLoadPtr(P *mir.Program, proc *mir.Procedure, instr *mir.Instr) []*amd64Instr {
	newA := convertOperandProc(P, proc, instr.A)
	newDest := convertOperandProc(P, proc, instr.Dest)
	return []*amd64Instr{
		genMov(newDest, genType(instr.Type)+"["+newA+"]"), // xD
	}
}

func genStorePtr(P *mir.Program, proc *mir.Procedure, instr *mir.Instr) []*amd64Instr {
	newA := convertOperandProc(P, proc, instr.A)
	newDest := convertOperandProc(P, proc, instr.B)
	return []*amd64Instr{
		genMov(genType(instr.Type)+"["+newDest+"]", newA),
	}
}

func genNot(P *mir.Program, proc *mir.Procedure, instr *mir.Instr) []*amd64Instr {
	newA := convertOperandProc(P, proc, instr.A)
	newDest := convertOperandProc(P, proc, instr.Dest)
	return []*amd64Instr{
		genBinInstr(Cmp, newA, "0"),
		genUnaryInstr(Sete, newDest),
	}
}

func genUnaryMinus(P *mir.Program, proc *mir.Procedure, instr *mir.Instr) []*amd64Instr {
	newA := convertOperandProc(P, proc, instr.A)
	newDest := convertOperandProc(P, proc, instr.Dest)
	return []*amd64Instr{
		genMov(newDest, newA),
		genUnaryInstr(Neg, newDest),
	}
}

var compInstrMap = map[IT.InstrKind]string{
	IT.Eq:     Sete,
	IT.Diff:   Setne,
	IT.Less:   Setl,
	IT.More:   Setg,
	IT.MoreEq: Setge,
	IT.LessEq: Setle,
}

func genComp(P *mir.Program, proc *mir.Procedure, instr *mir.Instr) []*amd64Instr {
	newOp1 := convertOperandProc(P, proc, instr.A)
	newOp2 := convertOperandProc(P, proc, instr.B)
	newDest := convertOperandProc(P, proc, instr.Dest)
	newInstr := compInstrMap[instr.T]
	if instr.A.Class == mirc.Lit || instr.A.Class == mirc.Static {
		rbx := genReg(RBX, instr.A.Type)
		return []*amd64Instr{
			genBinInstr(Mov, rbx, newOp1),
			genBinInstr(Cmp, rbx, newOp2),
			genUnaryInstr(newInstr, newDest),
		}
	}
	return []*amd64Instr{
		genBinInstr(Cmp, newOp1, newOp2),
		genUnaryInstr(newInstr, newDest),
	}
}

func genDiv(P *mir.Program, proc *mir.Procedure, instr *mir.Instr) []*amd64Instr {
	newOp1 := convertOperandProc(P, proc, instr.A)
	newOp2 := convertOperandProc(P, proc, instr.B)
	newDest := convertOperandProc(P, proc, instr.Dest)
	if instr.B.Class == mirc.Lit || instr.B.Class == mirc.Static {
		rbx := genReg(RBX, instr.Type)
		return []*amd64Instr{
			genBinInstr(Xor, RDX.QWord, RDX.QWord),
			genMov(genReg(RAX, instr.Type), newOp1),
			genMov(rbx, newOp2),
			genUnaryInstr(IDiv, rbx),
			genMov(newDest, genReg(RAX, instr.Type)),
		}
	}
	return []*amd64Instr{
		genBinInstr(Xor, RDX.QWord, RDX.QWord),
		genMov(genReg(RAX, instr.Type), newOp1),
		genUnaryInstr(IDiv, newOp2),
		genMov(newDest, genReg(RAX, instr.Type)),
	}
}

func genRem(P *mir.Program, proc *mir.Procedure, instr *mir.Instr) []*amd64Instr {
	newOp1 := convertOperandProc(P, proc, instr.A)
	newOp2 := convertOperandProc(P, proc, instr.B)
	newDest := convertOperandProc(P, proc, instr.Dest)
	if mirc.IsImmediate(instr.B.Class) {
		rbx := genReg(RBX, instr.Type)
		return []*amd64Instr{
			genBinInstr(Xor, RDX.QWord, RDX.QWord),
			genMov(genReg(RAX, instr.Type), newOp1),
			genMov(rbx, newOp2),
			genUnaryInstr(IDiv, rbx),
			genMov(newDest, genReg(RDX, instr.Type)),
		}
	}
	return []*amd64Instr{
		genBinInstr(Xor, RDX.QWord, RDX.QWord),
		genMov(genReg(RAX, instr.Type), newOp1),
		genUnaryInstr(IDiv, newOp2),
		genMov(newDest, genReg(RDX, instr.Type)),
	}
}

var BinInstrMap = map[IT.InstrKind]string{
	IT.Add:  Add,
	IT.Sub:  Sub,
	IT.Mult: IMul,
	IT.And:  And,
	IT.Or:   Or,
}

func genSub(P *mir.Program, proc *mir.Procedure, instr *mir.Instr) []*amd64Instr {
	sub := BinInstrMap[instr.T]

	if areOpEqual(instr.A, instr.Dest) {
		newOp1 := convertOperandProc(P, proc, instr.B)
		newDest := convertOperandProc(P, proc, instr.Dest)
		return []*amd64Instr{
			genBinInstr(sub, newDest, newOp1),
		}
	}

	if areOpEqual(instr.B, instr.Dest) {
		rbx := genReg(RBX, instr.Type)
		newOp1 := convertOperandProc(P, proc, instr.A)
		newOp2 := convertOperandProc(P, proc, instr.B)
		newDest := convertOperandProc(P, proc, instr.Dest)
		return []*amd64Instr{
			genBinInstr(Xor, RBX.QWord, RBX.QWord),
			genMov(rbx, newOp1),
			genBinInstr(sub, rbx, newOp2),
			genMov(newDest, rbx),
		}
	}

	newOp1 := convertOperandProc(P, proc, instr.A)
	newOp2 := convertOperandProc(P, proc, instr.B)
	newDest := convertOperandProc(P, proc, instr.Dest)
	return []*amd64Instr{
		genMov(newDest, newOp1),
		genBinInstr(sub, newDest, newOp2),
	}
}

func genBin(P *mir.Program, proc *mir.Procedure, instr *mir.Instr) []*amd64Instr {
	dest, op := convertToTwoAddr(instr)
	newInstr := BinInstrMap[instr.T]
	if dest == nil || op == nil {
		newOp1 := convertOperandProc(P, proc, instr.A)
		newOp2 := convertOperandProc(P, proc, instr.B)
		newDest := convertOperandProc(P, proc, instr.Dest)
		return []*amd64Instr{
			genMov(newDest, newOp1),
			genBinInstr(newInstr, newDest, newOp2),
		}
	}
	newOp1 := convertOperandProc(P, proc, op)
	newDest := convertOperandProc(P, proc, dest)
	return []*amd64Instr{
		genBinInstr(newInstr, newDest, newOp1),
	}
}

func genUnaryInstr(instr string, op string) *amd64Instr {
	return &amd64Instr{
		Instr: instr,
		Op1:   op,
	}
}

func genBinInstr(instr string, dest, source string) *amd64Instr {
	return &amd64Instr{
		Instr: instr,
		Op1:   dest,
		Op2:   source,
	}
}

func genMov(dest, source string) *amd64Instr {
	return &amd64Instr{
		Instr: Mov,
		Op1:   dest,
		Op2:   source,
	}
}

func convertToTwoAddr(instr *mir.Instr) (dest *mir.Operand, op *mir.Operand) {
	if instr.Dest == nil {
		return nil, nil
	}
	if instr.T == IT.Sub {
		panic("subtraction is not associative")
	}

	if areOpEqual(instr.A, instr.Dest) {
		return instr.Dest, instr.B
	}
	if areOpEqual(instr.B, instr.Dest) {
		return instr.Dest, instr.A
	}

	return nil, nil
}

func areOpEqual(a, b *mir.Operand) bool {
	return a.Class == b.Class &&
		a.Num == b.Num
}

func convertOperandProc(P *mir.Program, proc *mir.Procedure, op *mir.Operand) string {
	return convertOperand(P, op, int64(proc.NumOfVars), int64(proc.NumOfSpills), int64(proc.NumOfMaxCalleeArguments))
}

func convertOperand(P *mir.Program, op *mir.Operand, NumOfVars, NumOfSpills, NumOfMaxCalleeArguments int64) string {
	switch op.Class {
	case mirc.Register:
		return getReg(op.Num, op.Type)
	case mirc.CallerInterproc:
		//        v must jump last rbp + return address
		offset := 16 + op.Num*8
		return genType(op.Type) + "[rbp + " + strconv.FormatInt(offset, 10) + "]"
	case mirc.Local:
		//        v begins at 8 because rbp points to the last rbp
		offset := 8 + op.Num*8
		return genType(op.Type) + "[rbp - " + strconv.FormatInt(offset, 10) + "]"
	case mirc.Spill:
		offset := 8 + NumOfVars*8 + op.Num*8
		return genType(op.Type) + "[rbp - " + strconv.FormatInt(offset, 10) + "]"
	case mirc.CalleeInterproc:
		offset := 8 + NumOfVars*8 +
			NumOfSpills*8 +
			// v count                   v index
			(NumOfMaxCalleeArguments-1-op.Num)*8
		return genType(op.Type) + "[rbp - " + strconv.FormatInt(offset, 10) + "]"
	case mirc.Lit:
		return strconv.FormatInt(op.Num, 10)
	case mirc.Static:
		sy := P.Symbols[op.Num]
		if sy.Proc != nil {
			return sy.Proc.Label
		}
		return sy.Mem.Label
	}
	panic("unimplemented: " + op.String())
}

func getReg(num int64, t *T.Type) string {
	if num > int64(len(Registers)) || num < 0 {
		panic("oh no")
	}
	r := Registers[num]
	return genReg(r, t)
}

func genReg(r *register, t *T.Type) string {
	if T.IsBasic(t) {
		switch t.Basic {
		case T.Ptr:
			return r.QWord
		case T.I64:
			return r.QWord
		case T.I32:
			return r.DWord
		case T.I16:
			return r.Word
		case T.I8:
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

func genType(t *T.Type) string {
	switch t.Size() {
	case 1:
		return "byte"
	case 2:
		return "word"
	case 4:
		return "dword"
	case 8:
		return "qword"
	}
	panic(t.String())
}
