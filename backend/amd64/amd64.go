package amd64

import (
	T "mpc/frontend/Type"
	mirc "mpc/frontend/enums/MIRClass"
	FT "mpc/frontend/enums/flowType"
	IT "mpc/frontend/enums/instrType"
	ST "mpc/frontend/enums/symbolType"
	ir "mpc/frontend/ir"
	"strconv"
)

func Generate(M *ir.Module) *fasmProgram {
	output := &fasmProgram{
		executable: []*fasmProc{genWrite(), genRead(), genError()},
		data:       []*fasmData{},
		entry:      genEntry(M),
	}
	generate(M, output)
	M.ResetVisited()
	return output
}

func generate(M *ir.Module, output *fasmProgram) {
	if M.Visited {
		return
	}
	for _, dep := range M.Dependencies {
		generate(dep.M, output)
	}
	M.Visited = true
	for _, sy := range M.Globals {
		if !sy.External {
			switch sy.T {
			case ST.Proc:
				proc := genProc(sy)
				output.executable = append(output.executable, proc)
			case ST.Mem:
				mem := genMem(sy)
				output.data = append(output.data, mem)
			}
		}
	}
}

type fasmProgram struct {
	entry      []*amd64Instr
	executable []*fasmProc
	data       []*fasmData
}

func (this *fasmProgram) Fasmify() string {
	output := "format ELF64 executable 3\n"
	output += "\nsegment readable writable\n"
	for _, decData := range this.data {
		output += decData.Fasmify()
	}
	output += "\nsegment readable executable\n"
	output += "entry $\n"
	for _, instr := range this.entry {
		output += "\t" + instr.Fasmify() + "\n"
	}
	output += "; ---- end \n"
	for _, fproc := range this.executable {
		output += fproc.Fasmify()
	}
	return output
}

type fasmData struct {
	label    string
	content  string
	declared bool
}

func (this *fasmData) Fasmify() string {
	if this.declared {
		return this.label + " db " + this.content + "\n"
	}
	return this.label + " rb " + this.content + "\n"
}

type fasmProc struct {
	label  string
	blocks []*fasmBlock // order matters
}

func (this *fasmProc) Fasmify() string {
	output := ""
	for _, b := range this.blocks {
		output += b.Fasmify() + "\n"
	}
	return output
}

type fasmBlock struct {
	label string
	code  []*amd64Instr // order matters ofc
}

func (this *fasmBlock) Fasmify() string {
	output := this.label + ":\n"
	for _, instr := range this.code {
		output += "\t" + instr.Fasmify() + "\n"
	}
	return output
}

type InstrType int

type amd64Instr struct {
	Instr string
	Op1   string
	Op2   string
}

func (this *amd64Instr) Fasmify() string {
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
func genRead() *fasmProc {
	ptrArg := &ir.Operand{Mirc: mirc.CallerInterproc, Num: 0, Type: T.T_Ptr}
	sizeArg := &ir.Operand{Mirc: mirc.CallerInterproc, Num: 1, Type: T.T_I64}
	ptr := convertOperand(ptrArg, 0, 0, 0)
	size := convertOperand(sizeArg, 0, 0, 0)
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
func genWrite() *fasmProc {
	ptrArg := &ir.Operand{Mirc: mirc.CallerInterproc, Num: 0, Type: T.T_Ptr}
	sizeArg := &ir.Operand{Mirc: mirc.CallerInterproc, Num: 1, Type: T.T_I64}
	ptr := convertOperand(ptrArg, 0, 0, 0)
	size := convertOperand(sizeArg, 0, 0, 0)
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
func genError() *fasmProc {
	ptrArg := &ir.Operand{Mirc: mirc.CallerInterproc, Num: 0, Type: T.T_Ptr}
	sizeArg := &ir.Operand{Mirc: mirc.CallerInterproc, Num: 1, Type: T.T_I64}
	ptr := convertOperand(ptrArg, 0, 0, 0)
	size := convertOperand(sizeArg, 0, 0, 0)
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

func genMem(sy *ir.Symbol) *fasmData {
	mem := sy.Mem
	if mem.Contents == "" {
		return &fasmData{
			label:    sy.ModuleName + "_" + mem.Name,
			content:  strconv.FormatInt(mem.Size, 10),
			declared: false,
		}
	}
	return &fasmData{
		label:    sy.ModuleName + "_" + mem.Name,
		content:  convertString(mem.Contents),
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

func genEntry(M *ir.Module) []*amd64Instr {
	return []*amd64Instr{
		genUnaryInstr(Call, M.Name+"_main"),
		genBinInstr(Xor, "rdi", "rdi"), // EXIT CODE 0
		genBinInstr(Mov, "rax", "60"),  // EXIT
		{Instr: Syscall},
	}
}

func genProc(sy *ir.Symbol) *fasmProc {
	proc := sy.Proc
	proc.ResetVisited()
	stackReserve := 8 * (proc.NumOfVars + proc.NumOfSpills + proc.NumOfMaxCalleeArguments)
	init := &fasmBlock{
		label: sy.ModuleName + "_" + proc.Name,
		code: []*amd64Instr{
			genUnaryInstr(Push, "rbp"),
			genBinInstr(Mov, "rbp", "rsp"),
			genBinInstr(Sub, "rsp", strconv.FormatInt(int64(stackReserve), 10)),
		},
	}
	fproc := &fasmProc{label: sy.ModuleName + "_" + proc.Name, blocks: []*fasmBlock{init}}
	fproc.blocks = append(fproc.blocks, genBlocks(proc, proc.Code)...)
	return fproc
}

func genBlocks(proc *ir.Proc, start *ir.BasicBlock) []*fasmBlock {
	trueBranches := []*ir.BasicBlock{}
	falseBlocks := genFalseBranches(proc, start, &trueBranches)
	for _, tBlock := range trueBranches {
		out := genBlocks(proc, tBlock)
		falseBlocks = append(falseBlocks, out...)
	}
	return falseBlocks
}

func genFalseBranches(proc *ir.Proc, block *ir.BasicBlock, trueBranches *[]*ir.BasicBlock) []*fasmBlock {
	if block.Visited {
		panic("no blocks should be already visited")
	}
	block.Visited = true
	fb := genCode(proc, block)

	// should generate Jmp only for true branches and Jmps that point to
	// already visited blocks
	switch block.Out.T {
	case FT.Jmp:
		if block.Out.True.Visited {
			jmp := genUnaryInstr(Jmp, block.Out.True.Label)
			fb.code = append(fb.code, jmp)
			return []*fasmBlock{fb}
		}
		out := genFalseBranches(proc, block.Out.True, trueBranches)
		out = append([]*fasmBlock{fb}, out...)
		return out
	case FT.If:
		if !block.Out.True.Visited {
			*trueBranches = append(*trueBranches, block.Out.True)
		}
		jmp := genCondJmp(proc, block.Out.True, block.Out.V[0])
		fb.code = append(fb.code, jmp...)
		out := genFalseBranches(proc, block.Out.False, trueBranches)
		out = append([]*fasmBlock{fb}, out...)
		return out
	case FT.Exit:
		exit := genExit(proc, block.Out.V[0])
		fb.code = append(fb.code, exit...)
		return []*fasmBlock{fb}
	case FT.Return:
		ret := genRet()
		fb.code = append(fb.code, ret...)
		return []*fasmBlock{fb}
	}
	panic("Invalid flow: " + block.Out.String())
}

func genCondJmp(proc *ir.Proc, block *ir.BasicBlock, op *ir.Operand) []*amd64Instr {
	newOp := convertOperandProc(proc, op)
	if op.Mirc == mirc.Lit || op.Mirc == mirc.Static {
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

func genExit(proc *ir.Proc, op *ir.Operand) []*amd64Instr {
	exitCode := convertOperandProc(proc, op)
	return []*amd64Instr{
		genBinInstr(Xor, "rdi", "rdi"),
		genBinInstr(Mov, "dil", exitCode), // EXIT CODE
		genBinInstr(Mov, "rax", "60"),     // EXIT
		{Instr: Syscall},
	}
}

func genCode(proc *ir.Proc, block *ir.BasicBlock) *fasmBlock {
	output := make([]*amd64Instr, len(block.Code))[:0]
	for _, instr := range block.Code {
		output = append(output, genInstr(proc, instr)...)
	}
	return &fasmBlock{label: block.Label, code: output}
}

func genInstr(proc *ir.Proc, instr *ir.Instr) []*amd64Instr {
	switch instr.T {
	case IT.Add, IT.Sub, IT.Mult, IT.Or, IT.And:
		return genBin(proc, instr)
	case IT.Eq, IT.Diff, IT.Less, IT.More, IT.LessEq, IT.MoreEq:
		return genComp(proc, instr)
	case IT.Load, IT.Store, IT.Copy:
		return genLoadStore(proc, instr)
	case IT.LoadPtr:
		return genLoadPtr(proc, instr)
	case IT.StorePtr:
		return genStorePtr(proc, instr)
	case IT.UnaryMinus:
		return genUnaryMinus(proc, instr)
	case IT.Div:
		return genDiv(proc, instr)
	case IT.Rem:
		return genRem(proc, instr)
	case IT.Not:
		return genNot(proc, instr)
	case IT.Convert:
		return genConvert(proc, instr)
	case IT.Call:
		return genCall(proc, instr)
	case IT.UnaryPlus:
		// does nothing
		return []*amd64Instr{}
	default:
		panic("unimplemented: " + instr.String())
	}
}

func genConvert(proc *ir.Proc, instr *ir.Instr) []*amd64Instr {
	a := instr.Operands[0]
	dest := instr.Destination[0]

	if dest.Type.Size() > a.Type.Size() {
		newA := convertOperandProc(proc, a)
		newDest := convertOperandProc(proc, dest)
		return []*amd64Instr{
			genBinInstr(Movsx, newDest, newA),
		}
	}
	if a.Mirc == mirc.Lit {
		newA := convertOperandProc(proc, a)
		newDest := convertOperandProc(proc, dest)
		return []*amd64Instr{
			genBinInstr(Mov, newDest, newA),
		}
	}
	if a.Mirc == mirc.Static {
		newA := convertOperandProc(proc, a)
		newDest := convertOperandProc(proc, dest)
		res := genReg(RAX, dest.Type)
		return []*amd64Instr{
			genBinInstr(Mov, "rax", newA),
			genBinInstr(Mov, newDest, res),
		}
	}
	if !areOpEqual(a, dest) {
		newA := getReg(a.Num, dest.Type)
		newDest := convertOperandProc(proc, dest)
		return []*amd64Instr{
			genBinInstr(Mov, newDest, newA),
		}
	}
	return []*amd64Instr{}
}

func genCall(proc *ir.Proc, instr *ir.Instr) []*amd64Instr {
	newA := convertOperandProc(proc, instr.Operands[0])
	return []*amd64Instr{
		genUnaryInstr(Call, newA),
	}
}

func genLoadStore(proc *ir.Proc, instr *ir.Instr) []*amd64Instr {
	newA := convertOperandProc(proc, instr.Operands[0])
	newDest := convertOperandProc(proc, instr.Destination[0])
	return []*amd64Instr{
		genMov(newDest, newA),
	}
}

func genLoadPtr(proc *ir.Proc, instr *ir.Instr) []*amd64Instr {
	newA := convertOperandProc(proc, instr.Operands[0])
	newDest := convertOperandProc(proc, instr.Destination[0])
	return []*amd64Instr{
		genMov(newDest, genType(instr.Type)+"["+newA+"]"), // xD
	}
}

func genStorePtr(proc *ir.Proc, instr *ir.Instr) []*amd64Instr {
	newA := convertOperandProc(proc, instr.Operands[0])
	newDest := convertOperandProc(proc, instr.Operands[1])
	return []*amd64Instr{
		genMov(genType(instr.Type)+"["+newDest+"]", newA),
	}
}

func genNot(proc *ir.Proc, instr *ir.Instr) []*amd64Instr {
	newA := convertOperandProc(proc, instr.Operands[0])
	newDest := convertOperandProc(proc, instr.Destination[0])
	return []*amd64Instr{
		genBinInstr(Cmp, newA, "0"),
		genUnaryInstr(Sete, newDest),
	}
}

func genUnaryMinus(proc *ir.Proc, instr *ir.Instr) []*amd64Instr {
	newA := convertOperandProc(proc, instr.Operands[0])
	newDest := convertOperandProc(proc, instr.Destination[0])
	return []*amd64Instr{
		genMov(newDest, newA),
		genUnaryInstr(Neg, newDest),
	}
}

var compInstrMap = map[IT.InstrType]string{
	IT.Eq:     Sete,
	IT.Diff:   Setne,
	IT.Less:   Setl,
	IT.More:   Setg,
	IT.MoreEq: Setge,
	IT.LessEq: Setle,
}

func genComp(proc *ir.Proc, instr *ir.Instr) []*amd64Instr {
	a := instr.Operands[0]
	newOp1 := convertOperandProc(proc, a)
	newOp2 := convertOperandProc(proc, instr.Operands[1])
	newDest := convertOperandProc(proc, instr.Destination[0])
	newInstr := compInstrMap[instr.T]
	if a.Mirc == mirc.Lit || a.Mirc == mirc.Static {
		rbx := genReg(RBX, a.Type)
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

func genDiv(proc *ir.Proc, instr *ir.Instr) []*amd64Instr {
	b := instr.Operands[1]
	newOp1 := convertOperandProc(proc, instr.Operands[0])
	newOp2 := convertOperandProc(proc, b)
	newDest := convertOperandProc(proc, instr.Destination[0])
	if b.Mirc == mirc.Lit || b.Mirc == mirc.Static {
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

func genRem(proc *ir.Proc, instr *ir.Instr) []*amd64Instr {
	b := instr.Operands[1]
	newOp1 := convertOperandProc(proc, instr.Operands[0])
	newOp2 := convertOperandProc(proc, b)
	newDest := convertOperandProc(proc, instr.Destination[0])
	if b.Mirc == mirc.Lit || b.Mirc == mirc.Static {
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

var BinInstrMap = map[IT.InstrType]string{
	IT.Add:  Add,
	IT.Sub:  Sub,
	IT.Mult: IMul,
	IT.And:  And,
	IT.Or:   Or,
}

func genBin(proc *ir.Proc, instr *ir.Instr) []*amd64Instr {
	dest, op := convertToTwoAddr(instr)
	newInstr := BinInstrMap[instr.T]
	if dest == nil || op == nil {
		newOp1 := convertOperandProc(proc, instr.Operands[0])
		newOp2 := convertOperandProc(proc, instr.Operands[1])
		newDest := convertOperandProc(proc, instr.Destination[0])
		return []*amd64Instr{
			genMov(newDest, newOp1),
			genBinInstr(newInstr, newDest, newOp2),
		}
	}
	newOp1 := convertOperandProc(proc, op)
	newDest := convertOperandProc(proc, dest)
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
	// subtraction is not associative
	if areOpEqual(b, c) && instr.T != IT.Sub {
		return c, a
	}

	return nil, nil
}

func areOpEqual(a, b *ir.Operand) bool {
	return a.Mirc == b.Mirc &&
		a.Num == b.Num &&
		a.Symbol == b.Symbol
}

func convertOperandProc(proc *ir.Proc, op *ir.Operand) string {
	return convertOperand(op, int64(proc.NumOfVars), int64(proc.NumOfSpills), int64(proc.NumOfMaxCalleeArguments))
}

func convertOperand(op *ir.Operand, NumOfVars, NumOfSpills, NumOfMaxCalleeArguments int64) string {
	switch op.Mirc {
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
		if op.Symbol.T == ST.Builtin {
			return "_" + op.Symbol.Name
		}
		return op.Symbol.ModuleName + "_" + op.Symbol.Name
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
