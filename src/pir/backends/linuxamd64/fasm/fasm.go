package fasm

import (
	mir "mpc/pir/backends/linuxamd64/mir"
	mirc "mpc/pir/backends/linuxamd64/mir/class"
	FT "mpc/pir/backends/linuxamd64/mir/flowkind"
	IT "mpc/pir/backends/linuxamd64/mir/instrkind"
	T "mpc/pir/types"

	"fmt"
	"math/big"
	"strconv"
)

func Generate(P *mir.Program, outname string) *FasmProgram {
	output := &fasmProgram{
		executable: []*fasmProc{},
		data:       []*fasmData{},
		entry:      genEntry(P),
	}
	for _, sy := range P.Symbols {
		if sy.Proc != nil {
			proc := genProc(P, sy.Proc)
			output.executable = append(output.executable, proc)
		}
		if sy.Mem != nil {
			mem := genMem(sy.Mem)
			output.data = append(output.data, mem)
		}
	}
	if outname == "" {
		outname = P.Name
	}
	return &FasmProgram{
		Contents: output.String(),
		Name:     outname,
	}
}

type llist struct {
	s    []byte
	next *llist
}

type builder struct {
	head *llist
	curr *llist
}

func (this *builder) Place(s string) {
	new := &llist{
		s:    []byte(s),
		next: nil,
	}
	if this.curr != nil {
		this.curr.next = new
	}
	this.curr = new
	if this.head == nil {
		this.head = new
	}
}

func (this *builder) String() string {
	size := this.getSize()
	buff := make([]byte, size)
	index := 0
	curr := this.head
	for curr != nil {
		copy(buff[index:], curr.s)
		index += len(curr.s)
		curr = curr.next
	}
	return string(buff)
}

func (this *builder) getSize() int {
	output := 0
	curr := this.head
	for curr != nil {
		output += len(curr.s)
		curr = curr.next
	}
	return output
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
	b := &builder{}
	b.Place("format ELF64 executable 3\n")
	b.Place("\nsegment readable writable\n")
	for _, decData := range this.data {
		decData.Str(b)
	}
	b.Place("\nsegment readable executable\n")
	b.Place("entry $\n")
	for _, instr := range this.entry {
		b.Place("\t")
		instr.Str(b)
		b.Place("\n")
	}
	b.Place("; ---- end \n")
	for _, fproc := range this.executable {
		fproc.Str(b)
	}
	return b.String()
}

type fasmData struct {
	label    string
	content  string
	dataSize string
	declared bool
}

func (this *fasmData) Str(b *builder) {
	b.Place(this.label)
	if this.declared {
		b.Place(" d")
	} else {
		b.Place(" r")
	}
	b.Place(this.dataSize)
	b.Place(" ")
	b.Place(this.content)
	b.Place("\n")
}

type fasmProc struct {
	label  string
	blocks []*fasmBlock // order matters
}

func (this *fasmProc) Str(b *builder) {
	for _, bl := range this.blocks {
		bl.Str(b)
		b.Place("\n")
	}
}

type fasmBlock struct {
	label string
	code  []*amd64Instr // order matters ofc
}

func (this *fasmBlock) Str(b *builder) {
	b.Place(this.label)
	b.Place(":\n")
	for _, instr := range this.code {
		b.Place("\t")
		instr.Str(b)
		b.Place("\n")
	}
}

type InstrType int

type amd64Instr struct {
	Instr string
	Op1   string
	Op2   string
}

func (this *amd64Instr) Str(b *builder) {
	if this.Instr == "" {
		b.Place("???")
		return
	}
	if this.Op1 == "" {
		b.Place(this.Instr)
		return
	}
	if this.Op2 == "" {
		b.Place(this.Instr)
		b.Place("\t")
		b.Place(this.Op1)
		return
	}
	b.Place(this.Instr)
	b.Place("\t")
	b.Place(this.Op1)
	b.Place(", ")
	b.Place(this.Op2)
	return
}

const (
	Add = "add"
	Sub = "sub"
	Neg = "neg"
	//signed
	IMul = "imul"
	IDiv = "idiv"
	//unsigned
	Div = "div"

	Mov    = "mov"
	Movsx  = "movsx" // sign extend
	Movzx  = "movzx" // zero extend
	Movsxd = "movsxd"
	Push   = "push"
	Pop    = "pop"

	Not = "not"
	And = "and"
	Or  = "or"
	Xor = "xor"
	Sal = "sal"
	Shl = "shl"
	Sar = "sar"
	Shr = "shr"

	Cmp = "cmp"

	Sete  = "sete"
	Setne = "setne"

	// signed
	Setg  = "setg"
	Setge = "setge"
	Setl  = "setl"
	Setle = "setle"
	// unsigned
	Seta  = "seta"
	Setae = "setae"
	Setb  = "setb"
	Setbe = "setbe"

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

// we use this three as scratch space,
// IDIV already needs rax and rdx, and SAR already needs RCX,
// and very rarely will a block need more than 3~5 registers
var RAX = &register{QWord: "rax", DWord: "eax", Word: "ax", Byte: "al"}
var RDX = &register{QWord: "rdx", DWord: "edx", Word: "dx", Byte: "dl"}
var RCX = &register{QWord: "rcx", DWord: "ecx", Word: "cx", Byte: "cl"}

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
	{QWord: "rbx", DWord: "ebx", Word: "bx", Byte: "bl"},
}

func genMem(mem *mir.DataDecl) *fasmData {
	if mem.Data != "" {
		return &fasmData{
			label:    mem.Label,
			content:  convertString(mem.Data),
			dataSize: "b",
			declared: true,
		}
	}
	if mem.Nums != nil {
		return &fasmData{
			label:    mem.Label,
			content:  convertNums(mem.Nums),
			dataSize: genDataSize(mem.DataSize),
			declared: true,
		}
	}
	return &fasmData{
		label:    mem.Label,
		content:  mem.Size.Text(10),
		dataSize: "b",
		declared: false,
	}
}

func genDataSize(size int) string {
	switch size {
	case 1:
		return "b" // [b]yte
	case 2:
		return "w" // [w]ord
	case 4:
		return "d" // [d]ouble word
	case 8:
		return "q" // [q]uad word
	default:
		panic("unknown size")
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

var zero = big.NewInt(0)

func convertNums(nums []*big.Int) string {
	b := builder{}
	scratch := big.NewInt(0)
	for i, num := range nums {
		if num.Cmp(zero) == -1 {
			b.Place("-")
		}
		b.Place("0x")
		b.Place(scratch.Abs(num).Text(16))
		if i < len(nums)-1 {
			b.Place(",")
		}
	}
	return b.String()
}

func genEntry(P *mir.Program) []*amd64Instr {
	entry := P.Symbols[P.Entry]
	if entry == nil || entry.Proc == nil {
		panic("nil entrypoint")
	}
	return []*amd64Instr{
		unary(Call, entry.Proc.Label),
		bin(Xor, "rdi", "rdi"), // EXIT CODE 0
		bin(Mov, "rax", "60"),  // EXIT
		{Instr: Syscall},
	}
}

func genProc(P *mir.Program, proc *mir.Procedure) *fasmProc {
	stackReserve := 8 * (proc.NumOfVars + proc.NumOfSpills + proc.NumOfMaxCalleeArguments)
	init := &fasmBlock{
		label: proc.Label,
		code: []*amd64Instr{
			unary(Push, "rbp"),
			bin(Mov, "rbp", "rsp"),
			bin(Sub, "rsp", strconv.FormatInt(int64(stackReserve), 10)),
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
			jmp := unary(Jmp, t.Label)
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

func genCondJmp(P *mir.Program, proc *mir.Procedure, block *mir.BasicBlock, op mir.Operand) []*amd64Instr {
	newOp := convertOperandProc(P, proc, op)
	if op.Class == mirc.Lit || op.Class == mirc.Static {
		rcx := _genReg(RCX, op.Type)
		return []*amd64Instr{
			bin(Mov, rcx, newOp),
			bin(Cmp, rcx, "1"),
			unary(Je, block.Label),
		}
	}
	return []*amd64Instr{
		bin(Cmp, newOp, "1"),
		unary(Je, block.Label),
	}
}

func genRet() []*amd64Instr {
	return []*amd64Instr{
		bin(Mov, "rsp", "rbp"),
		unary(Pop, "rbp"),
		{Instr: Ret},
	}
}

func genExit(P *mir.Program, proc *mir.Procedure, op mir.Operand) []*amd64Instr {
	exitCode := convertOperandProc(P, proc, op)
	return []*amd64Instr{
		bin(Xor, "rdi", "rdi"),
		bin(Mov, "dil", exitCode), // EXIT CODE
		bin(Mov, "rax", "60"),     // EXIT
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

func genInstr(P *mir.Program, proc *mir.Procedure, instr mir.Instr) []*amd64Instr {
	switch instr.T {
	case IT.Add:
		return genAdd(P, proc, instr)
	case IT.Sub:
		return genSub(P, proc, instr)
	case IT.Or, IT.And, IT.Xor:
		return genSimpleBin(P, proc, instr)
	case IT.ShiftLeft, IT.ShiftRight:
		return genShift(P, proc, instr)
	case IT.Mult:
		return genMul(P, proc, instr)
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

// convert immediate -> reg
func genConvert(P *mir.Program, proc *mir.Procedure, instr mir.Instr) []*amd64Instr {
	if instr.A.Class == mirc.Lit || instr.A.Class == mirc.Static {
		out, newA := resolveOperand(P, proc, instr.A.Operand)
		rcx := _genReg(RCX, instr.A.Type)
		out = append(out, bin(Mov, rcx, newA))

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
		return []*amd64Instr{}
	}
	dest := getReg(instr.Dest.Op())
	source := getReg(instr.A.Op())
	return []*amd64Instr{
		mov_t(
			dest, source,
			instr.Dest.Type.Size(), instr.A.Type.Size(),
			T.IsSigned(instr.Dest.Type), T.IsSigned(instr.A.Type),
		),
	}
}

func genCall(P *mir.Program, proc *mir.Procedure, instr mir.Instr) []*amd64Instr {
	newA := convertOptOperandProc(P, proc, instr.A)
	return []*amd64Instr{
		unary(Call, newA),
	}
}

func genLoadStore(P *mir.Program, proc *mir.Procedure, instr mir.Instr) []*amd64Instr {
	out, newA := resolveOperand(P, proc, instr.A.Operand)
	newDest := convertOptOperandProc(P, proc, instr.Dest)
	out = append(out, mov(newDest, newA))
	return out
}

func genLoadPtr(P *mir.Program, proc *mir.Procedure, instr mir.Instr) []*amd64Instr {
	newA := convertOptOperandProc(P, proc, instr.A)
	newDest := convertOptOperandProc(P, proc, instr.Dest)
	a := mov(newDest, genType(instr.Type)+"["+newA+"]") // xD
	return []*amd64Instr{a}
}

func genStorePtr(P *mir.Program, proc *mir.Procedure, instr mir.Instr) []*amd64Instr {
	out, newA := resolveOperand(P, proc, instr.A.Operand)
	newDest := convertOptOperandProc(P, proc, instr.B)
	a := mov(genType(instr.Type)+"["+newDest+"]", newA)
	out = append(out, a)
	return out
}

func genNot(P *mir.Program, proc *mir.Procedure, instr mir.Instr) []*amd64Instr {
	if instr.Type.Basic == T.Bool {
		newA := convertOptOperandProc(P, proc, instr.A)
		newDest := convertOptOperandProc(P, proc, instr.Dest)
		return []*amd64Instr{
			bin(Cmp, newA, "0"),
			unary(Sete, newDest),
		}
	}
	out, newA := resolveOperand(P, proc, instr.A.Operand)
	newDest := convertOptOperandProc(P, proc, instr.Dest)
	out = append(out, []*amd64Instr{
		mov(newDest, newA),
		unary(Not, newDest),
	}...)
	return out
}

func genUnaryMinus(P *mir.Program, proc *mir.Procedure, instr mir.Instr) []*amd64Instr {
	out, newA := resolveOperand(P, proc, instr.A.Operand)
	newDest := convertOptOperandProc(P, proc, instr.Dest)
	out = append(out, []*amd64Instr{
		mov(newDest, newA),
		unary(Neg, newDest),
	}...)
	return out
}

func genComp(P *mir.Program, proc *mir.Procedure, instr mir.Instr) []*amd64Instr {
	newOp1 := convertOptOperandProc(P, proc, instr.A)
	out, newOp2 := resolveOperand(P, proc, instr.B.Operand)
	newDest := convertOptOperandProc(P, proc, instr.Dest)
	newInstr := genInstrName(instr)
	if instr.A.Class == mirc.Lit || instr.A.Class == mirc.Static {
		rax := _genReg(RAX, instr.A.Type)
		out = append(out, []*amd64Instr{
			bin(Mov, rax, newOp1),
			bin(Cmp, rax, newOp2),
			unary(newInstr, newDest),
		}...)
		return out
	}
	out = append(out, []*amd64Instr{
		bin(Cmp, newOp1, newOp2),
		unary(newInstr, newDest),
	}...)
	return out
}

func signExtendRCX(instr mir.Instr) (rcx string, op string) {
	rcx = _genReg(RCX, instr.Type)
	op = "nop"
	switch instr.Type.Size() {
	case 1, 2, 4:
		rcx = "ecx"
		op = "cdq"
	case 8:
		if T.IsSigned(instr.Type) {
			rcx = "rcx"
			op = "cqo"
		}
	}
	return rcx, op
}

func signExtend(P *mir.Program, proc *mir.Procedure, instr mir.Instr, opnd mir.OptOperand) (operand string, operation string) {
	operand = convertOptOperandProc(P, proc, instr.B)
	operation = "nop"
	switch instr.Type.Size() {
	case 1, 2, 4:
		opnd.Type = T.T_I32
		operand = convertOptOperandProc(P, proc, opnd)
		operation = "cdq"
	case 8:
		if T.IsSigned(instr.Type) {
			operand = convertOptOperandProc(P, proc, instr.B)
			operation = "cqo"
		}
	}
	return operand, operation
}

func genDiv(P *mir.Program, proc *mir.Procedure, instr mir.Instr) []*amd64Instr {
	instrName := genInstrName(instr)
	newOp1 := convertOptOperandProc(P, proc, instr.A)
	newOp2 := convertOptOperandProc(P, proc, instr.B)
	newDest := convertOptOperandProc(P, proc, instr.Dest)
	if instr.B.Class == mirc.Lit || instr.B.Class == mirc.Static {
		rcx, op := signExtendRCX(instr)
		return []*amd64Instr{
			bin(Xor, RDX.QWord, RDX.QWord),
			mov(_genReg(RAX, instr.Type), newOp1),
			mov(_genReg(RCX, instr.Type), newOp2),
			{Instr: op},
			unary(instrName, rcx),
			mov(newDest, _genReg(RAX, instr.Type)),
		}
	}
	newOp2, op := signExtend(P, proc, instr, instr.B)
	return []*amd64Instr{
		bin(Xor, RDX.QWord, RDX.QWord),
		mov(_genReg(RAX, instr.Type), newOp1),
		{Instr: op},
		unary(instrName, newOp2),
		mov(newDest, _genReg(RAX, instr.Type)),
	}
}

func genRem(P *mir.Program, proc *mir.Procedure, instr mir.Instr) []*amd64Instr {
	instrName := genInstrName(instr)

	newOp1 := convertOptOperandProc(P, proc, instr.A)
	newOp2 := convertOptOperandProc(P, proc, instr.B)
	newDest := convertOptOperandProc(P, proc, instr.Dest)
	if mirc.IsImmediate(instr.B.Class) {
		rcx, op := signExtendRCX(instr)
		return []*amd64Instr{
			bin(Xor, RDX.QWord, RDX.QWord),
			mov(_genReg(RAX, instr.Type), newOp1),
			mov(_genReg(RCX, instr.Type), newOp2),
			{Instr: op}, // may god help me
			unary(instrName, rcx),
			mov(newDest, _genReg(RDX, instr.Type)),
		}
	}
	newOp2, op := signExtend(P, proc, instr, instr.B)
	return []*amd64Instr{
		bin(Xor, RDX.QWord, RDX.QWord),
		mov(_genReg(RAX, instr.Type), newOp1),
		{Instr: op},
		unary(instrName, newOp2),
		mov(newDest, _genReg(RDX, instr.Type)),
	}
}

// given P+N:
//      if N is i8~i32 we sign-extend
//      if N is i64 we add normally
//      if N is u8~u16 we zero-extend
//      if N is u32 we mov to higher register
//      if N is u64 we add normally
func genAdd(P *mir.Program, proc *mir.Procedure, instr mir.Instr) []*amd64Instr {
	if !T.IsPtr(instr.A.Type) && !T.IsPtr(instr.B.Type) {
		return genSimpleBin(P, proc, instr)
	}
	out := []*amd64Instr{}
	if T.IsPtr(instr.A.Type) {
		ptrOp := convertOptOperandProc(P, proc, instr.A)
		intOp := _genReg(RAX, T.T_Ptr)

		switch instr.B.Class {
		case mirc.Lit, mirc.Static:
			op := convertOptOperandProc(P, proc, instr.B)
			mv := mov(intOp, op)
			out = append(out, mv)
		case mirc.Register:
			breg := getReg(instr.B.Op())
			mv := mov_t(RAX, breg, T.T_Ptr.Size(), instr.B.Type.Size(), true, T.IsSigned(instr.B.Type))
			out = append(out, mv)
		}

		if areOpEqual(instr.A, instr.Dest) { // the output must be a pointer
			add := bin(Add, ptrOp, intOp)
			out = append(out, add)
			return out
		} else {
			destOp := convertOptOperandProc(P, proc, instr.Dest)
			mv := mov(destOp, ptrOp)
			out = append(out, mv)
			add := bin(Add, destOp, intOp)
			out = append(out, add)
			return out
		}
	}
	if T.IsPtr(instr.B.Type) {
		ptrOp := convertOptOperandProc(P, proc, instr.B)
		intOp := _genReg(RAX, T.T_Ptr)

		switch instr.A.Class {
		case mirc.Lit, mirc.Static:
			op := convertOptOperandProc(P, proc, instr.A)
			mv := mov(intOp, op)
			out = append(out, mv)
		case mirc.Register:
			areg := getReg(instr.A.Op())
			mv := mov_t(RAX, areg, T.T_Ptr.Size(), instr.A.Type.Size(), true, T.IsSigned(instr.A.Type))
			out = append(out, mv)
		}
		if areOpEqual(instr.B, instr.Dest) { // the output must be a pointer
			add := bin(Add, ptrOp, intOp)
			out = append(out, add)
			return out
		} else {
			destOp := convertOptOperandProc(P, proc, instr.Dest)
			mv := mov(destOp, ptrOp)
			out = append(out, mv)
			add := bin(Add, destOp, intOp)
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
func genSub(P *mir.Program, proc *mir.Procedure, instr mir.Instr) []*amd64Instr {
	if T.IsPtr(instr.A.Type) {
		out := []*amd64Instr{}
		ptrOp := convertOptOperandProc(P, proc, instr.A)
		intOp := _genReg(RAX, T.T_Ptr)

		switch instr.B.Class {
		case mirc.Lit, mirc.Static:
			op := convertOptOperandProc(P, proc, instr.B)
			mv := mov(intOp, op)
			out = append(out, mv)
		case mirc.Register:
			breg := getReg(instr.B.Op())
			mv := mov_t(RAX, breg, T.T_Ptr.Size(), instr.B.Type.Size(), true, T.IsSigned(instr.B.Type))
			out = append(out, mv)
		}

		neg := unary(Neg, _genReg(RAX, T.T_Ptr))
		out = append(out, neg)

		if areOpEqual(instr.A, instr.Dest) { // the output must be a pointer
			add := bin(Add, ptrOp, intOp)
			out = append(out, add)
			return out
		} else {
			destOp := convertOptOperandProc(P, proc, instr.Dest)
			mv := mov(destOp, ptrOp)
			out = append(out, mv)
			add := bin(Add, destOp, intOp)
			out = append(out, add)
			return out
		}
	} else {
		if areOpEqual(instr.A, instr.Dest) {
			out, newOp1 := resolveOperand(P, proc, instr.B.Operand)
			newDest := convertOptOperandProc(P, proc, instr.Dest)
			out = append(out, []*amd64Instr{
				bin(Sub, newDest, newOp1),
			}...)
			return out
		}

		if areOpEqual(instr.B, instr.Dest) {
			rax := _genReg(RAX, instr.Type)
			newOp1 := convertOptOperandProc(P, proc, instr.A)
			out, newOp2 := resolveOperand(P, proc, instr.B.Operand)
			newDest := convertOptOperandProc(P, proc, instr.Dest)
			out = append(out, []*amd64Instr{
				bin(Xor, RAX.QWord, RAX.QWord),
				mov(rax, newOp1),
				bin(Sub, rax, newOp2),
				mov(newDest, rax),
			}...)
			return out
		}

		newOp1 := convertOptOperandProc(P, proc, instr.A)
		out, newOp2 := resolveOperand(P, proc, instr.B.Operand)
		newDest := convertOptOperandProc(P, proc, instr.Dest)
		out = append(out, []*amd64Instr{
			mov(newDest, newOp1),
			bin(Sub, newDest, newOp2),
		}...)
		return out
	}
}

func genMul(P *mir.Program, proc *mir.Procedure, instr mir.Instr) []*amd64Instr {
	newInstr := genInstrName(instr)
	if instr.Type.Size() == 1 {
		newOp1 := convertOptOperandProc(P, proc, instr.A)
		newOp2 := convertOptOperandProc(P, proc, instr.B)
		newDest := convertOptOperandProc(P, proc, instr.Dest)
		rax := _genReg(RAX, instr.Type)
		return []*amd64Instr{
			bin(Xor, RAX.QWord, RAX.QWord),
			mov(rax, newOp1),
			unary(newInstr, newOp2),
			mov(newDest, rax),
		}
	}
	dest, op, ok := convertToTwoAddr(instr)
	if ok {
		out, newOp1 := resolveOperand(P, proc, op)
		newDest := convertOperandProc(P, proc, dest)
		out = append(out, []*amd64Instr{
			bin(newInstr, newDest, newOp1),
		}...)
		return out
	}
	newOp1 := convertOptOperandProc(P, proc, instr.A)
	// we only need to resolve it for the second one, the first is already going in a register
	out, newOp2 := resolveOperand(P, proc, instr.B.Operand)
	newDest := convertOptOperandProc(P, proc, instr.Dest)
	out = append(out, []*amd64Instr{
		mov(newDest, newOp1),
		bin(newInstr, newDest, newOp2),
	}...)
	return out
}

func genSimpleBin(P *mir.Program, proc *mir.Procedure, instr mir.Instr) []*amd64Instr {
	dest, op, ok := convertToTwoAddr(instr)
	newInstr := genInstrName(instr)
	if ok {
		out, newOp1 := resolveOperand(P, proc, op)
		newDest := convertOperandProc(P, proc, dest)
		out = append(out, []*amd64Instr{
			bin(newInstr, newDest, newOp1),
		}...)
		return out
	}
	newOp1 := convertOptOperandProc(P, proc, instr.A)
	// we only need to resolve it for the second one, the first is already going in a register
	out, newOp2 := resolveOperand(P, proc, instr.B.Operand)
	newDest := convertOptOperandProc(P, proc, instr.Dest)
	out = append(out, []*amd64Instr{
		mov(newDest, newOp1),
		bin(newInstr, newDest, newOp2),
	}...)
	return out
}

// shifts only accept CL as registers, yay amd64!
func genShift(P *mir.Program, proc *mir.Procedure, instr mir.Instr) []*amd64Instr {
	dest, op, ok := convertToTwoAddr(instr)
	newInstr := genInstrName(instr)
	if ok {
		out, newOp1 := resolveOperand(P, proc, op)
		if mirc.IsRegister(op.Class) {
			out = append(out, mov_opToReg(RCX, 1, false, op))
			newOp1 = "cl"
		}
		newDest := convertOperandProc(P, proc, dest)
		out = append(out, []*amd64Instr{
			bin(newInstr, newDest, newOp1),
		}...)
		return out
	}
	newOp1 := convertOptOperandProc(P, proc, instr.A)
	// we only need to resolve it for the second one, the first is already going in a register
	out, newOp2 := resolveOperand(P, proc, instr.B.Operand)
	if mirc.IsRegister(instr.B.Class) {
		out = append(out, mov_opToReg(RCX, 1, false, instr.B.Op()))
		newOp2 = "cl"
	}
	newDest := convertOptOperandProc(P, proc, instr.Dest)
	out = append(out, []*amd64Instr{
		mov(newDest, newOp1),
		bin(newInstr, newDest, newOp2),
	}...)
	return out
}

func mov_opToReg(
	dest *register, destSize int, destSigned bool,
	op mir.Operand,
) *amd64Instr {
	source := getReg(op)
	sourceSize := op.Type.Size()
	sourceSigned := T.IsSigned(op.Type)
	return mov_t(dest, source, destSize, sourceSize, destSigned, sourceSigned)
}

func mov_t(
	dest, source *register,
	destSize, sourceSize int,
	destSigned, sourceSigned bool,
) *amd64Instr {
	if destSize == sourceSize {
		return mov(genReg_s(dest, destSize), genReg_s(source, sourceSize))
	}
	if destSize > sourceSize {
		d := genReg_s(dest, destSize)
		s := genReg_s(source, sourceSize)
		if destSize == 8 && sourceSize == 4 {
			if destSigned && sourceSigned {
				return bin(Movsxd, d, s)
			} else {
				// when mov'ing 32 bit regiters, the upper 32bits are set to zero
				// https://stackoverflow.com/questions/51387571/movzx-missing-32-bit-register-to-64-bit-register
				return bin(Mov, genReg_s(dest, sourceSize), genReg_s(source, sourceSize))
			}
		} else {
			if destSigned && sourceSigned {
				return bin(Movsx, d, s)
			} else {
				return bin(Movzx, d, s)
			}
		}
	}
	return mov(genReg_s(dest, destSize), genReg_s(source, destSize)) // truncates the value :)
}

func unary(instr string, op string) *amd64Instr {
	return &amd64Instr{
		Instr: instr,
		Op1:   op,
	}
}

func bin(instr string, dest, source string) *amd64Instr {
	return &amd64Instr{
		Instr: instr,
		Op1:   dest,
		Op2:   source,
	}
}

func mov(dest, source string) *amd64Instr {
	return &amd64Instr{
		Instr: Mov,
		Op1:   dest,
		Op2:   source,
	}
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

func convertOperandProc(P *mir.Program, proc *mir.Procedure, op mir.Operand) string {
	return convertOperand(P, op, int64(proc.NumOfVars), int64(proc.NumOfSpills), int64(proc.NumOfMaxCalleeArguments))
}

func convertOptOperandProc(P *mir.Program, proc *mir.Procedure, op mir.OptOperand) string {
	if !op.Valid {
		panic("invalid operand")
	}
	return convertOperand(P, op.Operand, int64(proc.NumOfVars), int64(proc.NumOfSpills), int64(proc.NumOfMaxCalleeArguments))
}

func convertOperand(P *mir.Program, op mir.Operand, NumOfVars, NumOfSpills, NumOfMaxCalleeArguments int64) string {
	switch op.Class {
	case mirc.Register:
		return genReg(op.ID, op.Type)
	case mirc.CallerInterproc:
		//        v must jump last rbp + return address
		offset := 16 + op.ID*8
		return genType(op.Type) + "[rbp + " + strconv.FormatInt(offset, 10) + "]"
	case mirc.Local:
		//        v begins at 8 because rbp points to the last rbp
		offset := 8 + op.ID*8
		return genType(op.Type) + "[rbp - " + strconv.FormatInt(offset, 10) + "]"
	case mirc.Spill:
		offset := 8 + NumOfVars*8 + op.ID*8
		return genType(op.Type) + "[rbp - " + strconv.FormatInt(offset, 10) + "]"
	case mirc.CalleeInterproc:
		offset := 8 + NumOfVars*8 +
			NumOfSpills*8 +
			// v count                   v index
			(NumOfMaxCalleeArguments-1-op.ID)*8
		return genType(op.Type) + "[rbp - " + strconv.FormatInt(offset, 10) + "]"
	case mirc.Lit:
		if op.Num == nil {
			panic("bignum was nil")
		}
		return op.Num.Text(10)
	case mirc.Static:
		sy := P.Symbols[op.ID]
		if sy.Proc != nil {
			return sy.Proc.Label
		}
		return sy.Mem.Label
	}
	panic("unimplemented: " + op.String())
}

func getReg(op mir.Operand) *register {
	num := op.ID
	if num > int64(len(Registers)) || num < 0 {
		fmt.Println(op)
		panic("oh no")
	}
	return Registers[num]
}

func genReg(num int64, t *T.Type) string {
	if num > int64(len(Registers)) || num < 0 {
		panic("oh no")
	}
	r := Registers[num]
	return _genReg(r, t)
}

func _genReg(r *register, t *T.Type) string {
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

func genReg_s(r *register, size int) string {
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

func genInstrName(instr mir.Instr) string {
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
func resolveOperand(P *mir.Program, proc *mir.Procedure, op mir.Operand) ([]*amd64Instr, string) {
	opstr := convertOperandProc(P, proc, op)
	if op.Class == mirc.Lit &&
		(op.Num.Cmp(min) == -1 || op.Num.Cmp(max) == 1) {
		out := _genReg(RCX, op.Type)
		mv := mov(out, opstr)
		return []*amd64Instr{mv}, out
	}
	return nil, opstr
}
