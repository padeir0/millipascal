package mir

import (
	mirc "mpc/core/mir/class"
	FT "mpc/core/mir/flowkind"
	IT "mpc/core/mir/instrkind"
	T "mpc/core/types"
	"strconv"
	"strings"
)

// index into Program.Symbols array
type SymbolID int

type Program struct {
	Name    string
	Entry   SymbolID
	Symbols []*Symbol
}

func (this *Program) FindSymbol(i SymbolID) *Symbol {
	if i < 0 || int64(i) >= int64(len(this.Symbols)) {
		return nil
	}
	return this.Symbols[i]
}

func (this *Program) AddProc(p *Procedure) int {
	index := len(this.Symbols)
	this.Symbols = append(this.Symbols, &Symbol{Proc: p})
	return index
}

func (this *Program) AddMem(m *MemoryDecl) int {
	index := len(this.Symbols)
	this.Symbols = append(this.Symbols, &Symbol{Mem: m})
	return index
}

func (this *Program) String() string {
	if this == nil {
		return "nil"
	}
	output := "Program: " + this.Name + "\n\n"
	for i, sy := range this.Symbols {
		output += "[" + strconv.FormatInt(int64(i), 10) + "]" + sy.String() + "\n"
	}
	return output
}

type Symbol struct {
	Proc    *Procedure
	Mem     *MemoryDecl
	Builtin bool
}

func (this *Symbol) String() string {
	if this.Builtin {
		return this.Proc.Label + ": " + "builtin"
	}
	if this.Proc != nil {
		return this.Proc.String()
	}
	return this.Mem.String()
}

type MemoryDecl struct {
	Label string
	Data  string
	Size  int64
}

func (this *MemoryDecl) String() string {
	if this.Data != "" {
		return this.Label + ": " + this.Data
	}
	return this.Label + ": " + strconv.FormatInt(this.Size, 10)
}

type Procedure struct {
	Label string
	Vars  []*T.Type
	Args  []*T.Type
	Rets  []*T.Type

	Start     BlockID
	AllBlocks []*BasicBlock

	NumOfVars               int
	NumOfSpills             int
	NumOfMaxCalleeArguments int
}

func (this *Procedure) String() string {
	output := this.Label + "{\n"
	output += this.StrArgs() + "\n"
	output += this.StrRets() + "\n"
	output += this.StrLocals() + "\n"
	output += "}:\n"
	for _, bb := range this.AllBlocks {
		output += bb.String() + "\n"
	}
	return output + "\n"
}

func (p *Procedure) StrRets() string {
	return StrTypes(p.Rets)
}
func (p *Procedure) StrArgs() string {
	return StrTypes(p.Args)
}
func (p *Procedure) StrLocals() string {
	return StrTypes(p.Vars)
}

func (this *Procedure) FirstBlock() *BasicBlock {
	return this.AllBlocks[this.Start]
}

func (this *Procedure) GetBlock(id BlockID) *BasicBlock {
	return this.AllBlocks[id]
}

func (this *Procedure) ResetBlocks() {
	for _, b := range this.AllBlocks {
		b.Visited = false
	}
}

func StrTypes(tps []*T.Type) string {
	if len(tps) == 0 {
		return ""
	}
	if len(tps) == 1 {
		return tps[0].String()
	}
	output := tps[0].String()
	for _, t := range tps {
		output += ", " + t.String()
	}
	return output
}

type BasicBlock struct {
	Label   string
	Code    []*Instr
	Out     Flow
	Visited bool
}

func (b *BasicBlock) AddInstr(i *Instr) {
	b.Code = append(b.Code, i)
}

func (b *BasicBlock) Jmp(o BlockID) {
	b.Out = Flow{
		T:    FT.Jmp,
		V:    nil,
		True: o,
	}
}

func (b *BasicBlock) Branch(cond *Operand, True BlockID, False BlockID) {
	b.Out = Flow{
		T:     FT.If,
		V:     []*Operand{cond},
		True:  True,
		False: False,
	}
}

func (b *BasicBlock) Return(rets []*Operand) {
	b.Out = Flow{
		V: rets,
		T: FT.Return,
	}
}

func (b *BasicBlock) Exit(code *Operand) {
	b.Out = Flow{
		V: []*Operand{code},
		T: FT.Exit,
	}
}

func (b *BasicBlock) HasFlow() bool {
	return b.Out.T != FT.InvalidFlow
}

func (b *BasicBlock) IsTerminal() bool {
	return b.Out.T == FT.Return || b.Out.T == FT.Exit
}

func (b *BasicBlock) String() string {
	output := b.Label + ":\n"
	for _, v := range b.Code {
		output += "\t" + v.String() + "\n"
	}
	output += "\t" + b.Out.String()
	return output
}

// index into Procedure.Blocks
type BlockID int

type Flow struct {
	T     FT.FlowKind
	V     []*Operand
	True  BlockID
	False BlockID
}

func (this *Flow) String() string {
	switch this.T {
	case FT.Jmp:
		t := strconv.FormatInt(int64(this.True), 10)
		return "jmp .L" + t // :)
	case FT.If:
		t := strconv.FormatInt(int64(this.True), 10)
		f := strconv.FormatInt(int64(this.False), 10)
		return "if " + this.StrRets() + "? .L" + t + " : .L" + f
	case FT.Return:
		return "ret " + this.StrRets()
	case FT.Exit:
		return "exit " + this.StrRets()
	}
	return "invalid FlowType"
}

func (f *Flow) StrRets() string {
	output := []string{}
	for _, op := range f.V {
		output = append(output, op.String())
	}
	return strings.Join(output, ", ")
}

type Operand struct {
	Class mirc.Class
	Type  *T.Type
	Num   int64
}

func (o *Operand) String() string {
	if o == nil {
		return "nil"
	}
	value := strconv.FormatInt(o.Num, 10)
	switch o.Class {
	case mirc.Lit:
		return value
	case mirc.Local:
		return "local#" + value + ":" + o.Type.String()
	case mirc.Spill:
		return "spill#" + value + ":" + o.Type.String()
	case mirc.Register:
		return "r" + value + ":" + o.Type.String()
	case mirc.Static:
		return "globall#" + value + ":" + o.Type.String()
	case mirc.CallerInterproc:
		return "caller#" + value + ":" + o.Type.String()
	case mirc.CalleeInterproc:
		return "callee#" + value + ":" + o.Type.String()
	}
	return "?"
}

type Instr struct {
	T    IT.InstrKind
	Type *T.Type
	A    *Operand
	B    *Operand
	Dest *Operand
}

func (this *Instr) String() string {
	if this == nil {
		return "nil"
	}
	output := this.T.String()
	if this.Type != nil {
		output += ":" + this.Type.String()
	}
	if this.A != nil {
		output += " " + this.A.String()
		if this.B != nil {
			output += " " + this.B.String()
		}
	} else {
		if this.B != nil {
			output += " ???, " + this.B.String()
		}
	}
	if this.Dest != nil {
		output += " -> " + this.Dest.String()
	}
	return output
}
