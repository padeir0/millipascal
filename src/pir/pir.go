package pir

import (
	"fmt"
	hirc "mpc/pir/class"
	FT "mpc/pir/flowkind"
	IT "mpc/pir/instrkind"
	T "mpc/pir/types"

	"math/big"
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

func (this *Program) AddBuiltin(p *Procedure) int {
	index := len(this.Symbols)
	this.Symbols = append(this.Symbols, &Symbol{Proc: p, Builtin: true})
	return index
}

func (this *Program) AddProc(p *Procedure) int {
	index := len(this.Symbols)
	this.Symbols = append(this.Symbols, &Symbol{Proc: p})
	return index
}

func (this *Program) AddMem(m *DataDecl) int {
	index := len(this.Symbols)
	this.Symbols = append(this.Symbols, &Symbol{Mem: m})
	return index
}

func (this *Program) String() string {
	if this == nil {
		return "nil program"
	}
	output := "Program: " + this.Name + "\n\n"
	for _, sy := range this.Symbols {
		output += sy.String() + "\n"
	}
	return output
}

func NewProgram() *Program {
	return &Program{Symbols: []*Symbol{}}
}

type Symbol struct {
	Proc    *Procedure
	Mem     *DataDecl
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

type DataDecl struct {
	Label string
	Data  string
	Size  *big.Int
}

func (this *DataDecl) String() string {
	if this.Data != "" {
		return this.Label + ": " + this.Data
	}
	return this.Label + ": " + this.Size.Text(10)
}

type Procedure struct {
	Label string
	Vars  []*T.Type
	Args  []*T.Type
	Rets  []*T.Type

	Start     BlockID
	AllBlocks []*BasicBlock
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

func (p *Procedure) StrRets() string {
	return StrTypes(p.Rets)
}
func (p *Procedure) StrArgs() string {
	return StrTypes(p.Args)
}
func (p *Procedure) StrLocals() string {
	return StrTypes(p.Vars)
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
	Code    []Instr
	Out     Flow
	Visited bool
}

func (this *BasicBlock) AddInstr(i Instr) {
	this.Code = append(this.Code, i)
}

func (this *BasicBlock) Jmp(id BlockID) {
	this.Out = Flow{
		T:    FT.Jmp,
		V:    nil,
		True: id,
	}
}

func (b *BasicBlock) Branch(cond Operand, True BlockID, False BlockID) {
	b.Out = Flow{
		T:     FT.If,
		V:     []Operand{cond},
		True:  True,
		False: False,
	}
}

func (b *BasicBlock) Return(rets []Operand) {
	b.Out = Flow{
		V: rets,
		T: FT.Return,
	}
}

func (b *BasicBlock) Exit(code Operand) {
	b.Out = Flow{
		V: []Operand{code},
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

func ProperlyTerminates(proc *Procedure) bool {
	start := proc.FirstBlock()
	proc.ResetBlocks()
	return properlyTerminates(proc, start)
}

func properlyTerminates(proc *Procedure, b *BasicBlock) bool {
	if b.Visited {
		// we just say that this looping branch doesn't matter
		return true
	}
	b.Visited = true
	switch b.Out.T {
	case FT.If:
		t := proc.GetBlock(b.Out.True)
		f := proc.GetBlock(b.Out.False)
		return properlyTerminates(proc, t) && properlyTerminates(proc, f)
	case FT.Jmp:
		t := proc.GetBlock(b.Out.True)
		return properlyTerminates(proc, t)
	case FT.Return, FT.Exit:
		return true
	}
	return false
}

type BlockID int

type Flow struct {
	T     FT.FlowKind
	V     []Operand
	True  BlockID
	False BlockID
}

func (this *Flow) String() string {
	switch this.T {
	case FT.Jmp:
		t := strconv.FormatInt(int64(this.True), 10)
		return "jmp .L" + t
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

func (this *Flow) StrRets() string {
	output := []string{}
	for _, op := range this.V {
		output = append(output, op.String())
	}
	return strings.Join(output, ", ")
}

type Operand struct {
	Class hirc.Class
	Type  *T.Type
	ID    int64
	Num   *big.Int
}

func (this *Operand) String() string {
	if this == nil {
		return "nil"
	}
	value := strconv.FormatInt(this.ID, 10)
	switch this.Class {
	case hirc.Temp:
		return "'" + value + ":" + this.Type.String()
	case hirc.Local:
		return "local#" + value + ":" + this.Type.String()
	case hirc.Arg:
		return "arg#" + value + ":" + this.Type.String()
	case hirc.Global:
		return "global#" + value + ":" + this.Type.String()
	case hirc.Lit:
		return this.Num.Text(10)
	}
	return "?"
}

type Instr struct {
	T           IT.InstrKind
	Type        *T.Type
	Operands    []Operand
	Destination []Operand
}

func (this *Instr) String() string {
	if this == nil {
		return "nil"
	}
	if this.Destination != nil {
		if this.Type != nil {
			return fmt.Sprintf("%v:%v %v -> %v", this.T.String(), this.Type.String(), this.StrOps(), this.StrDests())
		} else {
			return fmt.Sprintf("%v, %v -> %v", this.T.String(), this.StrOps(), this.StrDests())
		}
	} else {
		if this.Type != nil {
			return fmt.Sprintf("%v:%v %v", this.T.String(), this.Type.String(), this.StrOps())
		} else {
			return fmt.Sprintf("%v, %v", this.T.String(), this.StrOps())
		}
	}
}

func (this *Instr) StrOps() string {
	if len(this.Operands) == 0 {
		return ""
	}
	output := this.Operands[0].String()
	for _, v := range this.Operands[1:] {
		output += ", " + v.String()
	}
	return output
}

func (this *Instr) StrDests() string {
	if len(this.Destination) == 0 {
		return ""
	}
	output := this.Destination[0].String()
	for _, v := range this.Destination[1:] {
		output += ", " + v.String()
	}
	return output
}
