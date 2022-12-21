package ir

import (
	T "mpc/frontend/Type"
	hirc "mpc/frontend/enums/HIRClass"
	mirc "mpc/frontend/enums/MIRClass"
	FT "mpc/frontend/enums/flowType"
	IT "mpc/frontend/enums/instrType"
	lex "mpc/frontend/enums/lexType"
	ST "mpc/frontend/enums/symbolType"

	"fmt"
	"strconv"
	"strings"
)

type Node struct {
	Text   string
	Lex    lex.TkType
	Leaves []*Node

	T *T.Type

	Value int64 // for literals

	Line, Col int
	Length    int
}

func (n *Node) String() string {
	if n.Lex == lex.EOF {
		return "EOF"
	}
	return n.Text
}

func FmtNode(n *Node) string {
	return ast(n, 0)
}

func ast(n *Node, i int) string {
	output := fmt.Sprintf("{%s, '%s':%s",
		lex.FmtNodeType(n.Lex),
		n.Text,
		n.T.String(),
	)
	output += "}"
	for _, kid := range n.Leaves {
		if kid == nil {
			output += indent(i) + "nil"
			continue
		}
		output += indent(i) + ast(kid, i+1)
	}
	return output
}

func indent(n int) string {
	output := "\n"
	for i := -1; i < n-1; i++ {
		output += "    "
	}
	output += "└─>"
	return output
}

type Module struct {
	ID   string
	Name string

	Globals map[string]*Symbol

	Root *Node
}

func (M *Module) String() string {
	return fmt.Sprintf("%v{\nRoot:\n\t%v,\nSymbols: %v\n}", M.ID, ast(M.Root, 1), M.StrGlobals())
}

func (M *Module) StrGlobals() string {
	output := []string{}
	for _, sy := range M.Globals {
		output = append(output, sy.Name)
	}

	return strings.Join(output, ", ")
}

func (M *Module) StringifyCode() string {
	output := ""
	for _, sy := range M.Globals {
		if sy.T == ST.Proc {
			output += sy.Proc.Name + ":\n" +
				"[" + sy.Proc.StrArgs() + "]\n" +
				FmtBasicBlock(sy.Proc.Code)
		}
	}
	return output
}

type Symbol struct {
	T    ST.SymbolType
	Name string
	N    *Node

	Type *T.Type
	Proc *Proc
	Mem  *Mem
}

func (v *Symbol) String() string {
	switch v.T {
	case ST.Proc:
		return "proc " + v.Name
	case ST.Var:
		return "var " + v.Name + ":" + v.Type.String()
	case ST.Arg:
		return "arg " + v.Name + ":" + v.Type.String()
	case ST.Mem:
		return "mem " + v.Name
	default:
		return "invalid"
	}
}

type PositionalSymbol struct {
	Position int
	Symbol   *Symbol
}

type Proc struct {
	Name   string
	ArgMap map[string]PositionalSymbol
	Vars   map[string]*Symbol
	Args   []*Symbol
	Rets   []*T.Type
	T      *T.Type

	N           *Node
	Code        *BasicBlock

	NumOfVars int
	NumOfSpills int
	NumOfMaxCalleeArguments int
}

func (p *Proc) StrArgs() string {
	output := []string{}
	for _, decl := range p.ArgMap {
		output = append(output, decl.Symbol.String())
	}
	return strings.Join(output, ", ")
}

func (p *Proc) StrVars() string {
	output := []string{}
	for _, decl := range p.Vars {
		output = append(output, decl.String())
	}
	return strings.Join(output, ", ")
}

func (p *Proc) StrRets() string {
	output := []string{}
	for _, ret := range p.Rets {
		output = append(output, ret.String())
	}
	return strings.Join(output, ", ")
}

func (p *Proc) DoesReturnSomething() bool {
	return len(p.Rets) > 0
}

func (p *Proc) ResetVisited() {
	resetVisitedBB(p.Code)
}

type Mem struct {
	Name string
	Size int64
	Contents string
	Type T.Type
	Init *Node
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

func (b *BasicBlock) Jmp(o *BasicBlock) {
	b.Out = Flow{
		T:    FT.Jmp,
		V:    nil,
		True: o,
	}
}

func (b *BasicBlock) Branch(cond *Operand, True *BasicBlock, False *BasicBlock) {
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

func resetVisitedBB(bb *BasicBlock) {
	if !bb.Visited {
		return
	}
	bb.Visited = false
	switch bb.Out.T {
	case FT.If:
		resetVisitedBB(bb.Out.True)
		resetVisitedBB(bb.Out.False)
	case FT.Return:
		return
	case FT.Jmp:
		resetVisitedBB(bb.Out.True)
	}
}


func ApplyToBlocks(b *BasicBlock, proc func(*BasicBlock)) {
	resetVisitedBB(b)
	applyToBlocks(b, proc)
}

func applyToBlocks(b *BasicBlock, proc func(*BasicBlock)) {
	if b.Visited {
		return
	}
	proc(b)
	b.Visited = true
	switch b.Out.T {
	case FT.If:
		applyToBlocks(b.Out.True, proc)
		applyToBlocks(b.Out.False, proc)
	case FT.Jmp:
		applyToBlocks(b.Out.True, proc)
	case FT.Return, FT.Exit:
		// do nothing
	}
	return 
}

func ProperlyTerminates(b *BasicBlock) bool {
	resetVisitedBB(b)
	return properlyTerminates(b)
}

func properlyTerminates(b *BasicBlock) bool {
	if b.Visited {
		// we just say that this looping branch doesn't matter
		return true 
	}
	b.Visited = true
	switch b.Out.T {
	case FT.If:
		return properlyTerminates(b.Out.True) && properlyTerminates(b.Out.False)
	case FT.Jmp:
		return properlyTerminates(b.Out.True)
	case FT.Return, FT.Exit:
		return true
	}
	return false
}

func FmtBasicBlock(bb *BasicBlock) string {
	bblist := FlattenGraph(bb)
	output := ""
	for _, b := range bblist {
		output += b.String() + "\n"
	}
	return output
}

func FlattenGraph(bb *BasicBlock) []*BasicBlock {
	BBset := map[*BasicBlock]struct{}{}
	flattenHelper(bb, &BBset)

	output := make([]*BasicBlock, len(BBset))
	i := 0
	for b := range BBset {
		output[i] = b
		i++
	}
	return output
}

func flattenHelper(bb *BasicBlock, BBset *map[*BasicBlock]struct{}) {
	(*BBset)[bb] = struct{}{}
	_, ok := (*BBset)[bb.Out.True]
	if bb.Out.True != nil && !ok {
		flattenHelper(bb.Out.True, BBset)
	}
	_, ok = (*BBset)[bb.Out.False]
	if bb.Out.False != nil && !ok {
		flattenHelper(bb.Out.False, BBset)
	}
}

type Flow struct {
	T     FT.FlowType
	V     []*Operand
	True  *BasicBlock
	False *BasicBlock
}

func (f *Flow) String() string {
	switch f.T {
	case FT.Jmp:
		return "jmp " + f.True.Label
	case FT.If:
		return "if " + f.StrRets() + "? " + f.True.Label + " : " + f.False.Label
	case FT.Return:
		return "ret " + f.StrRets()
	case FT.Exit:
		return "exit " + f.StrRets()
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
	Hirc   hirc.HIRClass
	Mirc   mirc.MIRClass
	Type   *T.Type
	Symbol *Symbol
	Num    int64
}

func (o *Operand) String() string {
	if o == nil {
		return "nil"
	}
	switch o.Mirc {
	case mirc.Lit:
		return strconv.FormatInt(o.Num, 10)
	case mirc.Local:
		return "$" + o.Symbol.Name + ":" + o.Type.String()
	case mirc.Spill:
		return "s" + strconv.FormatInt(o.Num, 10) + ":" + o.Type.String()
	case mirc.Register:
		return "r" + strconv.FormatInt(o.Num, 10) + ":" + o.Type.String()
	case mirc.Static:
		return "%" + strconv.FormatInt(o.Num, 10) + ":" + o.Type.String()
	case mirc.CallerInterproc:
		return "caller#" + strconv.FormatInt(o.Num, 10) + ":" + o.Type.String()
	case mirc.CalleeInterproc:
		return "callee#" + strconv.FormatInt(o.Num, 10) + ":" + o.Type.String()
	}
	switch o.Hirc {
	case hirc.Temp:
		return "'" + strconv.FormatInt(o.Num, 10) + ":" + o.Type.String()
	case hirc.Local:
		return "local#" + o.Symbol.Name + ":" + o.Type.String()
	case hirc.Arg:
		return "arg#" + o.Symbol.Name + ":" + o.Type.String()
	case hirc.Global:
		return o.Symbol.Name + ":" + o.Type.String()
	case hirc.Lit:
		return strconv.FormatInt(o.Num, 10)
	}
	return "?"
}

func (o *Operand) MirStr() string {
	if o == nil {
		return "nil"
	}
	switch o.Mirc {
	case mirc.Register:
		return "'" + strconv.FormatInt(o.Num, 10) + ":" + o.Type.String()
	case mirc.Spill:
		return "^" + strconv.FormatInt(o.Num, 10) + ":" + o.Type.String()
	case mirc.CallerInterproc:
		return "~" + strconv.FormatInt(o.Num, 10) + ":" + o.Type.String()
	case mirc.Local:
		return "$" + o.Symbol.Name + ":" + o.Type.String()
	case mirc.Static:
		return o.Symbol.Name + ":" + o.Type.String()
	case mirc.Lit:
		return o.Symbol.Name
	default:
		return o.Symbol.Name + "?" + strconv.FormatInt(o.Num, 10)
	}
}

type Instr struct {
	T           IT.InstrType
	Type        *T.Type
	Operands    []*Operand
	Destination []*Operand
}

func (i *Instr) String() string {
	if i == nil {
		return "nil"
	}
	if i.Destination != nil {
		if i.Type != nil {
			return fmt.Sprintf("%v:%v %v -> %v", i.T.String(), i.Type.String(), i.StrOps(), i.StrDests())
		} else {
			return fmt.Sprintf("%v %v -> %v", i.T.String(), i.StrOps(), i.StrDests())
		}
	} else {
		if i.Type != nil {
			return fmt.Sprintf("%v:%v %v", i.T.String(), i.Type.String(), i.StrOps())
		} else {
			return fmt.Sprintf("%v %v", i.T.String(), i.StrOps())
		}
	}
}

func (i *Instr) StrOps() string {
	if len(i.Operands) == 0 {
		return ""
	}
	output := i.Operands[0].String()
	for _, v := range i.Operands[1:] {
		output += ", " + v.String()
	}
	return output
}

func (i *Instr) StrDests() string {
	if len(i.Destination) == 0 {
		return ""
	}
	output := i.Destination[0].String()
	for _, v := range i.Destination[1:] {
		output += ", " + v.String()
	}
	return output
}
