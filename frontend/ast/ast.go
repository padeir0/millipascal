package ast

import (
	lex "mpc/frontend/enums/lexType"
	T "mpc/frontend/enums/Type"
	IT "mpc/frontend/enums/instrType"
	ST "mpc/frontend/enums/symbolType"
	FT "mpc/frontend/enums/flowType"
	OT "mpc/frontend/enums/operandType"

	"fmt"
	"strings"
	"strconv"
)

type Node struct {
	Text   string
	Lex    lex.TkType
	Leaves []*Node

	T     T.Type

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
			output += sy.Proc.Name + ":\n"+
				"env: " + sy.Proc.StrArgs() + "\n" + 
				FmtBasicBlock(sy.Proc.Code)
		}
	}
	return output
}

type Symbol struct {
	T ST.SymbolType
	Name string
	N *Node

	Proc  *Proc
	Mem   *Mem
}

func (v *Symbol) String() string {
	switch v.T {
	case ST.Proc:
		return "proc "+v.Name
	case ST.Mem:
		return "mem "+v.Name
	case ST.Const:
		return "const "+v.Name
	default:
		return "invalid"
	}
}

type Proc struct {
	Name string
	Args []*Decl
	Rets []T.Type
	Vars []*Decl

	Names map[string]*Decl
	N *Node
	Code *BasicBlock
	SpillRegionSize int
}

func (p *Proc) StrArgs() string {
	output := []string{}
	for _, decl := range p.Names {
		output = append(output, decl.String())
	}
	return strings.Join(output, ", ")
}

type Decl struct {
	Name string
	Type T.Type
	N *Node
}

func (d *Decl) String() string {
	return d.Name + ":" + d.Type.String()
}

type Mem struct {
	Size int
	Type T.Type
	Init []*Node
}

type BasicBlock struct {
	Label string
	Code  []*Instr
	Out   Flow
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
		V:     cond,
		True:  True,
		False: False,
	}
}

func (b *BasicBlock) Return() {
	b.Out = Flow{
		T:     FT.Return,
	}
}

func (b *BasicBlock) HasFlow() bool {
	return b.Out.T != FT.InvalidFlow
}

func (b *BasicBlock) String() string {
	output := b.Label + ":\n"
	for _, v := range b.Code {
		output += "\t" + v.String() + "\n"
	}
	output += "\t" + b.Out.String()
	return output
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
	V     *Operand
	True  *BasicBlock
	False *BasicBlock
}

func (f *Flow) String() string {
	switch f.T {
	case FT.Jmp:
		return "jmp " + f.True.Label
	case FT.If:
		return "if "+f.V.String()+"? "+f.True.Label+" : "+f.False.Label
	case FT.Return:
		return "ret"
	}
	return "invalid FlowType"
}

type Operand struct {
	T OT.OperandType
	Type T.Type
	Label string
	Num int
}

func (o *Operand) String() string {
	switch o.T {
	case OT.Temp:
		return "'"+strconv.Itoa(o.Num)
	case OT.Register:
		return "'r"+strconv.Itoa(o.Num)
	case OT.Spill:
		return "'s"+strconv.Itoa(o.Num)
	default:
		return o.Label
	}
}

type Instr struct {
	T           IT.InstrType
	Type        T.Type
	Operands    []*Operand
	Destination []*Operand
}

func (i *Instr) String() string {
	if i.Destination != nil {
		if i.Type != T.Invalid {
			return fmt.Sprintf("%v:%v %v -> %v", i.T.String(), i.Type.String(), i.StrOps(), i.StrDests())
		} else {
			return fmt.Sprintf("%v %v -> %v", i.T.String(), i.StrOps(), i.StrDests())
		}
	} else {
		if i.Type != T.Invalid {
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
