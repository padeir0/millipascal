package module

import (
	. "mpc/core"
	lex "mpc/core/module/lexkind"
	ST "mpc/core/module/symbolkind"
	T "mpc/core/types"

	"fmt"
	"strings"
)

type Node struct {
	Text   string
	Lex    lex.LexKind
	Leaves []*Node

	T *T.Type

	Value int64 // for literals

	Range *Range
}

func (n *Node) String() string {
	return ast(n, 0)
}

func (this *Node) AddLeaf(other *Node) {
	if this.Leaves == nil {
		this.Leaves = []*Node{other}
	} else {
		this.Leaves = append(this.Leaves, other)
	}
	if this.Range == nil {
		this.Range = other.Range
		return
	}
	if other.Range == nil {
		return
	}
	if this.Range.Begin.MoreThan(other.Range.Begin) {
		this.Range.Begin = other.Range.Begin
	}
	if this.Range.End.LessThan(other.Range.End) {
		this.Range.End = other.Range.End
	}
}

func (this *Node) SetLeaves(leaves []*Node) {
	for _, n := range leaves {
		if this.Range == nil {
			this.Range = n.Range
			continue
		}
		if n == nil || n.Range == nil {
			continue
		}
		if this.Range.Begin.MoreThan(n.Range.Begin) {
			this.Range.Begin = n.Range.Begin
		}
		if this.Range.End.LessThan(n.Range.End) {
			this.Range.End = n.Range.End
		}
	}
	this.Leaves = leaves
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

type Dependency struct {
	M      *Module
	Source *Node
}

type Module struct {
	BasePath string
	Name     string
	FullPath string
	Root     *Node

	Globals      map[string]*Symbol
	Dependencies map[string]*Dependency
	Exported     map[string]*Symbol

	Visited bool
}

func (M *Module) String() string {
	if M == nil {
		return "nil"
	}
	return fmt.Sprintf("%v{\nRoot:\n\t%v,\nSymbols: %v\n}", M.Name, ast(M.Root, 1), M.StrGlobals())
}

func (M *Module) StrGlobals() string {
	output := []string{}
	for _, sy := range M.Globals {
		output = append(output, sy.Name)
	}

	return strings.Join(output, ", ")
}

func (M *Module) ResetVisited() {
	if !M.Visited {
		return
	}
	M.Visited = false
	for _, dep := range M.Dependencies {
		dep.M.ResetVisited()
	}
}

func (M *Module) GetSymbol(name string) *Symbol {
	sy, ok := M.Globals[name]
	if !ok {
		return nil
	}
	if sy.External {
		dep, ok := M.Dependencies[sy.ModuleName]
		if !ok {
			panic("use of unknown dependency")
		}
		sy, ok := dep.M.Exported[name]
		if !ok {
			panic("use of non-exported name")
		}
		return sy
	}
	return sy
}

type Symbol struct {
	T        ST.SymbolKind
	Name     string
	N        *Node
	External bool

	Type       *T.Type
	Proc       *Proc
	Mem        *Mem
	ModuleName string
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
	case ST.Module:
		return "module " + v.Name
	case ST.Builtin:
		return "builtin " + v.Name
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
	Vars   map[string]PositionalSymbol
	Args   []*Symbol
	Rets   []*T.Type
	T      *T.Type

	N *Node
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
		output = append(output, decl.Symbol.String())
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

type Mem struct {
	Name     string
	Size     int64
	Contents string
	Type     T.Type
	Init     *Node
}
