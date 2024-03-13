package module

import (
	. "mpc/core"
	lex "mpc/core/module/lexkind"
	ST "mpc/core/module/symbolkind"
	T "mpc/core/module/types"

	"fmt"
	"strings"

	"math/big"
)

type TypeInfo struct {
	MultiRet bool
	ID       T.TypeID
}

func (this TypeInfo) String() string {
	return fmt.Sprintf("[%v, %v]", this.MultiRet, this.ID)
}

type Node struct {
	Text   string
	Lex    lex.LexKind
	Leaves []*Node

	TInfo TypeInfo

	Value *big.Int // for int literals

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
}

func (this *Node) SetLeaves(leaves []*Node) {
	this.Leaves = leaves
}

func ast(n *Node, i int) string {
	rng := "nil"
	if n.Range != nil {
		rng = n.Range.String()
	}
	output := fmt.Sprintf("{%s, '%s':%s, %s",
		lex.FmtLexKind(n.Lex),
		n.Text,
		n.TInfo.String(),
		rng,
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

func (M *Module) ResetVisitedSymbols() {
	for _, sy := range M.Globals {
		sy.ResetVisited()
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
		sy, ok := dep.M.Exported[sy.Name]
		if !ok {
			panic("use of non-exported name")
		}
		return sy
	}
	return sy
}

func (M *Module) GetExternalSymbol(module string, name string) *Symbol {
	dep, ok := M.Dependencies[module]
	if !ok {
		panic("use of unknown dependency")
	}
	sy, ok := dep.M.Exported[name]
	if !ok {
		panic("use of non-exported name")
	}
	return sy
}

type Symbol struct {
	T          ST.SymbolKind
	ModuleName string
	Name       string
	N          *Node
	External   bool

	Proc  *Proc
	Data  *Data
	Const *Const
	Type  *TypeDef

	Refs    map[string]*Symbol
	Visited bool
}

func (this *Symbol) ResetVisited() {
	if !this.Visited {
		return
	}
	this.Visited = false
	for _, ref := range this.Refs {
		ref.ResetVisited()
	}
}

func (v *Symbol) String() string {
	return v.T.String() + " " + v.Name
}

func (this *Symbol) Link(s *Symbol) {
	if this.Refs == nil {
		this.Refs = map[string]*Symbol{}
	}
	_, ok := this.Refs[s.Name]
	if ok {
		return
	}
	this.Refs[s.Name] = s
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
	Rets   []T.TypeID
	Type   T.TypeID

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

type Data struct {
	Name     string
	Size     *big.Int
	Init     *Node
	DataType T.TypeID

	Contents string

	// allocate two slices with the same size,
	// each index represents a single item from
	// the source blob:
	//     if nums is nil, look at symbols
	//     if symbol is also nil, panic
	// this could have been written in Anu as:
	//     Blob *(&Symbol|int)
	Symbols []*Symbol
	Nums    []*big.Int
}

// a constant can either be a integer or a symbol
type Const struct {
	Value  *big.Int
	Symbol *Symbol
	Type   T.TypeID
}

type TypeDef struct {
	Type T.TypeID
}
