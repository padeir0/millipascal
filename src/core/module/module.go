package module

import (
	. "mpc/core"
	"mpc/core/asm"
	GK "mpc/core/module/globalkind"
	LxK "mpc/core/module/lexkind"
	LcK "mpc/core/module/localkind"
	T "mpc/core/types"

	"fmt"
	"strings"

	"math/big"
)

type Node struct {
	Text   string
	Lex    LxK.LexKind
	Leaves []*Node

	Type     *T.Type
	MultiRet bool // if this is true, T == nil

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
		LxK.FmtLexKind(n.Lex),
		n.Text,
		n.Type.String(),
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

	Globals      map[string]*Global
	Dependencies map[string]*Dependency
	Exported     map[string]*Global

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

func (M *Module) GetSymbol(name string) *Global {
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

func (M *Module) GetExternalSymbol(module string, name string) *Global {
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

type Global struct {
	Kind       GK.GlobalKind
	ModuleName string
	Name       string
	N          *Node
	External   bool
	Refs       Refs
	Attr       []string
	Visited    bool

	Proc   *Proc
	Data   *Data
	Const  *Const
	Struct *Struct
}

func (this *Global) Link(other *Global) {
	this.Refs.Link(other)
}

func (this *Global) LinkField(other *Global, field int) {
	this.Refs.LinkField(other, field)
}

func (this *Global) ResetVisited() {
	if !this.Visited {
		return
	}
	this.Visited = false
	this.Refs.ResetVisited()
	if this.Kind == GK.Struct {
		for i, field := range this.Struct.Fields {
			if field.Visited {
				this.Struct.Fields[i].Visited = false
				field.Refs.ResetVisited()
			}
		}
	}
}

func (this *Global) String() string {
	return this.Kind.String() + " " + this.Name
}

func (this *Global) GetType() *T.Type {
	switch this.Kind {
	case GK.Data:
		return this.Data.Type
	case GK.Proc:
		return this.Proc.Type
	case GK.Const:
		return this.Const.Type
	case GK.Struct:
		return this.Struct.Type
	default:
		panic("unreachable 212")
	}
}

/*
All ABIs must respect this, no calling convention may
alter the mangling of names.
*/
func (this *Global) Label() string {
	return this.ModuleName + "_" + this.Name
}

type Local struct {
	Kind     LcK.LocalKind
	Position int
	Name     string
	N        *Node
	T        *T.Type
}

func (this *Local) String() string {
	return this.Name + ":" + this.T.String()
}

type Proc struct {
	Name   string
	ArgMap map[string]int
	VarMap map[string]int
	Args   []*Local
	Vars   []*Local
	Rets   []*T.Type
	Type   *T.Type
	Asm    []asm.Line

	N *Node
}

func (this *Proc) StrArgs() string {
	output := []string{}
	for _, decl := range this.Args {
		output = append(output, decl.String())
	}
	return strings.Join(output, ", ")
}

func (this *Proc) StrVars() string {
	output := []string{}
	for _, decl := range this.Vars {
		output = append(output, decl.String())
	}
	return strings.Join(output, ", ")
}

func (this *Proc) StrRets() string {
	output := []string{}
	for _, ret := range this.Rets {
		output = append(output, ret.String())
	}
	return strings.Join(output, ", ")
}

func (this *Proc) DoesReturnSomething() bool {
	return len(this.Rets) > 0
}

func (this *Proc) GetLocal(name string) *Local {
	pos, ok := this.ArgMap[name]
	if ok {
		return this.Args[pos]
	}
	pos, ok = this.VarMap[name]
	if ok {
		return this.Vars[pos]
	}
	return nil
}

func FromSymbol(s *Global) SyField {
	return SyField{
		Sy:    s,
		Field: -1,
	}
}

type SyField struct {
	Sy    *Global
	Field int
}

func (this *SyField) IsVisited() bool {
	if this.IsField() {
		return this.Sy.Struct.Fields[this.Field].Visited
	} else {
		return this.Sy.Visited
	}
}

func (this *SyField) SetVisited(b bool) {
	if this.IsField() {
		this.Sy.Struct.Fields[this.Field].Visited = b
	} else {
		this.Sy.Visited = b
	}
}

func (this *SyField) IsField() bool {
	return this.Field >= 0
}

func (this *SyField) GetField() *Field {
	if this.IsField() {
		return &this.Sy.Struct.Fields[this.Field]
	}
	return nil
}

func (this *SyField) GetRefs() Refs {
	if this.IsField() {
		return this.Sy.Struct.Fields[this.Field].Refs
	}
	return this.Sy.Refs
}

func (this *SyField) Name() string {
	name := ""
	if this.IsField() {
		field := this.Sy.Struct.Fields[this.Field]
		name = this.Sy.Name + "." + field.Name
	} else {
		name = this.Sy.Name
	}
	return name
}

func (this *SyField) Link(sy *Global) {
	this.LinkField(sy, -1)
}

func (this *SyField) LinkField(sy *Global, field int) {
	if this.IsField() {
		this.Sy.Struct.Fields[this.Field].Refs.LinkField(sy, field)
	} else {
		this.Sy.LinkField(sy, field)
	}
}

type Unit struct{}

type Refs struct {
	Set     map[string]Unit
	Symbols []SyField
}

func (this *Refs) Link(s *Global) {
	this.LinkField(s, -1)
}

func (this *Refs) LinkField(s *Global, field int) {
	if this.Set == nil {
		this.Set = map[string]Unit{}
	}
	internalName := fmt.Sprintf("%v.%d", s.Name, field)
	_, ok := this.Set[internalName]
	if ok {
		return
	}
	this.Set[internalName] = Unit{}
	sf := SyField{
		Sy:    s,
		Field: field,
	}
	this.Symbols = append(this.Symbols, sf)
}

func (this *Refs) ResetVisited() {
	for _, sf := range this.Symbols {
		sf.Sy.ResetVisited()
	}
}

type Data struct {
	Name string
	Size *big.Int
	Init *Node
	Type *T.Type

	Contents string

	// blob
	Nums []asm.DataEntry
}

func (this *Data) DataTypeSize(M *Module) *big.Int {
	size := this.Type.Sizeof()
	if size == nil {
		panic("size was nil")
	} else {
		return size
	}
}

// a constant can either be a integer or a symbol
type Const struct {
	Value  *big.Int
	Symbol *Global
	Type   *T.Type
}

type Struct struct {
	Type     *T.Type
	Fields   []Field
	FieldMap map[string]int
}

type Field struct {
	Name    string
	Refs    Refs
	Offset  *Node
	Visited bool
}
