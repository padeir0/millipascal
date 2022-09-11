package ast

import (
	T "mpc/frontend/lexType"
	"fmt"
	"strings"
)

type Type int

func (t Type) String() string {
	switch t {
	case InvalidType:
		return "invalid"
	case Byte:
		return "byte"
	case Word:
		return "word"
	case DWord:
		return "dword"
	case QWord:
		return "qword"
	case MultiRet:
		return "multi-return"
	}
	panic("Type.String(): Invalid Type")
}

const (
	InvalidType Type = iota
	Byte
	Word
	DWord
	QWord
	MultiRet
)

type Node struct {
	Text   string
	Lex    T.TkType
	Leaves []*Node

	T     Type

	Line, Col int
	Length    int
}

func (n *Node) String() string {
	if n.Lex == T.EOF {
		return "EOF"
	}
	return n.Text
}

func FmtNode(n *Node) string {
	return ast(n, 0)
}

func ast(n *Node, i int) string {
	output := fmt.Sprintf("{%s, '%s':%s",
		T.FmtNodeType(n.Lex),
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

type SymbolType int

func (s SymbolType) String() string {
	switch s {
	case SProc:
		return "proc"
	case SMem:
		return "mem"
	case SConst:
		return "constant"
	}
	return "??"
}

const (
	InvalidSymbolType SymbolType = iota
	SProc
	SMem
	SConst
)

type Symbol struct {
	T SymbolType
	Name string
	N *Node

	Proc  *Proc
	Mem   *Mem
}

func (v *Symbol) String() string {
	switch v.T {
	case SProc:
		return "proc "+v.Name
	case SMem:
		return "mem "+v.Name
	case SConst:
		return "const "+v.Name
	default:
		return "invalid"
	}
}

type Proc struct {
	Args []*Decl
	Rets []*Decl
	Vars []*Decl

	Names map[string]*Decl
	Code *Node
}

type Decl struct {
	Name string
	Type Type
	N *Node
}

type Mem struct {
	Size int
	Type Type
	Init []*Node
}
