package symbolkind

type SymbolKind int

func (s SymbolKind) String() string {
	switch s {
	case Proc:
		return "proc"
	case Mem:
		return "mem"
	case Var:
		return "Var"
	case Arg:
		return "Arg"
	case Module:
		return "Module"
	}
	return "??"
}

const (
	Invalid SymbolKind = iota

	Proc
	Builtin
	Mem
	Var
	Arg
	Module
)
