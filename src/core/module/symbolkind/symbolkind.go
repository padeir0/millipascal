package symbolkind

type SymbolKind int

func (s SymbolKind) String() string {
	switch s {
	case Proc:
		return "proc"
	case Data:
		return "data"
	case Var:
		return "var"
	case Arg:
		return "arg"
	case Module:
		return "module"
	}
	return "??"
}

const (
	Invalid SymbolKind = iota

	Proc
	Builtin
	Data
	Const
	Var
	Arg
	Module
	Struct
)
