package symbolkind

type SymbolKind int

func (s SymbolKind) String() string {
	switch s {
	case Proc:
		return "proc"
	case Data:
		return "data"
	case Builtin:
		return "builtin"
	case Var:
		return "var"
	case Arg:
		return "arg"
	case Module:
		return "module"
	case Struct:
		return "struct"
	case StructField:
		return "struct field"
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
	StructField
)
