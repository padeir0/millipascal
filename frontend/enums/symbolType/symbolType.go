package symboltype

type SymbolType int

func (s SymbolType) String() string {
	switch s {
	case Proc:
		return "proc"
	case Mem:
		return "mem"
	case Var:
		return "Var"
	case Arg:
		return "Arg"
	}
	return "??"
}

const (
	Invalid SymbolType = iota

	Proc
	Builtin
	Mem
	Var
	Arg
)
