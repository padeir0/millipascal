package symboltype

type SymbolType int

func (s SymbolType) String() string {
	switch s {
	case Proc:
		return "proc"
	case Mem:
		return "mem"
	case Const:
		return "constant"
	}
	return "??"
}

const (
	Invalid SymbolType = iota
	Proc
	Mem
	Const
)
