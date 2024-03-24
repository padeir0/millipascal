package globalkind

type GlobalKind int

func (s GlobalKind) String() string {
	switch s {
	case Proc:
		return "proc"
	case Data:
		return "data"
	case Const:
		return "const"
	case Module:
		return "module"
	case Struct:
		return "struct"
	}
	return "??"
}

const (
	Invalid GlobalKind = iota

	Proc
	Data
	Const
	Module
	Struct
)
