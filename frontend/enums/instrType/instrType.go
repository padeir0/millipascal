package instrtype

type InstrType int

func (i InstrType) String() string {
	switch i {
	case Add:
		return "add"
	case UnaryPlus:
		return "unary_plus"
	case Sub:
		return "sub"
	case UnaryMinus:
		return "unary_minus"
	case Div:
		return "div"
	case Mult:
		return "mult"
	case Rem:
		return "rem"
	case Eq:
		return "eq"
	case Diff:
		return "diff"
	case Less:
		return "less"
	case More:
		return "more"
	case LessEq:
		return "less_eq"
	case MoreEq:
		return "more_eq"
	case Or:
		return "or"
	case And:
		return "and"
	case Not:
		return "not"
	case Call:
		return "call"
	case MemLoad:
		return "mem_load"
	case MemStore:
		return "mem_store"
	case LocalStore:
		return "local_store"
	case Convert:
		return "convert"
	case BoundsCheck:
		return "bounds_check"
	}
	panic("Unstringified InstrType")
}

const (
	InvalidInstr InstrType = iota
	Add
	UnaryPlus
	Sub
	UnaryMinus
	Div
	Mult
	Rem

	Eq
	Diff
	Less
	More
	LessEq
	MoreEq

	Or
	And
	Not

	Convert

	LocalStore
	MemLoad
	MemStore

	BoundsCheck

	Call
	StoreRet
	LoadRet
	StoreArg
	LoadArg
)
