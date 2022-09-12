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
	case Load:
		return "load"
	case Store:
		return "store"
	case Convert:
		return "convert"
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

	Load
	Store

	BoundsCheck

	Call
)
