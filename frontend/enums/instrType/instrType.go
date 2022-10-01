package instrtype

import "strconv"

type InstrType int

func (i InstrType) String() string {
	switch i {
	case Add:
		return "add"
	case UnaryPlus:
		return "uplus"
	case Sub:
		return "sub"
	case UnaryMinus:
		return "uminus"
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
		return "lesseq"
	case MoreEq:
		return "moreeq"
	case Or:
		return "or"
	case And:
		return "and"
	case Not:
		return "not"
	case Convert:
		return "convert"
	case Load:
		return "load"
	case Store:
		return "store"
	case Copy:
		return "copy"
	case LoadPtr:
		return "loadptr"
	case StorePtr:
		return "storeptr"
	case Offset:
		return "offset"
	case Call:
		return "call"
	case IncFrame:
		return "incframe"
	case DecFrame:
		return "decframe"
	}
	panic("Unstringified InstrType: " + strconv.Itoa(int(i)))
}

const (
	InvalidInstr InstrType = iota

	Add
	Sub
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

	UnaryMinus
	UnaryPlus

	Convert

	Offset
	LoadPtr
	StorePtr

	Load
	Store

	Copy

	Call

	IncFrame
	DecFrame
)
