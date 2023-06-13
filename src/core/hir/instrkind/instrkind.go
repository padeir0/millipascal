package instrkind

import "strconv"

type InstrKind int

func (i InstrKind) String() string {
	switch i {
	case Add:
		return "add"
	case Sub:
		return "sub"
	case Neg:
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
	case Copy:
		return "copy"
	case LoadPtr:
		return "loadptr"
	case StorePtr:
		return "storeptr"
	case Call:
		return "call"
	}
	panic("Unstringified InstrType: " + strconv.Itoa(int(i)))
}

const (
	InvalidInstr InstrKind = iota

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

	Neg

	Convert

	LoadPtr
	StorePtr

	Copy

	Call
)
