package instrtype

import "strconv"

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
	case Convert:
		return "convert"

	case Call:
		return "call"

	case LoadLocal:
		return "load_local"
	case StoreLocal:
		return "store_local"

	case LoadMem:
		return "load_mem"
	case StoreMem:
		return "store_mem"

	case LoadSpill:
		return "load_spill"
	case StoreSpill:
		return "store_spill"

	case BoundsCheck:
		return "bound_scheck"

	case PushRet:
		return "push_ret"
	case PopRet:
		return "pop_ret"
	case PushArg:
		return "push_arg"
	case PopArg:
		return "pop_arg"
	}
	panic("Unstringified InstrType: " + strconv.Itoa(int(i)))
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

	Call

	LoadLocal
	StoreLocal

	LoadMem
	StoreMem

	LoadSpill
	StoreSpill

	BoundsCheck

	PushRet
	PopRet
	PushArg
	PopArg
)
