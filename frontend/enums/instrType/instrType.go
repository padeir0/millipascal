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
	case LoadPtr:
		return "loadptr"
	case StorePtr:
		return "storeptr"
	case Offset:
		return "offset"
	case Call:
		return "call"
	}
	panic("Unstringified InstrType: " + strconv.Itoa(int(i)))
}

/*
These instructions are used both as an Medium-level IR
and Low Level IR, the difference are the operands:
	- When in MIR, the operands are of any class, as long as it typechecks
	- When in LIR, the operands must be of specific classes and typecheck

The annotations bellow show the type and operand class as:
	type'class

The instructions are typed, and the following notation is used to show
how operands must be typed relative to the instruction type:
	instr:T of Constraint [T'class, T'class] -> T'class

Where all occurrences of T can be substituted
by one type inside the constraint.

On the left side of the arrow are the operands, surrounded by brackets,
while on the right side, after the arrow, is the single destination:
	instr:type [operand, operand] -> destination

The classes are defined by the operandType enum and are grouped as:
	Immediate = register|lit
	Addressable = spill|argument|return|local

While the types are defined by the Types enum and form
the following constraints:
	Any = i8|i16|i32|i64|ptr|bool
	NonPtr = i8|i16|i32|i64|bool
	Number = i8|i16|i32|i64
*/

const (
	InvalidInstr InstrType = iota

	Add //:T of Number [T'Immediate, T'Immediate] -> T'register
	Sub
	Div
	Mult
	Rem

	Eq //:T of Any [T'Immediate, T'Immediate] -> bool'register
	Diff
	Less
	More
	LessEq
	MoreEq

	Or //:bool [bool'Immediate, bool'Immediate] -> bool'register
	And

	Not //:bool [bool'Immediate] -> bool'register

	UnaryMinus //:T of Number [T'Immediate] -> T'register
	UnaryPlus

	Convert //:T of NonPtr [NonPtr'Immediate] -> T'register

	Offset   //:T of Number [ptr'Immediate, T'Immediate] -> ptr'register
	LoadPtr  //:T of Any [ptr'Immediate] -> T'register
	StorePtr //:T of Any [T'Immediate] -> ptr'Immediate

	Load  //:T of Any [T'Addressable] -> T'register
	Store //:T of Any [T'Immediate] -> T'Addressable

	// MIR: [proc:proc, Any'Immediate...] -> Any'Immediate...
	// LIR: [proc:proc]
	Call 

	// only for LIR
	InitFrame // [Number'lit, Number'lit]
	EndFrame  // [Number'lit, Number'lit]
)
