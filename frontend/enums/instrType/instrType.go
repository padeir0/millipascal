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
	case Load:
		return "load"
	case Store:
		return "store"
	case Offset:
		return "offset"
	case Call:
		return "call"
	case InitFrame:
		return "init_frame"
	}
	panic("Unstringified InstrType: " + strconv.Itoa(int(i)))
}

/*
These instructions are used both as an Medium-level IR
and Low Level IR, the difference are the operands:
	- When in MIR, the operands are of any class, as long as it typechecks
	- When in LIR, the operands must be of specific classes and typecheck

The annotations bellow show the type and operand class as:
	type:class
On the left side are the operands, surrounded by brackets,
While on the right side, after the arrow, is the single destination:
	[operand, operand] -> destination

The classes are defined by the operandType enum and are grouped as:
	class Immediate = register|lit
	class Addressable = spill|argument|return|local
While the types are defined by the Types enum and are grouped as:
	type Any = i8|i16|i32|i64|ptr|bool
	type NonPtr = i8|i16|i32|i64|bool
	type Number = i8|i16|i32|i64
*/

const (
	InvalidInstr InstrType = iota
	Add // [Number:Immediate, Number:Immediate] -> Number:register
	Sub
	Div
	Mult
	Rem

	Eq // [Any:Immediate, Any:Immediate] -> bool:register
	Diff
	Less
	More
	LessEq
	MoreEq

	Or // [bool:Immediate, bool:Immediate] -> bool:register
	And

	UnaryMinus // [Number:Immediate] -> Number:register
	UnaryPlus

	Not // [bool:Immediate] -> bool:register

	Convert // [NonPtr:Immediate] -> NonPtr:register

	Offset   // [ptr:Immediate, Number:Immediate] -> ptr:register
	LoadPtr  // [ptr:Immediate] -> Any:register
	StorePtr // [Any:Immediate] -> ptr:Immediate

	Load  // [Any:Addressable] -> Any:register
	Store // [Any:Immediate] -> Any:Addressable

        // reserves space for parameters and returns of a procedure call
	InitFrame  // [Number:lit, Number:lit]
	Call       // [proc:proc]
)
