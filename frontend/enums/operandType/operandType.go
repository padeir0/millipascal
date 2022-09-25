package operandtype

type OperandType int

const (
	InvalidOpType OperandType = iota
	Proc
	Mem
	Temp
	Lit

	Local
	Register
	Spill
	Return
	Argument
)
