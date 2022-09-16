package operandtype

type OperandType int

const (
	InvalidOpType OperandType = iota
	Proc
	Mem
	Temp
	Local
	Lit

	Register
	Spill
)
