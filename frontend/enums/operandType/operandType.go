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

func IsImmediate(ot OperandType) bool {
	return ot == Register ||
		ot == Lit
}

func IsAddressable(ot OperandType) bool {
	return  ot == Spill ||
		ot == Argument ||
		ot == Return ||
		ot == Local
}

func IsRegister(ot OperandType) bool  {
	return ot == Register
}
