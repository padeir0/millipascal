package mirclass

type MIRClass int

const (
	InvalidMIRClass MIRClass = iota

	Register
	Spill
	Interproc
	Local
	Lit
	Static
)

func IsImmediate(ot MIRClass) bool {
	return ot == Register ||
		ot == Lit ||
		ot == Static
}

func IsAddressable(ot MIRClass) bool {
	return  ot == Spill ||
		ot == Interproc ||
		ot == Local
}

func IsRegister(ot MIRClass) bool  {
	return ot == Register
}
