package class

type Class int

const (
	InvalidMIRClass Class = iota

	Register
	Spill
	CallerInterproc
	CalleeInterproc
	Local

	Lit
	Static
)

func IsImmediate(ot Class) bool {
	return ot == Register ||
		ot == Lit ||
		ot == Static
}

func IsAddressable(ot Class) bool {
	return ot == Spill ||
		ot == CallerInterproc ||
		ot == CalleeInterproc ||
		ot == Local
}

func IsRegister(ot Class) bool {
	return ot == Register
}
