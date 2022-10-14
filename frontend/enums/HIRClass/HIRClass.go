package hirclass

type HIRClass int

const (
	InvalidOpType HIRClass = iota

	Temp
	Lit
	Local
	Global
)

func IsOperable(c HIRClass) bool {
	return c == Temp ||
		c == Lit ||
		c == Local ||
		c == Global
}

func IsResult(c HIRClass) bool {
	return c == Temp ||
		c == Local
}
