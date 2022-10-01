package hirclass

type HIRClass int

const (
	InvalidOpType HIRClass = iota

	Temp
	Lit
	Local
	Global
)
