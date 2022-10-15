package hirclass

type HIRClass int

func (c HIRClass) String() string {
	switch c {
	case Temp: return "temp"
	case Lit: return "lit"
	case Local: return "local"
	case Global: return "global"
	}
	return "?"
}

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
