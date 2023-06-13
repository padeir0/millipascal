package class

type Class int

func (c Class) String() string {
	switch c {
	case Temp:
		return "temp"
	case Lit:
		return "lit"
	case Local:
		return "local"
	case Arg:
		return "arg"
	case Global:
		return "global"
	}
	return "?"
}

const (
	InvalidClass Class = iota

	Temp
	Lit
	Local
	Arg
	Global
)

func IsOperable(c Class) bool {
	return c == Temp ||
		c == Lit ||
		c == Local ||
		c == Arg ||
		c == Global
}

func IsResult(c Class) bool {
	return c == Temp ||
		c == Local ||
		c == Arg
}
