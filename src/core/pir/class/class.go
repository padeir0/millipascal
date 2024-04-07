package class

type Class int

func (c Class) String() string {
	switch c {
	case Temp:
		return "temp"
	case Lit:
		return "lit"
	case Variable:
		return "variable"
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
	Variable
	Arg
	Global
)

func IsOperable(c Class) bool {
	return c == Temp ||
		c == Lit ||
		c == Variable ||
		c == Arg ||
		c == Global
}

func IsResult(c Class) bool {
	return c == Temp ||
		c == Variable ||
		c == Arg
}

func IsLocal(c Class) bool {
	return c == Variable || c == Arg
}
