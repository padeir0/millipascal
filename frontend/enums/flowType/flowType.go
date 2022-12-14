package flowtype

type FlowType int

func (f FlowType) String() string {
	switch f {
	case Jmp:
		return "jmp"
	case If:
		return "if"
	case Return:
		return "ret"
	case Exit:
		return "exit"
	}
	return "invalid FlowType"
}

const (
	InvalidFlow FlowType = iota

	Jmp
	If
	Return
	Exit
)
