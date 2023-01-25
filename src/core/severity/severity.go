package severity

type Severity int

func (this Severity) String() string {
	switch this {
	case Error:
		return "error"
	case Warning:
		return "warning"
	case Information:
		return "info"
	case Hint:
		return "hint"
	case InternalError:
		return "internal error"
	}
	panic("invalid severity")
}

const (
	InvalidSeverity Severity = iota
	Error
	Warning
	Information
	Hint
	InternalError // should never happen (but will)
)
