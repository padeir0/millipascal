package Type

type Type int

func (t Type) String() string {
	switch t {
	case Invalid:
		return "invalid"
	case Byte:
		return "byte"
	case Word:
		return "word"
	case DWord:
		return "dword"
	case QWord:
		return "qword"
	case MultiRet:
		return "multi-return"
	}
	panic("Type.String(): Invalid Type")
}

const (
	Invalid Type = iota
	Byte
	Word
	DWord
	QWord
	MultiRet
)
