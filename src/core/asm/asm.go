package asm

import (
	"math/big"
	ik "mpc/core/asm/instrkind"
	"strconv"
)

type DataEntry struct {
	Type TypeSize
	Num  *big.Int
}

type Program struct {
	FileName string

	Writable   []Data
	Readonly   []Data
	Executable []Line
}

type Data struct {
	Label string
	Blob  []DataEntry // declared
	Str   string
	Size  int // reserved
}

type Line struct {
	Label string
	Instr Instr

	IsLabel bool
}

type Instr struct {
	Kind     ik.InstrKind
	Operands []Operand
}

type OperandKind int

const (
	InvalidOperandKind OperandKind = iota
	Simple
	Addressing
)

type Operand struct {
	Kind     OperandKind
	A        Value
	B        Value
	TypeSize TypeSize
}

type ValueKind int

const (
	InvalidValueKind ValueKind = iota
	Const
	Reg
	Label
)

type Value struct {
	Kind  ValueKind
	Const *big.Int
	Reg   Register
	Label string
}

// normal registers: 0 to 15
// instruction pointer: 16
type Register struct {
	ID       int
	TypeSize TypeSize
}

func (this Register) String() string {
	if this.TypeSize == QuadWord {
		switch this.ID {
		case 4:
			return "rsp"
		case 5:
			return "rbp"
		case 16:
			return "rip"
		}
	}
	return "r" + strconv.Itoa(this.ID) + regtsize(this.TypeSize)
}

func regtsize(size TypeSize) string {
	switch size {
	case QuadWord:
		return ""
	case DoubleWord:
		return "d"
	case Word:
		return "w"
	case Byte:
		return "b"
	default:
		return "?"
	}
}

type TypeSize int

func (this TypeSize) String() string {
	switch this {
	case Byte:
		return "byte"
	case Word:
		return "word"
	case DoubleWord:
		return "dword"
	case QuadWord:
		return "qword"
	default:
		return "??"
	}
}

const (
	InvalidTypeSize TypeSize = iota
	Byte
	Word
	DoubleWord
	QuadWord
)

// rN(d|w|b|\e)
// rbp, rsp, rip
func StringToReg(s string) (Register, bool) {
	if s == "" || s[0] != 'r' {
		return Register{}, false
	}
	switch s {
	case "rsp":
		return Register{
			ID:       4,
			TypeSize: QuadWord,
		}, true
	case "rbp":
		return Register{
			ID:       5,
			TypeSize: QuadWord,
		}, true
	case "rip":
		return Register{
			ID:       16,
			TypeSize: QuadWord,
		}, true
	}
	start, end := findDigits(s)
	i, err := strconv.ParseInt(s[start:end+1], 10, 64)
	if err != nil {
		return Register{}, false
	}
	var sz TypeSize = QuadWord
	if end+1 < len(s) {
		switch s[end+1] {
		case 'd':
			sz = DoubleWord
		case 'w':
			sz = Word
		case 'b':
			sz = Byte
		}
	}
	return Register{
		ID:       int(i),
		TypeSize: sz,
	}, true
}

func findDigits(s string) (int, int) {
	start := -1
	end := -1
	for i := range s {
		digit := isdigit(s[i])
		if start == -1 && digit {
			start = i
		}
		if digit {
			end = i
		}
	}
	return start, end
}

func isdigit(c byte) bool {
	return c >= '0' && c <= '9'
}
