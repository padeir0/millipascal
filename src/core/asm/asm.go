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
