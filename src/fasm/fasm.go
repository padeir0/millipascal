package fasm

import (
	"math/big"
	"mpc/core/asm"
	ik "mpc/core/asm/instrkind"
	. "mpc/core/strbuilder"
	"strconv"
	"strings"
)

func Generate(program *asm.Program) string {
	b := &Builder{}
	b.Place("format ELF64 executable 3\n")
	b.Place("\nsegment readable\n")
	for _, data := range program.Readonly {
		genData(b, data)
	}
	b.Place("\nsegment readable writable\n")
	for _, data := range program.Writable {
		genData(b, data)
		b.Place("\n")
	}
	b.Place("\nsegment readable executable\n")
	b.Place("entry $\n")
	for _, line := range program.Executable {
		genLine(b, line)
		b.Place("\n")
	}
	return b.String()
}

func genData(b *Builder, data asm.Data) {
	b.Place(data.Label)
	b.Place(" ")
	if data.Str != "" {
		b.Place(convertString(data.Str))
	} else if data.Blob == nil {
		b.Place(" rb ")
		b.Place(strconv.Itoa(data.Size))
	} else {
		b.Place(convertNums(data.Blob))
	}
}

func convertNums(entries []asm.DataEntry) string {
	b := &Builder{}
	for i, entry := range entries {
		s := genDataSize(entry.Type)
		b.Place(s)
		if entry.Num.Cmp(zero) == -1 {
			b.Place("-")
		}
		b.Place("0x")
		b.Place(scratch.Abs(entry.Num).Text(16))
		if i < len(entries)-1 {
			b.Place("\n")
		}
	}
	return b.String()
}

func genDataSize(size asm.TypeSize) string {
	switch size {
	case asm.Byte:
		return " db " // [b]yte
	case asm.Word:
		return " dw " // [w]ord
	case asm.DoubleWord:
		return " dd " // [d]ouble word
	case asm.QuadWord:
		return " dq " // [q]uad word
	default:
		panic("unknown size")
	}
}

func genLine(b *Builder, line asm.Line) {
	if line.IsLabel {
		b.Place(line.Label)
		b.Place(":")
	} else {
		instr := line.Instr
		b.Place("\t")
		b.Place(ik.KindToString(instr.Kind))
		b.Place("\t")
		for i, op := range instr.Operands {
			if i != 0 {
				b.Place(", ")
			}
			switch op.Kind {
			case asm.Simple:
				b.Place(valueToString(op.A))
			case asm.Addressing:
				genAddr(b, op)
			}
		}
	}
}

func genAddr(b *Builder, op asm.Operand) {
	b.Place(op.TypeSize.String())
	b.Place("[")
	b.Place(valueToString(op.A))
	if op.B.Kind != asm.InvalidValueKind {
		if op.B.Const != nil {
			if op.B.Const.Cmp(zero) == -1 {
				b.Place(" - ")
				s := convNum(scratch.Abs(op.B.Const))
				b.Place(s)
				b.Place("]")
				return
			}
		}
		b.Place(" + ")
		b.Place(valueToString(op.B))
		b.Place("]")
	} else {
		b.Place("]")
	}
}

func valueToString(v asm.Value) string {
	switch v.Kind {
	case asm.Const:
		return convNum(v.Const)
	case asm.Label:
		return v.Label
	case asm.Reg:
		return genReg(v.Reg)
	default:
		return "???"
	}
}

type register struct {
	QWord string
	DWord string
	Word  string
	Byte  string
}

var registers = []*register{
	{QWord: "rax", DWord: "eax", Word: "ax", Byte: "al"},
	{QWord: "rcx", DWord: "ecx", Word: "cx", Byte: "cl"},
	{QWord: "rdx", DWord: "edx", Word: "dx", Byte: "dl"},
	{QWord: "rbx", DWord: "ebx", Word: "bx", Byte: "bl"},

	{QWord: "rsp", DWord: "esp", Word: "sp", Byte: "spl"},
	{QWord: "rbp", DWord: "ebp", Word: "bp", Byte: "bpl"},
	{QWord: "rsi", DWord: "esi", Word: "si", Byte: "sil"},
	{QWord: "rdi", DWord: "edi", Word: "di", Byte: "dil"},

	{QWord: "r8", DWord: "r8d", Word: "r8w", Byte: "r8b"},
	{QWord: "r9", DWord: "r9d", Word: "r9w", Byte: "r9b"},
	{QWord: "r10", DWord: "r10d", Word: "r10w", Byte: "r10b"},
	{QWord: "r11", DWord: "r11d", Word: "r11w", Byte: "r11b"},

	{QWord: "r12", DWord: "r12d", Word: "r12w", Byte: "r12b"},
	{QWord: "r13", DWord: "r13d", Word: "r13w", Byte: "r13b"},
	{QWord: "r14", DWord: "r14d", Word: "r14w", Byte: "r14b"},
	{QWord: "r15", DWord: "r15d", Word: "r15w", Byte: "r15b"},
}

func genReg(v asm.Register) string {
	r := registers[v.ID]
	switch v.TypeSize {
	case asm.QuadWord:
		return r.QWord
	case asm.DoubleWord:
		return r.DWord
	case asm.Word:
		return r.Word
	case asm.Byte:
		return r.Byte
	}
	panic("invalid type size")
}

var zero = big.NewInt(0)
var scratch = big.NewInt(0)

func convNum(num *big.Int) string {
	sign := ""
	if num.Cmp(zero) == -1 {
		sign = "-"
	}
	return sign + "0x" + strings.ToUpper(scratch.Abs(num).Text(16))
}

func convertString(original string) string {
	s := original[1 : len(original)-1] // removes quotes
	output := " db '"
	for i := 0; i < len(s); i++ {
		r := s[i]
		if r == '\\' {
			i++
			r = s[i]
			switch r {
			case 'n':
				output += "', 0xA, '"
			case 't':
				output += "', 0x9, '"
			case 'r':
				output += "', 0xD, '"
			case '\'':
				output += "', 0x27, '"
			case '\\':
				output += "', 0x5C, '"
			default:
				output += string(r)
			}
		} else {
			output += string(r)
		}
	}
	output += "'"
	return output
}
