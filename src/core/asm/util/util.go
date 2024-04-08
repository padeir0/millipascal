package util

import (
	"math/big"
	"mpc/core/asm"
	. "mpc/core/asm/instrkind"
	T "mpc/core/types"
	"strconv"
)

var RSP = asm.Operand{
	Kind: asm.Simple,
	A: asm.Value{
		Kind: asm.Reg,
		Reg: asm.Register{
			ID:       4,
			TypeSize: asm.QuadWord,
		},
	},
}

var RBP = asm.Operand{
	Kind: asm.Simple,
	A: asm.Value{
		Kind: asm.Reg,
		Reg: asm.Register{
			ID:       5,
			TypeSize: asm.QuadWord,
		},
	},
}

func ConstInt(num int) asm.Operand {
	return asm.Operand{
		Kind: asm.Simple,
		A: asm.Value{
			Kind:  asm.Const,
			Const: big.NewInt(int64(num)),
		},
	}
}

func Const(num *big.Int) asm.Operand {
	return asm.Operand{
		Kind: asm.Simple,
		A: asm.Value{
			Kind:  asm.Const,
			Const: num,
		},
	}
}

func Reg(id int, size asm.TypeSize) asm.Operand {
	return asm.Operand{
		Kind: asm.Simple,
		A: asm.Value{
			Kind: asm.Reg,
			Reg: asm.Register{
				ID:       id,
				TypeSize: size,
			},
		},
	}
}

func LabelOp(lbl string) asm.Operand {
	return asm.Operand{
		Kind: asm.Simple,
		A: asm.Value{
			Kind:  asm.Label,
			Label: lbl,
		},
	}
}

func LabelLine(lbl string) asm.Line {
	return asm.Line{
		Label:   lbl,
		IsLabel: true,
	}
}

func InstrLine(kind InstrKind, ops []asm.Operand) asm.Line {
	return asm.Line{
		Instr: asm.Instr{
			Kind:     kind,
			Operands: ops,
		},
		IsLabel: false,
	}
}

func AddrFrame(a int, size asm.TypeSize) asm.Operand {
	return asm.Operand{
		Kind: asm.Addressing,
		A:    RBP.A,
		B: asm.Value{
			Kind:  asm.Const,
			Const: big.NewInt(int64(a)),
		},
		TypeSize: size,
	}
}

func Addr(op asm.Operand, size asm.TypeSize) asm.Operand {
	return asm.Operand{
		Kind: asm.Addressing,
		A:    op.A,
		B: asm.Value{
			Kind: asm.InvalidValueKind,
		},
		TypeSize: size,
	}
}

func Unary(kind InstrKind, op asm.Operand) asm.Line {
	return asm.Line{
		Instr: asm.Instr{
			Kind:     kind,
			Operands: []asm.Operand{op},
		},
	}
}

func Bin(kind InstrKind, a, b asm.Operand) asm.Line {
	return asm.Line{
		Instr: asm.Instr{
			Kind:     kind,
			Operands: []asm.Operand{a, b},
		},
	}
}

func Plain(kind InstrKind) asm.Line {
	return asm.Line{
		Instr: asm.Instr{
			Kind:     kind,
			Operands: []asm.Operand{},
		},
	}
}

func BadOp() asm.Operand {
	return asm.Operand{}
}

func TypeToTsize(t *T.Type) asm.TypeSize {
	switch t.Size() {
	case 1:
		return asm.Byte
	case 2:
		return asm.Word
	case 4:
		return asm.DoubleWord
	case 8:
		return asm.QuadWord
	default:
		return asm.InvalidTypeSize
	}
}

func StringToReg(s string) (asm.Operand, bool) {
	reg, ok := stringToReg(s)
	if !ok {
		return BadOp(), false
	}
	return asm.Operand{
		Kind: asm.Simple,
		A: asm.Value{
			Kind: asm.Reg,
			Reg:  reg,
		},
	}, true
}

// rN(d|w|b|\e)
// rbp, rsp, rip
func stringToReg(s string) (asm.Register, bool) {
	if s == "" || s[0] != 'r' {
		return asm.Register{}, false
	}
	switch s {
	case "rsp":
		return asm.Register{
			ID:       4,
			TypeSize: asm.QuadWord,
		}, true
	case "rbp":
		return asm.Register{
			ID:       5,
			TypeSize: asm.QuadWord,
		}, true
	case "rip":
		return asm.Register{
			ID:       16,
			TypeSize: asm.QuadWord,
		}, true
	}
	start, end := FindDigits(s)
	i, err := strconv.ParseInt(s[start:end+1], 10, 64)
	if err != nil {
		return asm.Register{}, false
	}
	var sz asm.TypeSize = asm.QuadWord
	if end+1 < len(s) {
		switch s[end+1] {
		case 'd':
			sz = asm.DoubleWord
		case 'w':
			sz = asm.Word
		case 'b':
			sz = asm.Byte
		}
	}
	return asm.Register{
		ID:       int(i),
		TypeSize: sz,
	}, true
}

func FindDigits(s string) (int, int) {
	start := -1
	end := -1
	for i := range s {
		digit := IsDigit(s[i])
		if start == -1 && digit {
			start = i
		}
		if digit {
			end = i
		}
	}
	return start, end
}

func IsDigit(c byte) bool {
	return c >= '0' && c <= '9'
}
