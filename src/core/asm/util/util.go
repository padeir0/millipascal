package util

import (
	"math/big"
	"mpc/core/asm"
	. "mpc/core/asm/instrkind"
	T "mpc/core/types"
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
