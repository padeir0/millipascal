package ir

import (
	"mpc/frontend/ir"
	IT "mpc/frontend/enums/instrType"
)

func Load(source, destination *ir.Operand) *ir.Instr {
	return &ir.Instr{
		T: IT.Load,
		Type: source.Type,
		Operands: []*ir.Operand{source},
		Destination: []*ir.Operand{destination},
	}
}

func Store(source, destination *ir.Operand) *ir.Instr {
	return &ir.Instr {
		T: IT.Store,
		Type: source.Type,
		Operands: []*ir.Operand{source},
		Destination: []*ir.Operand{destination},
	}
}

func Offset(basePtr, amount, newPtr *ir.Operand) *ir.Instr {
	return &ir.Instr {
		T: IT.Offset,
		Type: amount.Type,
		Operands: []*ir.Operand{basePtr, amount},
		Destination: []*ir.Operand{newPtr},
	}
}

func StorePtr(source, ptr *ir.Operand) *ir.Instr {
	return &ir.Instr{
		T: IT.StorePtr,
		Type: source.Type,
		Operands: []*ir.Operand{source},
		Destination: []*ir.Operand{ptr},
	} 
}

func LoadPtr(ptr, dest *ir.Operand) *ir.Instr {
	return &ir.Instr{
		T: IT.LoadPtr,
		Type: dest.Type,
		Operands: []*ir.Operand{ptr},
		Destination: []*ir.Operand{dest},
	} 
}

func Convert(a, dest *ir.Operand) *ir.Instr {
	return &ir.Instr{
		T:           IT.Convert,
		Type:        dest.Type,
		Operands:    []*ir.Operand{a},
		Destination: []*ir.Operand{dest},
	}
}
