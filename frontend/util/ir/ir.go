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

func StorePtr(source, ptr *ir.Operand) *ir.Instr {
	return &ir.Instr{
		T: IT.StorePtr,
		Type: source.Type,
		Operands: []*ir.Operand{source, ptr},
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

func Copy(source, destination *ir.Operand) *ir.Instr {
	return &ir.Instr{
		T: IT.Copy,
		Type: source.Type,
		Operands: []*ir.Operand{source},
		Destination: []*ir.Operand{destination},
	}
}
