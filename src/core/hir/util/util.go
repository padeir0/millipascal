package util

import (
	"mpc/core/hir"
	IT "mpc/core/hir/instrkind"
)

func Load(source, destination *hir.Operand) *hir.Instr {
	return &hir.Instr{
		T:           IT.Load,
		Type:        source.Type,
		Operands:    []*hir.Operand{source},
		Destination: []*hir.Operand{destination},
	}
}

func Store(source, destination *hir.Operand) *hir.Instr {
	return &hir.Instr{
		T:           IT.Store,
		Type:        source.Type,
		Operands:    []*hir.Operand{source},
		Destination: []*hir.Operand{destination},
	}
}

func StorePtr(source, ptr *hir.Operand) *hir.Instr {
	return &hir.Instr{
		T:        IT.StorePtr,
		Type:     source.Type,
		Operands: []*hir.Operand{source, ptr},
	}
}

func LoadPtr(ptr, dest *hir.Operand) *hir.Instr {
	return &hir.Instr{
		T:           IT.LoadPtr,
		Type:        dest.Type,
		Operands:    []*hir.Operand{ptr},
		Destination: []*hir.Operand{dest},
	}
}

func Convert(a, dest *hir.Operand) *hir.Instr {
	return &hir.Instr{
		T:           IT.Convert,
		Type:        dest.Type,
		Operands:    []*hir.Operand{a},
		Destination: []*hir.Operand{dest},
	}
}

func Copy(source, destination *hir.Operand) *hir.Instr {
	return &hir.Instr{
		T:           IT.Copy,
		Type:        source.Type,
		Operands:    []*hir.Operand{source},
		Destination: []*hir.Operand{destination},
	}
}
