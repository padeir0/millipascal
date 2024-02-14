package util

import (
	"mpc/pir"
	IT "mpc/pir/instrkind"
)

func StorePtr(source, ptr pir.Operand) pir.Instr {
	return pir.Instr{
		T:        IT.StorePtr,
		Type:     source.Type,
		Operands: []pir.Operand{source, ptr},
	}
}

func LoadPtr(ptr, dest pir.Operand) pir.Instr {
	return pir.Instr{
		T:           IT.LoadPtr,
		Type:        dest.Type,
		Operands:    []pir.Operand{ptr},
		Destination: []pir.Operand{dest},
	}
}

func Convert(a, dest pir.Operand) pir.Instr {
	return pir.Instr{
		T:           IT.Convert,
		Type:        dest.Type,
		Operands:    []pir.Operand{a},
		Destination: []pir.Operand{dest},
	}
}

func Copy(source, destination pir.Operand) pir.Instr {
	return pir.Instr{
		T:           IT.Copy,
		Type:        source.Type,
		Operands:    []pir.Operand{source},
		Destination: []pir.Operand{destination},
	}
}
