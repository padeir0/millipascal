package util

import (
	"mpc/pir"
	IK "mpc/pir/instrkind"
)

func StorePtr(source, ptr pir.Operand) pir.Instr {
	return pir.Instr{
		T:        IK.StorePtr,
		Type:     source.Type,
		Operands: []pir.Operand{source, ptr},
	}
}

func LoadPtr(ptr, dest pir.Operand) pir.Instr {
	return pir.Instr{
		T:           IK.LoadPtr,
		Type:        dest.Type,
		Operands:    []pir.Operand{ptr},
		Destination: []pir.Operand{dest},
	}
}

func Convert(a, dest pir.Operand) pir.Instr {
	return pir.Instr{
		T:           IK.Convert,
		Type:        dest.Type,
		Operands:    []pir.Operand{a},
		Destination: []pir.Operand{dest},
	}
}

func Copy(source, destination pir.Operand) pir.Instr {
	return pir.Instr{
		T:           IK.Copy,
		Type:        source.Type,
		Operands:    []pir.Operand{source},
		Destination: []pir.Operand{destination},
	}
}

func Bin(kind IK.InstrKind, a, b, dest pir.Operand) pir.Instr {
	return pir.Instr{
		T:           kind,
		Type:        dest.Type,
		Operands:    []pir.Operand{a, b},
		Destination: []pir.Operand{dest},
	}
}

func BinOut(kind IK.InstrKind, a, b, dest pir.Operand) pir.Instr {
	return pir.Instr{
		T:           kind,
		Type:        a.Type, // takes the type of 'a' instead
		Operands:    []pir.Operand{a, b},
		Destination: []pir.Operand{dest},
	}
}

func Un(kind IK.InstrKind, a, dest pir.Operand) pir.Instr {
	return pir.Instr{
		T:           kind,
		Type:        dest.Type,
		Operands:    []pir.Operand{a},
		Destination: []pir.Operand{dest},
	}
}
