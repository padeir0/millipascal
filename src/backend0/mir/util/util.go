package util

import (
	"mpc/backend0/mir"
	mik "mpc/backend0/mir/instrkind"
)

func Load(source, destination mir.Operand) mir.Instr {
	return mir.Instr{
		T:    mik.Load,
		Type: source.Type,
		A:    mir.OptOperand_(source),
		Dest: mir.OptOperand_(destination),
	}
}

func Store(source, destination mir.Operand) mir.Instr {
	return mir.Instr{
		T:    mik.Store,
		Type: source.Type,
		A:    mir.OptOperand_(source),
		Dest: mir.OptOperand_(destination),
	}
}

func StorePtr(source, ptr mir.Operand) mir.Instr {
	return mir.Instr{
		T:    mik.StorePtr,
		Type: source.Type,
		A:    mir.OptOperand_(source),
		B:    mir.OptOperand_(ptr),
	}
}

func LoadPtr(ptr, dest mir.Operand) mir.Instr {
	return mir.Instr{
		T:    mik.LoadPtr,
		Type: dest.Type,
		A:    mir.OptOperand_(ptr),
		Dest: mir.OptOperand_(dest),
	}
}

func Convert(a, dest mir.Operand) *mir.Instr {
	return &mir.Instr{
		T:    mik.Convert,
		Type: dest.Type,
		A:    mir.OptOperand_(a),
		Dest: mir.OptOperand_(dest),
	}
}

func Copy(source, destination mir.Operand) *mir.Instr {
	return &mir.Instr{
		T:    mik.Copy,
		Type: source.Type,
		A:    mir.OptOperand_(source),
		Dest: mir.OptOperand_(destination),
	}
}
