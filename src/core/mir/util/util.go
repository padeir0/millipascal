package util

import (
	"mpc/core/mir"
	mik "mpc/core/mir/instrkind"
)

func Load(source, destination *mir.Operand) *mir.Instr {
	return &mir.Instr{
		T:    mik.Load,
		Type: source.Type,
		A:    source,
		Dest: destination,
	}
}

func Store(source, destination *mir.Operand) *mir.Instr {
	return &mir.Instr{
		T:    mik.Store,
		Type: source.Type,
		A:    source,
		Dest: destination,
	}
}

func StorePtr(source, ptr *mir.Operand) *mir.Instr {
	return &mir.Instr{
		T:    mik.StorePtr,
		Type: source.Type,
		A:    source,
		B:    ptr,
	}
}

func LoadPtr(ptr, dest *mir.Operand) *mir.Instr {
	return &mir.Instr{
		T:    mik.LoadPtr,
		Type: dest.Type,
		A:    ptr,
		Dest: dest,
	}
}

func Convert(a, dest *mir.Operand) *mir.Instr {
	return &mir.Instr{
		T:    mik.Convert,
		Type: dest.Type,
		A:    a,
		Dest: dest,
	}
}

func Copy(source, destination *mir.Operand) *mir.Instr {
	return &mir.Instr{
		T:    mik.Copy,
		Type: source.Type,
		A:    source,
		Dest: destination,
	}
}
