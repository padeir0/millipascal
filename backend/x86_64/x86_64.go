package x86_64

import (
	ir "mpc/frontend/ir"
)

const (
	InvalidLIRInstrType ir.LIRInstrType = iota

	Add
	Sub
	IMul
	IDiv

	Mov

	Sete
	Setne
	Setg
	Setge
	Setl
	Setle

	Jmp
	Je
	Jne
	Jg
	Jge
	Jl
	Jle

	Call
/*      ...      */
)

type register struct {
	QWord string
	DWord string
	Word  string
	Byte  string
}

// we use this two as scratch space, IDIV already needs these, so no matter
var RAX = &register{QWord: "rax", DWord: "eax", Word: "ax", Byte: "al"}
var RDX = &register{QWord: "rdx", DWord: "edx", Word: "dx", Byte: "dl"}

var X64Registers = []*register{
	{QWord: "r15", DWord: "r15d", Word: "r15w", Byte: "r15b"},
	{QWord: "r14", DWord: "r14d", Word: "r14w", Byte: "r14b"},
	{QWord: "r13", DWord: "r13d", Word: "r13w", Byte: "r13b"},
	{QWord: "r12", DWord: "r12d", Word: "r12w", Byte: "r12b"},
	{QWord: "r11", DWord: "r11d", Word: "r11w", Byte: "r11b"},
	{QWord: "r10", DWord: "r10d", Word: "r10w", Byte: "r10b"},
	{QWord: "r9", DWord: "r9d", Word: "r9w", Byte: "r9b"},
	{QWord: "r8", DWord: "r8d", Word: "r8w", Byte: "r8b"},
	{QWord: "rdi", DWord: "edi", Word: "di", Byte: "dil"},
	{QWord: "rsi", DWord: "esi", Word: "si", Byte: "sil"},
	{QWord: "rcx", DWord: "ecx", Word: "cx", Byte: "cl"},
	{QWord: "rbx", DWord: "ebx", Word: "bx", Byte: "bl"},
}
