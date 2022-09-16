package codegen

import (
	"mpc/frontend/ast"
	T "mpc/frontend/enums/Type"
	ST "mpc/frontend/enums/symbolType"

	"strconv"
)

// if operand is
// 	Stack    -> use Num as offset after vars
//	Register -> use label as register
//	Local    -> use label for load/store
//	Mem      -> use label for load/store
//
// Assume RDX and RAX are never used as general purpose

type register struct {
	Qword string
	Dword string
	Word  string
	Byte  string
}

var X64Registers = []*register{
	{Qword: "r15", Dword: "r15d", Word: "r15w", Byte: "r15b"},
	{Qword: "r14", Dword: "r14d", Word: "r14w", Byte: "r14b"},
	{Qword: "r13", Dword: "r13d", Word: "r13w", Byte: "r13b"},
	{Qword: "r12", Dword: "r12d", Word: "r12w", Byte: "r12b"},
	{Qword: "r11", Dword: "r11d", Word: "r11w", Byte: "r11b"},
	{Qword: "r10", Dword: "r10d", Word: "r10w", Byte: "r10b"},
	{Qword: "r9", Dword: "r9d", Word: "r9w", Byte: "r9b"},
	{Qword: "r8", Dword: "r8d", Word: "r8w", Byte: "r8b"},
	{Qword: "rdi", Dword: "edi", Word: "di", Byte: "dil"},
	{Qword: "rsi", Dword: "esi", Word: "si", Byte: "sil"},
	{Qword: "rcx", Dword: "ecx", Word: "cx", Byte: "cl"},
	{Qword: "rbx", Dword: "ebx", Word: "bx", Byte: "bl"},
}

func Generate(M *ast.Module) string {
	return ""
}

func genDataRegion(M *ast.Module) string {
	return ""
}

func genExecutableRegion(M *ast.Module) string {
	output := ""
	for _, sy := range M.Globals {
		switch sy.T {
		case ST.Proc:
			output += genProc(M, sy.Proc)
		}
	}
	return output
}

func genProc(M *ast.Module, P *ast.Proc) string {
	return ""
}

func genVars(M *ast.Module, P *ast.Proc) (map[string]string, int) {
	output := map[string]string{}
	offset := 0
	for _, v := range P.Vars {
		output[v.Name] = genVarAddr(v, offset)
		offset += 8
	}
	return output, offset
}

func genVarAddr(d *ast.Decl, offset int) string {
	return typeToAsm(d.Type) + " [rbp - " + strconv.Itoa(offset) + "]"
}

func typeToAsm(t T.Type) string {
	switch t {
	case T.Byte:  return "byte"
	case T.Word:  return "word"
	case T.DWord: return "dword"
	case T.QWord: return "qword"
	}
	panic("invalid type: "+ t.String())
}
