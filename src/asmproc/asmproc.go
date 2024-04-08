/*
This package transforms Asm procedures from AST to []asm.Line,
we expect this to run after consteval and before linearization,
so that all global consts are evaluated.

Things to think about:
 - How will globals, like procedures and data declarations, be passed? ie,
 how will you mangle their names so that AsmProcs can find symbols without
 resorting to weird stuff. Probably by specifying the {mod}_{symbolname}
 name mangling as default, as it is unique and simple.
*/
package asmproc

import (
	. "mpc/core"
	"mpc/core/asm"
	// ik "mpc/core/asm/instrkind"
	mod "mpc/core/module"
	gk "mpc/core/module/globalkind"
	lk "mpc/core/module/lexkind"
	msg "mpc/messages"
)

func GenAsmProcs(M *mod.Module) *Error {
	for _, sy := range M.Globals {
		switch sy.Kind {
		case gk.Proc:
			err := genProc(M, sy)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func genProc(M *mod.Module, sy *mod.Global) *Error {
	body := sy.N.Leaves[4]
	if body.Lex != lk.ASM {
		return nil
	}
	lines := body.Leaves[1]
	labels, err := findLabels(M, lines)
	if err != nil {
		return err
	}
	asmlines, err := transformLines(M, sy, labels, lines)
	if err != nil {
		return err
	}
	sy.Proc.Asm = asmlines
	return nil
}

func findLabels(M *mod.Module, lines *mod.Node) (map[string]struct{}, *Error) {
	output := map[string]struct{}{}
	for _, line := range lines.Leaves {
		if line.Lex == lk.DOT {
			id := line.Leaves[0]
			if _, ok := output[id.Text]; ok {
				return nil, msg.DuplicatedLabel(M, line)
			}
			output[id.Text] = struct{}{}
		}
	}
	return output, nil
}

func transformLines(M *mod.Module, sy *mod.Global, labels map[string]struct{}, lines *mod.Node) ([]asm.Line, *Error) {
	output := []asm.Line{}
	for _, line := range lines.Leaves {
		if line.Lex == lk.INSTR {
			panic("unimplemented")
		}
	}
	return output, nil
}

func evalOpList(M *mod.Module, opList *mod.Node, insideAddr bool) *Error {
	for _, op := range opList.Leaves {
		if op.Lex == lk.LEFTBRACE {
			panic("unimplemented")
		}
		if op.Lex == lk.LEFTBRACKET {
			if insideAddr {
				return msg.InvalidNestedAddr(M, op)
			}
			panic("unimplemented")
		}
	}
	return nil
}
