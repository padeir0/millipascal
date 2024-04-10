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
	ik "mpc/core/asm/instrkind"
	au "mpc/core/asm/util"

	mod "mpc/core/module"
	gk "mpc/core/module/globalkind"
	lk "mpc/core/module/lexkind"
	lck "mpc/core/module/localkind"
	msg "mpc/messages"

	cc "mpc/core/cc/stack"

	"fmt"
	"strconv"
	"strings"
)

func GenAsmProcs(M *mod.Module) *Error {
	M.ResetVisited()
	return genMod(M)
}

func genMod(M *mod.Module) *Error {
	if M.Visited {
		return nil
	}
	M.Visited = true
	for _, dep := range M.Dependencies {
		err := genMod(dep.M)
		if err != nil {
			return err
		}
	}
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
	lines := body.Leaves[0]
	labels, err := findLabels(M, lines)
	if err != nil {
		return err
	}
	ctx := context{
		M:      M,
		Sy:     sy,
		Labels: labels,
	}
	asmlines, err := transformLines(ctx, lines)
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

/*
Labels are only identifiers in the source asm,
but need to be converted to local labels here.
*/
func convertLabel(s string) string {
	return "." + s
}

func transformLines(ctx context, lines *mod.Node) ([]asm.Line, *Error) {
	output := []asm.Line{}
	for _, line := range lines.Leaves {
		if line.Lex == lk.DOT {
			text := line.Leaves[0].Text
			lbl := au.LabelLine(convertLabel(text))
			output = append(output, lbl)
		}
		if line.Lex == lk.INSTR {
			id := line.Leaves[0]
			kind := ik.StringToKind(id.Text)
			if kind == ik.InvalidInstrKind {
				return nil, msg.InvalidInstr(ctx.M, id)
			}
			ops := line.Leaves[1]
			asmOps := []asm.Operand{}
			for _, op := range ops.Leaves {
				asmOp, err := convertOp(ctx, op, false)
				if err != nil {
					return nil, err
				}
				asmOps = append(asmOps, asmOp)
			}
			instr := au.InstrLine(kind, asmOps)
			output = append(output, instr)
		}
	}
	return output, nil
}

type context struct {
	M      *mod.Module
	Sy     *mod.Global
	Labels map[string]struct{}
}

func convertOp(ctx context, op *mod.Node, insideAddr bool) (asm.Operand, *Error) {
	switch op.Lex {
	case lk.IDENTIFIER:
		return convertID(ctx, op)
	case lk.LEFTBRACKET:
		if insideAddr {
			return au.BadOp(), msg.InvalidNestedAddr(ctx.M, op)
		}
		op, err := convAddr(ctx, op)
		if err != nil {
			return au.BadOp(), err
		}
		return op, nil
	case lk.LEFTBRACE: // consteval pre-computed this already :)
		return au.Const(op.Value), nil
	case lk.I64_LIT, lk.I32_LIT, lk.I16_LIT, lk.I8_LIT,
		lk.U64_LIT, lk.U32_LIT, lk.U16_LIT, lk.U8_LIT,
		lk.CHAR_LIT, lk.TRUE, lk.FALSE, lk.PTR_LIT:
		return au.Const(op.Value), nil
	default:
		fmt.Println(op)
		panic("unreachable")
	}
}

func convAddr(ctx context, op *mod.Node) (asm.Operand, *Error) {
	ops := op.Leaves[0]
	tp := op.Leaves[1]
	tsize := strToTypeSize(tp.Text)
	if tsize == asm.InvalidTypeSize {
		return au.BadOp(), msg.InvalidTypeSize(ctx.M, op)
	}
	if len(ops.Leaves) == 1 { // [a]
		a := ops.Leaves[0]
		Aop, err := convertOp(ctx, a, true)
		if err != nil {
			return au.BadOp(), err
		}
		return au.AddrSimple(Aop, tsize), nil
	}
	if len(ops.Leaves) == 2 { // [a, b]
		a := ops.Leaves[0]
		Aop, err := convertOp(ctx, a, true)
		if err != nil {
			return au.BadOp(), err
		}
		b := ops.Leaves[1]
		Bop, err := convertOp(ctx, b, true)
		if err != nil {
			return au.BadOp(), err
		}
		return au.Addr(Aop, Bop, tsize), nil
	}
	return au.BadOp(), msg.InvalidOperand(ctx.M, op)
}

func convertID(ctx context, op *mod.Node) (asm.Operand, *Error) {
	text := op.Text
	reg, ok := au.StringToReg(text)
	if ok {
		return reg, nil
	}
	special, ok := stringToSpecial(text)
	if ok {
		return special, nil
	}
	lbl, ok := convLabelOp(ctx, text)
	if ok {
		return lbl, nil
	}
	local, ok := convLocalOp(ctx, text)
	if ok {
		return local, nil
	}
	global, ok := convGlobalOp(ctx, text)
	if ok {
		return global, nil
	}
	return au.BadOp(), msg.ErrorNameNotDefined(ctx.M, op)
}

const (
	argPrefix = "_arg"
	retPrefix = "_ret"
)

func stringToSpecial(s string) (asm.Operand, bool) {
	start, end := au.FindDigits(s)
	var num int
	if start != -1 && end != -1 {
		n, err := strconv.ParseInt(s[start:end+1], 10, 64)
		if err != nil {
			return au.BadOp(), false
		}
		num = int(n)
	}
	if strings.HasPrefix(s, argPrefix) {
		offset := cc.Arg(num)
		return au.ConstInt(offset), true
	}
	if strings.HasPrefix(s, retPrefix) {
		offset := cc.Ret(num)
		return au.ConstInt(offset), true
	}
	return au.BadOp(), false
}

func convLabelOp(ctx context, s string) (asm.Operand, bool) {
	_, ok := ctx.Labels[s]
	if ok {
		return au.LabelOp(convertLabel(s)), true
	}
	return au.BadOp(), false
}

func convLocalOp(ctx context, s string) (asm.Operand, bool) {
	if ctx.Sy.Proc == nil {
		panic("proc was nil!!!210!!")
	}
	proc := ctx.Sy.Proc
	local := proc.GetLocal(s)
	if local == nil {
		return au.BadOp(), false
	}
	if local.Kind == lck.Argument {
		offset := cc.Arg(local.Position)
		return au.ConstInt(offset), true
	} else if local.Kind == lck.Variable {
		offset := cc.Var(local.Position)
		return au.ConstInt(offset), true
	} else {
		panic("unreachable")
	}
}

func convGlobalOp(ctx context, s string) (asm.Operand, bool) {
	sy := ctx.M.GetSymbol(s)
	if sy != nil {
		label := sy.Label()
		return au.LabelOp(label), true
	}
	return au.BadOp(), false
}

func strToTypeSize(s string) asm.TypeSize {
	switch s {
	case "qword":
		return asm.QuadWord
	case "dword":
		return asm.DoubleWord
	case "word":
		return asm.Word
	case "byte":
		return asm.Byte
	default:
		return asm.InvalidTypeSize
	}
}
