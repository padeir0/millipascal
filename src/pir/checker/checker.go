package checker

import (
	. "mpc/pir/errors"

	hir "mpc/pir"
	hirc "mpc/pir/class"
	eu "mpc/pir/errors/util"
	FT "mpc/pir/flowkind"
	IT "mpc/pir/instrkind"
	T "mpc/pir/types"

	"strconv"
	"strings"
)

func Check(P *hir.Program) *Error {
	for _, sy := range P.Symbols {
		if sy.Proc != nil && !sy.Builtin {
			s := newState(P)
			s.proc = sy.Proc
			sy.Proc.ResetBlocks()
			err := checkCode(s, sy.Proc.FirstBlock())
			if err != nil {
				return err
			}
			err = checkVisited(sy.Proc)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

type state struct {
	m    *hir.Program
	proc *hir.Procedure
	bb   *hir.BasicBlock
}

func newState(P *hir.Program) *state {
	return &state{
		m: P,
	}
}

func checkVisited(proc *hir.Procedure) *Error {
	notVisited := []string{}
	for _, bb := range proc.AllBlocks {
		if !bb.Visited {
			notVisited = append(notVisited, bb.Label)
		}
	}
	if len(notVisited) > 0 {
		return eu.NewInternalSemanticError(proc.Label +
			": not all blocks are reachable (" +
			strings.Join(notVisited, ", ") + ")")
	}
	return nil
}

func checkCode(s *state, bb *hir.BasicBlock) *Error {
	if bb.Visited {
		return nil
	}
	s.bb = bb
	for _, instr := range bb.Code {
		err := checkInstr(s, instr)
		if err != nil {
			return err
		}
	}
	bb.Visited = true
	return checkJump(s)
}

func checkJump(s *state) *Error {
	bb := s.bb
	switch bb.Out.T {
	case FT.Jmp:
		t := s.proc.GetBlock(bb.Out.True)
		return checkCode(s, t)
	case FT.If:
		t := s.proc.GetBlock(bb.Out.True)
		err := checkCode(s, t)
		if err != nil {
			return err
		}
		f := s.proc.GetBlock(bb.Out.False)
		return checkCode(s, f)
	case FT.Return:
		return checkRet(s, bb.Out.V)
	case FT.Exit:
		return checkExit(s, bb.Out)
	}
	return invalidFlow(bb.Out)
}

func checkRet(s *state, rets []hir.Operand) *Error {
	if len(s.proc.Rets) != len(rets) {
		has := strconv.Itoa(len(rets))
		wants := strconv.Itoa(len(s.proc.Rets))
		return eu.NewInternalSemanticError("invalid number of returns: has " + has + " wanted " + wants)
	}
	for i, wanted_ret := range s.proc.Rets {
		curr_ret := rets[i]
		if !wanted_ret.Equals(curr_ret.Type) {
			has := curr_ret.Type.String()
			wants := wanted_ret.String()
			return eu.NewInternalSemanticError("invalid return for procedure: has " + has + " wanted " + wants)
		}
	}
	return nil
}

func checkExit(s *state, branch hir.Flow) *Error {
	if branch.V == nil {
		return eu.NewInternalSemanticError("invalid exit with zero operands")
	}
	if len(branch.V) != 1 {
		return eu.NewInternalSemanticError("exit should have one operand")
	}
	if !branch.V[0].Type.Equals(T.T_I8) {
		return eu.NewInternalSemanticError("exit operand must be I8")
	}
	return nil
}

type Checker struct {
	Class func(hirc.Class) bool
	Type  func(*T.Type) bool
}

func (c *Checker) Check(op hir.Operand) bool {
	return c.Type(op.Type) && c.Class(op.Class)
}

var basicOrProc_oper = Checker{
	Class: hirc.IsOperable,
	Type:  T.IsBasicOrProc,
}

var basicOrProc_res = Checker{
	Class: hirc.IsResult,
	Type:  T.IsBasicOrProc,
}

var basic_oper = Checker{
	Class: hirc.IsOperable,
	Type:  T.IsBasic,
}

var basic_res = Checker{
	Class: hirc.IsResult,
	Type:  T.IsBasic,
}

var num_oper = Checker{
	Class: hirc.IsOperable,
	Type:  T.IsNumber,
}

var num_res = Checker{
	Class: hirc.IsResult,
	Type:  T.IsNumber,
}

var bool_oper = Checker{
	Class: hirc.IsOperable,
	Type:  T.IsBool,
}

var bool_res = Checker{
	Class: hirc.IsResult,
	Type:  T.IsBool,
}

var ptr_oper = Checker{
	Class: hirc.IsOperable,
	Type:  T.IsPtr,
}

var ptr_res = Checker{
	Class: hirc.IsResult,
	Type:  T.IsPtr,
}

func checkInstr(s *state, instr hir.Instr) *Error {
	switch instr.T {
	case IT.Add, IT.Sub, IT.Div, IT.Mult, IT.Rem:
		return checkArith(instr)
	case IT.Eq, IT.Diff, IT.Less, IT.More, IT.LessEq, IT.MoreEq:
		return checkComp(instr)
	case IT.Or, IT.And, IT.Xor:
		return checkLogical(instr)
	case IT.Not:
		return checkNot(instr)
	case IT.ShiftRight, IT.ShiftLeft:
		return checkShift(instr)
	case IT.Neg:
		return checkUnaryArith(instr)
	case IT.Convert:
		return checkConvert(instr)
	case IT.LoadPtr:
		return checkLoadPtr(instr)
	case IT.StorePtr:
		return checkStorePtr(instr)
	case IT.Copy:
		return checkCopy(instr)
	case IT.Call:
		return checkCall(s, instr)
	}
	panic("sumthin' went wong")
}

func checkArith(instr hir.Instr) *Error {
	err := checkForm(instr, 2, true)
	if err != nil {
		return err
	}
	a := instr.Operands[0]
	b := instr.Operands[1]
	dest := instr.Destination[0]
	err = checkEqual(instr, instr.Type, a.Type, b.Type, dest.Type)
	if err != nil {
		return err
	}
	return checkBinary(instr, num_oper, num_oper, num_res)
}

func checkComp(instr hir.Instr) *Error {
	err := checkForm(instr, 2, true)
	if err != nil {
		return err
	}
	a := instr.Operands[0]
	b := instr.Operands[1]
	err = checkEqual(instr, instr.Type, a.Type, b.Type)
	if err != nil {
		return err
	}
	return checkBinary(instr, basic_oper, basic_oper, bool_res)
}

func checkLogical(instr hir.Instr) *Error {
	err := checkForm(instr, 2, true)
	if err != nil {
		return err
	}
	a := instr.Operands[0]
	b := instr.Operands[1]
	dest := instr.Destination[0]
	err = checkEqual(instr, instr.Type, a.Type, b.Type, dest.Type)
	if err != nil {
		return err
	}
	return checkBinary(instr, basic_oper, basic_oper, basic_res)
}

func checkShift(instr hir.Instr) *Error {
	err := checkForm(instr, 2, true)
	if err != nil {
		return err
	}
	a := instr.Operands[0]
	b := instr.Operands[1]
	dest := instr.Destination[0]
	err = checkEqual(instr, instr.Type, a.Type, b.Type, dest.Type)
	if err != nil {
		return err
	}
	return checkBinary(instr, num_oper, num_oper, num_res)
}

func checkUnaryArith(instr hir.Instr) *Error {
	err := checkForm(instr, 1, true)
	if err != nil {
		return err
	}
	a := instr.Operands[0]
	dest := instr.Destination[0]
	err = checkEqual(instr, instr.Type, a.Type, dest.Type)
	if err != nil {
		return err
	}
	return checkUnary(instr, num_oper, num_res)
}

func checkNot(instr hir.Instr) *Error {
	err := checkForm(instr, 1, true)
	if err != nil {
		return err
	}
	a := instr.Operands[0]
	dest := instr.Destination[0]
	err = checkEqual(instr, instr.Type, a.Type, dest.Type)
	if err != nil {
		return err
	}
	return checkUnary(instr, basic_oper, basic_res)
}

func checkConvert(instr hir.Instr) *Error {
	err := checkForm(instr, 1, true)
	if err != nil {
		return err
	}
	dest := instr.Destination[0]
	err = checkEqual(instr, instr.Type, dest.Type)
	if err != nil {
		return err
	}
	return checkUnary(instr, basic_oper, basic_res)
}

func checkCopy(instr hir.Instr) *Error {
	err := checkForm(instr, 1, true)
	if err != nil {
		return err
	}
	a := instr.Operands[0]
	dest := instr.Destination[0]
	err = checkEqual(instr, instr.Type, a.Type, dest.Type)
	if err != nil {
		return err
	}
	err = checkUnary(instr, basicOrProc_oper, basicOrProc_res)
	if err != nil {
		return err
	}

	return nil
}

func checkLoadPtr(instr hir.Instr) *Error {
	err := checkForm(instr, 1, true)
	if err != nil {
		return err
	}
	dest := instr.Destination[0]
	err = checkEqual(instr, instr.Type, dest.Type)
	return checkUnary(instr, ptr_oper, basicOrProc_res)
}

func checkStorePtr(instr hir.Instr) *Error {
	err := checkForm(instr, 2, false)
	if err != nil {
		return err
	}
	a := instr.Operands[0]
	dest := instr.Operands[1]
	err = checkEqual(instr, instr.Type, a.Type)
	if err != nil {
		return err
	}
	if basicOrProc_oper.Check(a) && ptr_oper.Check(dest) {
		return nil
	}
	return malformedTypeOrClass(instr)
}

func checkCall(s *state, instr hir.Instr) *Error {
	if len(instr.Operands) == 0 {
		return malformedInstr(instr)
	}
	procOp := instr.Operands[0]
	if !T.IsProc(procOp.Type) {
		return expectedProc(instr, procOp)
	}
	proc := procOp.Type.Proc

	if len(instr.Operands)-1 != len(proc.Args) {
		return procInvalidNumOfArgs(instr, proc)
	}
	if len(instr.Destination) != len(proc.Rets) {
		return procInvalidNumOfRets(instr, proc)
	}

	realArgs := instr.Operands[1:]

	for i, formal_arg := range proc.Args {
		real_arg := realArgs[i]
		if !formal_arg.Equals(real_arg.Type) {
			return procBadArg(instr, formal_arg, real_arg)
		}
	}

	for i, formal_ret := range proc.Rets {
		real_ret := instr.Destination[i]
		if !formal_ret.Equals(real_ret.Type) {
			return procBadRet(instr, formal_ret, real_ret)
		}
	}
	return nil
}

func checkEqual(instr hir.Instr, types ...*T.Type) *Error {
	if len(types) == 0 {
		return nil
	}
	first := types[0]
	for _, t := range types[1:] {
		if !first.Equals(t) {
			return malformedEqualTypes(instr)
		}
	}
	return nil
}

func checkBinary(instr hir.Instr, checkA, checkB, checkC Checker) *Error {
	a := instr.Operands[0]
	b := instr.Operands[1]
	dest := instr.Destination[0]

	if checkA.Check(a) &&
		checkB.Check(b) &&
		checkC.Check(dest) {
		return nil
	}
	return malformedTypeOrClass(instr)
}

func checkUnary(instr hir.Instr, checkA, checkC Checker) *Error {
	a := instr.Operands[0]
	dest := instr.Destination[0]

	if checkA.Check(a) &&
		checkC.Check(dest) {
		return nil
	}
	return malformedTypeOrClass(instr)
}

func checkForm(instr hir.Instr, numOperands int, hasDest bool) *Error {
	if len(instr.Operands) != numOperands {
		return malformedInstr(instr)
	}
	for _, op := range instr.Operands {
		if op.Class == hirc.InvalidClass {
			return malformedInstr(instr)
		}
	}
	if hasDest && instr.Destination == nil {
		return malformedInstr(instr)
	}
	return nil
}

func malformedInstr(instr hir.Instr) *Error {
	return eu.NewInternalSemanticError("malformed instruction: " + instr.String())
}
func malformedEqualTypes(instr hir.Instr) *Error {
	return eu.NewInternalSemanticError("unequal types: " + instr.String())
}
func malformedTypeOrClass(instr hir.Instr) *Error {
	return eu.NewInternalSemanticError("malformed type or class: " + instr.String())
}
func procArgNotFound(instr hir.Instr, p *hir.Procedure) *Error {
	return eu.NewInternalSemanticError("argument " + p.Label + " not found in: " + instr.String())
}
func procInvalidNumOfArgs(instr hir.Instr, p *T.ProcType) *Error {
	n := strconv.Itoa(len(p.Args))
	beepBop := strconv.Itoa(len(instr.Operands) - 1)
	return eu.NewInternalSemanticError("expected " + n + " arguments, instead found: " + beepBop)
}
func procInvalidNumOfRets(instr hir.Instr, p *T.ProcType) *Error {
	n := strconv.Itoa(len(p.Rets))
	beepBop := strconv.Itoa(len(instr.Destination))
	return eu.NewInternalSemanticError("expected " + n + " returns, instead found: " + beepBop)
}
func procBadArg(instr hir.Instr, d *T.Type, op hir.Operand) *Error {
	return eu.NewInternalSemanticError("argument " + op.String() + " doesn't match formal parameter (" + d.String() + ") in: " + instr.String())
}
func procBadRet(instr hir.Instr, d *T.Type, op hir.Operand) *Error {
	return eu.NewInternalSemanticError("return " + op.String() + " doesn't match formal return " + d.String() + " in: " + instr.String())
}
func invalidMirInstr(i hir.Instr) *Error {
	return eu.NewInternalSemanticError("invalid MIR Instr: " + i.String())
}
func invalidFlow(f hir.Flow) *Error {
	return eu.NewInternalSemanticError("invalid flow: " + f.String())
}
func expectedProc(instr hir.Instr, o hir.Operand) *Error {
	return eu.NewInternalSemanticError("expected procedure in: " + instr.String() + ", instead found: " + o.String())
}
