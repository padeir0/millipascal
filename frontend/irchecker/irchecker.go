package irchecker

import (
	T "mpc/frontend/enums/Type"
	FT "mpc/frontend/enums/flowType"
	IT "mpc/frontend/enums/instrType"
	ST "mpc/frontend/enums/symbolType"
	OT "mpc/frontend/enums/operandType"
	"mpc/frontend/errors"
	"mpc/frontend/ir"
	eu "mpc/frontend/util/errors"

	"strings"
	"strconv"
)

func Check(M *ir.Module, CheckClass bool) *errors.CompilerError {
	s := newState(M, CheckClass)
	for _, sy := range M.Globals {
		if sy.T == ST.Proc {
			s.proc = sy.Proc
			err := checkCode(s, sy.Proc.Code)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

type Region []*ir.Operand

func newRegion(size int) Region {
	return make(Region, size)
}

func (r Region) String() string {
	output := []string{}
	for _, op := range r {
		output = append(output, op.String())
	}
	return strings.Join(output, ", ")
}

func (r Region) Store(i int, op *ir.Operand) {
	if i >= len(r) {
		r = append(r, newRegion(i - len(r) + 1)...)
	}
	r[i] = op
}

func (r Region) Load(i int) *ir.Operand {
	if i >= len(r) {
		return nil
	}
	return r[i]
}

func (r Region) Clear(i int) {
	if i >= len(r) {
		return
	}
	r[i] = nil
}

type state struct {
	m    *ir.Module
	proc *ir.Proc
	bb   *ir.BasicBlock

	InterProc Region
	Variables Region
	Spill     Region

	CheckClass bool
}

func newState(M *ir.Module, checkClass bool) *state {
	return &state{
		m:         M,
		InterProc: newRegion(8),
		Variables: newRegion(8),
		Spill:     newRegion(8),
		CheckClass: checkClass,
	}
}

func checkCode(s *state, bb *ir.BasicBlock) *errors.CompilerError {
	if bb.Checked {
		return nil
	}
	s.bb = bb
	for _, instr := range bb.Code {
		err := checkInstr(s, instr)
		if err != nil {
			return err
		}
	}
	bb.Checked = true
	return checkJump(s)
}

func checkJump(s *state) *errors.CompilerError {
	bb := s.bb
	switch bb.Out.T {
	case FT.Jmp:
		return checkCode(s, bb.Out.True)
	case FT.If:
		err := checkCode(s, bb.Out.True)
		if err != nil {
			return err
		}
		return checkCode(s, bb.Out.False)
	case FT.Return:
		return checkRet(s)
	}
	return nil
}

func checkRet(s *state) *errors.CompilerError {
	if !s.CheckClass {
		return nil
	}
	for i, ret := range s.proc.Rets {
		op := s.InterProc.Load(i)
		if op == nil {
			return eu.NewInternalSemanticError("return stack is empty, expected returns: " + s.proc.Returns())
		}
		if op.Type != ret {
			return eu.NewInternalSemanticError("return of type " + ret.String() + " doesn't match value in stack: " + s.InterProc.String())
		}
		s.InterProc.Clear(i)
	}
	return nil
}

type Checker struct {
	Class func(OT.OperandType)bool
	Type  func(T.Type)bool
}

func (c *Checker) Check(op *ir.Operand, checkClass bool) bool {
	if checkClass {
		return c.Type(op.Type) && c.Class(op.T)
	}
	return c.Type(op.Type)
}

var any_imme = Checker {
	Class: OT.IsImmediate,
	Type: T.IsAny,
}

var any_reg = Checker {
	Class: OT.IsRegister,
	Type: T.IsAny,
}

var any_addr = Checker {
	Class: OT.IsAddressable,
	Type: T.IsAny,
}

var num_imme = Checker {
	Class: OT.IsImmediate,
	Type: T.IsNumber,
}

var num_reg = Checker {
	Class: OT.IsRegister,
	Type: T.IsNumber,
}

var bool_imme = Checker {
	Class: OT.IsImmediate,
	Type: T.IsBool,
}

var bool_reg = Checker {
	Class: OT.IsRegister,
	Type: T.IsBool,
}

var nonPtr_imme = Checker {
	Class: OT.IsImmediate,
	Type: T.IsNonPtr,
}

var nonPtr_reg = Checker {
	Class: OT.IsRegister,
	Type: T.IsNonPtr,
}

var ptr_imme = Checker {
	Class: OT.IsImmediate,
	Type: T.IsPtr,
}

var ptr_reg = Checker {
	Class: OT.IsRegister,
	Type: T.IsPtr,
}

func checkInstr(s *state, instr *ir.Instr) *errors.CompilerError {
	switch instr.T {
	case IT.Add, IT.Sub, IT.Div, IT.Mult, IT.Rem:
		return checkArith(instr, s.CheckClass)
	case IT.Eq, IT.Diff, IT.Less, IT.More, IT.LessEq, IT.MoreEq:
		return checkComp(instr, s.CheckClass)
	case IT.Or, IT.And:
		return checkLogical(instr, s.CheckClass)
	case IT.Not:
		return checkNot(instr, s.CheckClass)
	case IT.UnaryMinus, IT.UnaryPlus:
		return checkUnaryArith(instr, s.CheckClass)
	case IT.Convert:
		return checkConvert(instr, s.CheckClass)
	case IT.Offset:
		return checkOffset(instr, s.CheckClass)
	case IT.LoadPtr:
		return checkLoadPtr(instr, s.CheckClass)
	case IT.StorePtr:
		return checkStorePtr(instr, s.CheckClass)
	case IT.Store:
		return checkStore(s, instr)
	case IT.Load:
		return checkLoad(s, instr)
	case IT.Call:
		if s.CheckClass {
			return checkLIRCall(s, instr)
		}
		return checkCall(s, instr)
	}
	panic("sumthin' went wong")
}

func checkArith(instr *ir.Instr, checkClass bool) *errors.CompilerError {
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
	return checkBinary(instr, num_imme, num_imme, num_reg, checkClass)
}

func checkComp(instr *ir.Instr, checkClass bool) *errors.CompilerError {
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
	return checkBinary(instr, any_imme, any_imme, bool_reg, checkClass)
}

func checkLogical(instr *ir.Instr, checkClass bool) *errors.CompilerError {
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
	return checkBinary(instr, bool_imme, bool_imme, bool_reg, checkClass)
}

func checkUnaryArith(instr *ir.Instr, checkClass bool) *errors.CompilerError {
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
	return checkUnary(instr, num_imme, num_reg, checkClass)
}

func checkNot(instr *ir.Instr, checkClass bool) *errors.CompilerError {
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
	return checkUnary(instr, bool_imme, bool_reg, checkClass)
}

func checkConvert(instr *ir.Instr, checkClass bool) *errors.CompilerError {
	err := checkForm(instr, 1, true)
	if err != nil {
		return err
	}
	dest := instr.Destination[0]
	err = checkEqual(instr, instr.Type, dest.Type) 
	if err != nil {
		return err
	}
	return checkUnary(instr, nonPtr_imme, nonPtr_reg, checkClass)
}

func checkOffset(instr *ir.Instr, checkClass bool) *errors.CompilerError {
	err := checkForm(instr, 2, true)
	if err != nil {
		return err
	}
	b := instr.Operands[1]
	err = checkEqual(instr, instr.Type, b.Type)
	if err != nil {
		return err
	}
	return checkBinary(instr, ptr_imme, num_imme, ptr_reg, checkClass)
}

func checkLoadPtr(instr *ir.Instr, checkClass bool) *errors.CompilerError {
	err := checkForm(instr, 1, true)
	if err != nil {
		return err
	}
	dest := instr.Destination[0]
	err = checkEqual(instr, instr.Type, dest.Type) 
	if err != nil {
		return err
	}
	return checkUnary(instr, ptr_imme, any_reg, checkClass)
}

func checkStorePtr(instr *ir.Instr, checkClass bool) *errors.CompilerError {
	err := checkForm(instr, 1, true)
	if err != nil {
		return err
	}
	a := instr.Operands[0]
	err = checkEqual(instr, instr.Type, a.Type) 
	if err != nil {
		return err
	}
	return checkUnary(instr, any_reg, ptr_imme, checkClass)
}

func checkLoad(s *state, instr *ir.Instr) *errors.CompilerError {
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
	err = checkUnary(instr, any_addr, any_reg, s.CheckClass)
	if err != nil {
		return err
	}

	if s.CheckClass {
		return checkLoadState(s, instr)
	}

	return nil
}

func checkStore(s *state, instr *ir.Instr) *errors.CompilerError {
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
	err = checkUnary(instr, any_imme, any_addr, s.CheckClass)
	if err != nil {
		return err
	}

	if s.CheckClass {
		return checkStoreState(s, instr)
	}

	return nil
}

func checkCall(s *state, instr *ir.Instr) *errors.CompilerError {
	if len(instr.Operands) == 0 {
		return malformedInstr(instr)
	}
	procOp := instr.Operands[0]
	sy, ok := s.m.Globals[procOp.Label]
	if !ok {
		return procNotFound(instr)
	}
	proc := sy.Proc

	if len(instr.Operands) - 1 != len(proc.Args) {
		return procInvalidNumOfArgs(instr, proc)
	}
	if len(instr.Destination) != len(proc.Rets) {
		return procInvalidNumOfRets(instr, proc)
	}

	realArgs := instr.Operands[1:]

	for i, formal_arg := range proc.Args {
		real_arg := realArgs[i]
		if formal_arg.Type != real_arg.Type {
			return procBadArg(instr, formal_arg, real_arg)
		}
	}

	for i, formal_ret := range proc.Rets {
		real_ret := instr.Destination[i]
		if real_ret.Type != formal_ret {
			return procBadRet(instr, formal_ret, real_ret)
		}
	}
	return nil
}

func checkLIRCall(s *state, instr *ir.Instr) *errors.CompilerError {
	return nil
}

func checkLoadState(s *state, instr *ir.Instr) *errors.CompilerError {
	loadOp := instr.Operands[0]
	dest := instr.Destination[0]
	var source *ir.Operand
	switch loadOp.T {
	case OT.Spill:
		source = s.Spill.Load(loadOp.Num)
	case OT.Interproc:
		source = s.InterProc.Load(loadOp.Num)
	case OT.Local:
		source = s.Variables.Load(loadOp.Num)
	}
	if source == nil {
		return errorLoadingGarbage(instr)
	}
	err := checkEqual(instr, dest.Type, source.Type)
	if err != nil {
		return err
	}
	return nil
}

func checkStoreState(s *state, instr *ir.Instr) *errors.CompilerError {
	source := instr.Operands[0]
	dest := instr.Destination[0]
	switch source.T {
	case OT.Spill:
		s.Spill.Store(dest.Num, source)
	case OT.Interproc:
		s.InterProc.Store(dest.Num, source)
	case OT.Local:
		s.Variables.Store(dest.Num, source)
	}
	return nil
}

func checkEqual(instr *ir.Instr, types ...T.Type) *errors.CompilerError {
	if len(types) == 0 {
		return nil
	}
	first := types[0]
	for _, t := range types[1:] {
		if first != t {
			return malformedEqualTypes(instr)
		}
	}
	return nil
}

func checkBinary(instr *ir.Instr, checkA, checkB, checkC Checker, checkClass bool) *errors.CompilerError {
	a := instr.Operands[0]
	b := instr.Operands[1]
	dest := instr.Destination[0]

	if checkA.Check(a, checkClass) &&
		checkB.Check(b, checkClass) &&
		checkC.Check(dest, checkClass){
		return nil
	}
	return malformedTypeOrClass(instr)
}

func checkUnary(instr *ir.Instr, checkA, checkC Checker, checkClass bool) *errors.CompilerError {
	a := instr.Operands[0]
	dest := instr.Destination[0]

	if checkA.Check(a, checkClass) &&
		checkC.Check(dest, checkClass) {
		return nil
	}
	return malformedTypeOrClass(instr)
}

func checkForm(instr *ir.Instr, numOperands int, hasDest bool) *errors.CompilerError {
	if len(instr.Operands) != numOperands {
		return malformedInstr(instr)
	}
	for _, op := range instr.Operands {
		if op == nil {
			return malformedInstr(instr)
		}
	}
	if hasDest && instr.Destination == nil {
		return malformedInstr(instr)
	}
	return nil
}

func malformedInstr(instr *ir.Instr) *errors.CompilerError {
	return eu.NewInternalSemanticError("malformed instruction: " + instr.String())
}
func malformedEqualTypes(instr *ir.Instr) *errors.CompilerError {
	return eu.NewInternalSemanticError("unequal types: " + instr.String())
}
func malformedTypeOrClass(instr *ir.Instr) *errors.CompilerError {
	return eu.NewInternalSemanticError("malformed type or class: " + instr.String())
}
func errorLoadingGarbage(instr *ir.Instr) *errors.CompilerError {
	return eu.NewInternalSemanticError("loading garbage: " + instr.String())
}
func errorLoadingIncorrectType(instr *ir.Instr) *errors.CompilerError {
	return eu.NewInternalSemanticError("load of incorrect type: " + instr.String())
}
func procNotFound(instr *ir.Instr) *errors.CompilerError {
	return eu.NewInternalSemanticError("procedure not found: " + instr.String())
}
func procArgNotFound(instr *ir.Instr, d *ir.Decl) *errors.CompilerError {
	return eu.NewInternalSemanticError("argument "+d.Name+" not found in: " + instr.String())
}
func procInvalidNumOfArgs(instr *ir.Instr, p *ir.Proc) *errors.CompilerError {
	n := strconv.Itoa(len(p.Args))
	beepBop := strconv.Itoa(len(instr.Operands) -1)
	return eu.NewInternalSemanticError("expected "+n+" arguments, instead found: " + beepBop)
}
func procInvalidNumOfRets(instr *ir.Instr, p *ir.Proc) *errors.CompilerError {
	n := strconv.Itoa(len(p.Rets))
	beepBop := strconv.Itoa(len(instr.Destination))
	return eu.NewInternalSemanticError("expected "+n+" returns, instead found: " + beepBop)
}
func procBadArg(instr *ir.Instr, d *ir.Decl, op *ir.Operand) *errors.CompilerError {
	return eu.NewInternalSemanticError("argument "+op.String()+" doesn't match formal parameter "+d.Name+" in: " + instr.String())
}
func procBadRet(instr *ir.Instr, d T.Type, op *ir.Operand) *errors.CompilerError {
	return eu.NewInternalSemanticError("return "+op.String()+" doesn't match formal return "+d.String()+" in: " + instr.String())
}
