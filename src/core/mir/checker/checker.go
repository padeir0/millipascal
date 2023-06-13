package checker

import (
	. "mpc/core"
	"mpc/core/mir"
	mirc "mpc/core/mir/class"
	FT "mpc/core/mir/flowkind"
	IT "mpc/core/mir/instrkind"
	T "mpc/core/types"
	eu "mpc/core/util"

	"strconv"
	"strings"
)

func Check(P *mir.Program) *Error {
	for _, sy := range P.Symbols {
		if sy.Proc != nil && !sy.Builtin {
			s := newState(P)
			s.proc = sy.Proc
			s.proc.ResetBlocks()
			s.Init()
			err := checkCode(s, sy.Proc.FirstBlock())
			if err != nil {
				return err
			}
		}
	}
	return nil
}

type region []mir.OptOperand

func newRegion(size int64) region {
	return make(region, size)
}

func (r *region) String() string {
	output := []string{}
	for _, op := range *r {
		output = append(output, op.String())
	}
	return strings.Join(output, ", ")
}

func (r *region) Store(i int64, op mir.Operand) {
	if i >= int64(len(*r)) {
		*r = append(*r, newRegion(i-int64(len(*r)+1))...)
	}
	(*r)[i] = mir.OptOperand_(op)
}

func (r *region) Load(i int64) (mir.Operand, bool) {
	if i >= int64(len(*r)) {
		return mir.Operand{}, false
	}
	op := (*r)[i]
	return op.Operand, op.Valid
}

func (r region) Clear(i int) {
	if i >= len(r) {
		return
	}
	r[i].Valid = false
	r[i].Operand = mir.Operand{}
}

type state struct {
	Program *mir.Program
	proc    *mir.Procedure
	bb      *mir.BasicBlock

	CalleeInterproc region
	CallerInterproc region
	Spill           region
	Registers       region
	Locals          region
}

func newState(M *mir.Program) *state {
	return &state{
		Program:         M,
		CalleeInterproc: newRegion(8),
		CallerInterproc: newRegion(8),
		Spill:           newRegion(8),
		Registers:       newRegion(8),
		Locals:          newRegion(8),
	}
}

func (s *state) Init() {
	for i, arg := range s.proc.Args {
		argOp := newCallerOperand(arg, int64(i))
		s.CallerInterproc.Store(int64(i), argOp)
	}
	for i, loc := range s.proc.Vars {
		locOp := newCallerOperand(loc, int64(i))
		s.Locals.Store(int64(i), locOp)
	}
}

func (s *state) String() string {
	return s.proc.Label + s.bb.Label + "\n" +
		"callee: " + s.CalleeInterproc.String() + "\n" +
		"caller: " + s.CallerInterproc.String() + "\n" +
		"spill: " + s.Spill.String() + "\n" +
		"registers: " + s.Registers.String() + "\n" +
		"locals: " + s.Locals.String() + "\n"
}

func (s *state) Copy() *state {
	caller := make(region, len(s.CallerInterproc))
	callee := make(region, len(s.CalleeInterproc))
	spill := make(region, len(s.Spill))
	registers := make(region, len(s.Registers))
	locals := make(region, len(s.Locals))
	copy(caller, s.CallerInterproc)
	copy(callee, s.CalleeInterproc)
	copy(spill, s.Spill)
	copy(registers, s.Registers)
	copy(locals, s.Locals)
	return &state{
		CallerInterproc: caller,
		CalleeInterproc: callee,
		Spill:           spill,
		Registers:       registers,
		Locals:          locals,
		bb:              s.bb,
		Program:         s.Program,
		proc:            s.proc,
	}
}

func (s *state) SetReg(op mir.Operand) {
	if op.Class != mirc.Register {
		panic("is not setting a register: " + op.String())
	}
	s.Registers.Store(op.Num, op)
}

func newCallerOperand(t *T.Type, i int64) mir.Operand {
	return mir.Operand{
		Class: mirc.CallerInterproc,
		Num:   i,
		Type:  t,
	}
}

func newLocalOperand(t *T.Type, i int64) mir.Operand {
	return mir.Operand{
		Class: mirc.Local,
		Num:   i,
		Type:  t,
	}
}

func checkCode(s *state, bb *mir.BasicBlock) *Error {
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
		s2 := s.Copy()
		t := s.proc.GetBlock(bb.Out.True)
		err := checkCode(s, t)
		if err != nil {
			return err
		}
		f := s.proc.GetBlock(bb.Out.False)
		return checkCode(s2, f)
	case FT.Return:
		return checkRet(s)
	}
	return nil
}

func checkRet(s *state) *Error {
	for i, ret := range s.proc.Rets {
		op, ok := s.CallerInterproc.Load(int64(i))
		if !ok {
			return eu.NewInternalSemanticError("return stack is empty, expected returns: " + s.proc.StrRets())
		}
		if !ret.Equals(op.Type) {
			return eu.NewInternalSemanticError("return of type " + ret.String() + " doesn't match value in stack: " + s.CallerInterproc.String())
		}
		s.CallerInterproc.Clear(i)
	}
	return nil
}

type Checker struct {
	Class func(mirc.Class) bool
	Type  func(*T.Type) bool
}

func (c *Checker) Check(op mir.OptOperand) bool {
	if !op.Valid {
		panic("invalid operand")
	}
	return c.Type(op.Type) && c.Class(op.Class)
}

var proc_imme = Checker{
	Class: mirc.IsImmediate,
	Type:  T.IsProc,
}

var basicOrProc_imme = Checker{
	Class: mirc.IsImmediate,
	Type:  T.IsBasicOrProc,
}

var basicOrProc_reg = Checker{
	Class: mirc.IsRegister,
	Type:  T.IsBasicOrProc,
}

var basicOrProc_addr = Checker{
	Class: mirc.IsAddressable,
	Type:  T.IsBasicOrProc,
}

var basic_imme = Checker{
	Class: mirc.IsImmediate,
	Type:  T.IsBasic,
}

var basic_reg = Checker{
	Class: mirc.IsRegister,
	Type:  T.IsBasic,
}

var basic_addr = Checker{
	Class: mirc.IsAddressable,
	Type:  T.IsBasic,
}

var num_imme = Checker{
	Class: mirc.IsImmediate,
	Type:  T.IsNumber,
}

var num_reg = Checker{
	Class: mirc.IsRegister,
	Type:  T.IsNumber,
}

var bool_imme = Checker{
	Class: mirc.IsImmediate,
	Type:  T.IsBool,
}

var bool_reg = Checker{
	Class: mirc.IsRegister,
	Type:  T.IsBool,
}

var ptr_imme = Checker{
	Class: mirc.IsImmediate,
	Type:  T.IsPtr,
}

var ptr_reg = Checker{
	Class: mirc.IsRegister,
	Type:  T.IsPtr,
}

func checkInstr(s *state, instr mir.Instr) *Error {
	err := checkInvalidClass(instr)
	if err != nil {
		return err
	}
	switch instr.T {
	case IT.Add, IT.Sub, IT.Div, IT.Mult, IT.Rem:
		return checkArith(s, instr)
	case IT.Eq, IT.Diff, IT.Less, IT.More, IT.LessEq, IT.MoreEq:
		return checkComp(s, instr)
	case IT.Or, IT.And:
		return checkLogical(s, instr)
	case IT.Not:
		return checkNot(s, instr)
	case IT.Neg:
		return checkUnaryArith(s, instr)
	case IT.Convert:
		return checkConvert(s, instr)
	case IT.LoadPtr:
		return checkLoadPtr(s, instr)
	case IT.StorePtr:
		return checkStorePtr(s, instr)
	case IT.Store:
		return checkStore(s, instr)
	case IT.Load:
		return checkLoad(s, instr)
	case IT.Copy:
		return checkCopy(s, instr)
	case IT.Call:
		return checkCall(s, instr)
	}
	panic("sumthin' went wong")
}

func checkArith(s *state, instr mir.Instr) *Error {
	err := checkForm(instr, true, true, true)
	if err != nil {
		return err
	}

	err = checkRegs(s, instr)
	if err != nil {
		return err
	}
	s.SetReg(instr.Dest.Operand)

	err = checkEqual(instr, instr.Type, instr.A.Type, instr.B.Type, instr.Dest.Type)
	if err != nil {
		return err
	}
	return checkBinary(instr, num_imme, num_imme, num_reg)
}

func checkComp(s *state, instr mir.Instr) *Error {
	err := checkForm(instr, true, true, true)
	if err != nil {
		return err
	}

	err = checkRegs(s, instr)
	if err != nil {
		return err
	}
	s.SetReg(instr.Dest.Operand)

	err = checkEqual(instr, instr.Type, instr.A.Type, instr.B.Type)
	if err != nil {
		return err
	}
	return checkBinary(instr, basic_imme, basic_imme, bool_reg)
}

func checkLogical(s *state, instr mir.Instr) *Error {
	err := checkForm(instr, true, true, true)
	if err != nil {
		return err
	}

	err = checkRegs(s, instr)
	if err != nil {
		return err
	}
	s.SetReg(instr.Dest.Operand)

	err = checkEqual(instr, instr.Type, instr.A.Type, instr.B.Type, instr.Dest.Type)
	if err != nil {
		return err
	}
	return checkBinary(instr, bool_imme, bool_imme, bool_reg)
}

func checkUnaryArith(s *state, instr mir.Instr) *Error {
	err := checkForm(instr, true, false, true)
	if err != nil {
		return err
	}

	err = checkRegs(s, instr)
	if err != nil {
		return err
	}
	s.SetReg(instr.Dest.Operand)

	err = checkEqual(instr, instr.Type, instr.A.Type, instr.Dest.Type)
	if err != nil {
		return err
	}
	return checkUnary(instr, num_imme, num_reg)
}

func checkNot(s *state, instr mir.Instr) *Error {
	err := checkForm(instr, true, false, true)
	if err != nil {
		return err
	}

	err = checkRegs(s, instr)
	if err != nil {
		return err
	}
	s.SetReg(instr.Dest.Operand)

	err = checkEqual(instr, instr.Type, instr.A.Type, instr.Dest.Type)
	if err != nil {
		return err
	}
	return checkUnary(instr, bool_imme, bool_reg)
}

func checkConvert(s *state, instr mir.Instr) *Error {
	err := checkForm(instr, true, false, true)
	if err != nil {
		return err
	}

	err = checkRegs(s, instr)
	if err != nil {
		return err
	}
	s.SetReg(instr.Dest.Operand)

	err = checkEqual(instr, instr.Type, instr.Dest.Type)
	if err != nil {
		return err
	}
	return checkUnary(instr, basic_imme, basic_reg)
}

func checkLoadPtr(s *state, instr mir.Instr) *Error {
	err := checkForm(instr, true, false, true)
	if err != nil {
		return err
	}

	err = checkRegs(s, instr)
	if err != nil {
		return err
	}
	s.SetReg(instr.Dest.Operand)

	err = checkEqual(instr, instr.Type, instr.Dest.Type)
	if err != nil {
		return err
	}
	return checkUnary(instr, ptr_imme, basicOrProc_reg)
}

func checkStorePtr(s *state, instr mir.Instr) *Error {
	err := checkForm(instr, true, true, false)
	if err != nil {
		return err
	}

	err = checkRegs(s, instr)
	if err != nil {
		return err
	}

	err = checkEqual(instr, instr.Type, instr.A.Type)
	if err != nil {
		return err
	}
	if basicOrProc_imme.Check(instr.A) &&
		ptr_imme.Check(instr.B) {
		return nil
	}
	return malformedTypeOrClass(instr)
}

func checkLoad(s *state, instr mir.Instr) *Error {
	err := checkForm(instr, true, false, true)
	if err != nil {
		return err
	}

	s.SetReg(instr.Dest.Operand)

	err = checkEqual(instr, instr.Type, instr.A.Type, instr.Dest.Type)
	if err != nil {
		return err
	}
	err = checkUnary(instr, basicOrProc_addr, basicOrProc_reg)
	if err != nil {
		return err
	}

	return checkLoadState(s, instr)
}

func checkStore(s *state, instr mir.Instr) *Error {
	err := checkForm(instr, true, false, true)
	if err != nil {
		return err
	}

	err = checkRegs(s, instr)
	if err != nil {
		return err
	}

	err = checkEqual(instr, instr.Type, instr.A.Type, instr.Dest.Type)
	if err != nil {
		return err
	}
	err = checkUnary(instr, basicOrProc_imme, basicOrProc_addr)
	if err != nil {
		return err
	}

	return checkStoreState(s, instr)
}

func checkCopy(s *state, instr mir.Instr) *Error {
	err := checkForm(instr, true, false, true)
	if err != nil {
		return err
	}

	err = checkRegs(s, instr)
	if err != nil {
		return err
	}
	s.SetReg(instr.Dest.Operand)

	err = checkEqual(instr, instr.Type, instr.A.Type, instr.Dest.Type)
	if err != nil {
		return err
	}
	err = checkUnary(instr, basicOrProc_imme, basicOrProc_reg)
	if err != nil {
		return err
	}

	return nil
}

func checkCall(s *state, instr mir.Instr) *Error {
	err := checkForm(instr, true, false, false)
	if err != nil {
		return err
	}
	if !proc_imme.Check(instr.A) {
		return malformedTypeOrClass(instr)
	}

	t := instr.A.Type

	for i, formal_arg := range t.Proc.Args {
		real_arg, ok := s.CalleeInterproc.Load(int64(i))
		if !ok {
			return errorCallLoadingGarbage(instr)
		}
		if !formal_arg.Equals(real_arg.Type) {
			return procBadArg(instr, formal_arg, real_arg)
		}
		s.CalleeInterproc.Clear(i)
	}

	for i, formal_ret := range t.Proc.Rets {
		op := mir.Operand{Class: mirc.CalleeInterproc, Num: int64(i), Type: formal_ret}
		s.CalleeInterproc.Store(int64(i), op)
	}
	return nil
}

func checkLoadState(s *state, instr mir.Instr) *Error {
	var source mir.Operand
	var ok bool
	switch instr.A.Class {
	case mirc.Spill:
		source, ok = s.Spill.Load(instr.A.Num)
	case mirc.CalleeInterproc:
		source, ok = s.CalleeInterproc.Load(instr.A.Num)
	case mirc.CallerInterproc:
		source, ok = s.CallerInterproc.Load(instr.A.Num)
	case mirc.Local:
		source, ok = s.Locals.Load(instr.A.Num)
	default:
		panic("oh no")
	}
	if !ok {
		return errorLoadingGarbage(instr)
	}
	err := checkEqual(instr, instr.Dest.Type, source.Type)
	if err != nil {
		return err
	}
	return nil
}

func checkStoreState(s *state, instr mir.Instr) *Error {
	switch instr.Dest.Class {
	case mirc.Spill:
		s.Spill.Store(instr.Dest.Num, instr.A.Operand)
	case mirc.CalleeInterproc:
		s.CalleeInterproc.Store(instr.Dest.Num, instr.A.Operand)
	case mirc.CallerInterproc:
		s.CallerInterproc.Store(instr.Dest.Num, instr.A.Operand)
	case mirc.Local:
		s.Locals.Store(instr.Dest.Num, instr.A.Operand)
	default:
		panic("oh no")
	}
	return nil
}

func checkRegs(s *state, instr mir.Instr) *Error {
	err := checkRegOperand(s, instr, instr.A)
	if err != nil {
		return err
	}
	err = checkRegOperand(s, instr, instr.B)
	if err != nil {
		return err
	}
	return nil
}

func checkRegOperand(s *state, instr mir.Instr, op mir.OptOperand) *Error {
	if !op.Valid {
		return nil
	}
	if op.Class == mirc.Register {
		loaded, ok := s.Registers.Load(op.Num)
		if !ok {
			return errorUsingRegisterGarbage(instr, op.Operand)
		}
		if loaded.Num != op.Num || loaded.Class != op.Class || loaded.Type != op.Type {
			return errorIncorrectValueInRegister(instr, loaded, op.Operand)
		}
	}
	return nil
}

func checkEqual(instr mir.Instr, types ...*T.Type) *Error {
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

func checkBinary(instr mir.Instr, checkA, checkB, checkC Checker) *Error {
	if checkA.Check(instr.A) &&
		checkB.Check(instr.B) &&
		checkC.Check(instr.Dest) {
		return nil
	}
	return malformedTypeOrClass(instr)
}

func checkUnary(instr mir.Instr, checkA, checkC Checker) *Error {
	if checkA.Check(instr.A) &&
		checkC.Check(instr.Dest) {
		return nil
	}
	return malformedTypeOrClass(instr)
}

func checkInvalidClass(instr mir.Instr) *Error {
	if instr.A.Valid && instr.A.Class == mirc.InvalidMIRClass {
		return invalidClass(instr)
	}
	if instr.B.Valid && instr.B.Class == mirc.InvalidMIRClass {
		return invalidClass(instr)
	}
	if instr.Dest.Valid && instr.Dest.Class == mirc.InvalidMIRClass {
		return invalidClass(instr)
	}
	return nil
}

func checkForm(instr mir.Instr, hasA, hasB, hasDest bool) *Error {
	if hasA && !instr.A.Valid {
		return malformedInstr(instr)
	}
	if hasB && !instr.B.Valid {
		return malformedInstr(instr)
	}
	if hasDest && !instr.Dest.Valid {
		return malformedInstr(instr)
	}
	return nil
}

func malformedInstr(instr mir.Instr) *Error {
	return eu.NewInternalSemanticError("malformed instruction: " + instr.String())
}
func malformedEqualTypes(instr mir.Instr) *Error {
	return eu.NewInternalSemanticError("unequal types: " + instr.String())
}
func malformedTypeOrClass(instr mir.Instr) *Error {
	return eu.NewInternalSemanticError("malformed type or class: " + instr.String())
}
func invalidClass(instr mir.Instr) *Error {
	return eu.NewInternalSemanticError("invalid class: " + instr.String())
}
func errorLoadingGarbage(instr mir.Instr) *Error {
	return eu.NewInternalSemanticError("loading garbage: " + instr.String())
}
func errorCallLoadingGarbage(instr mir.Instr) *Error {
	return eu.NewInternalSemanticError("call loading garbage: " + instr.String())
}
func errorUsingRegisterGarbage(instr mir.Instr, op mir.Operand) *Error {
	return eu.NewInternalSemanticError("using register garbage: " + op.String() + " of " + instr.String())
}
func errorIncorrectValueInRegister(instr mir.Instr, o, op mir.Operand) *Error {
	return eu.NewInternalSemanticError("incorrect value in register (" + o.String() + "): " + op.String() + " of " + instr.String())
}
func errorLoadingIncorrectType(instr mir.Instr) *Error {
	return eu.NewInternalSemanticError("load of incorrect type: " + instr.String())
}
func procArgNotFound(instr mir.Instr, p *mir.Procedure) *Error {
	return eu.NewInternalSemanticError("argument " + p.Label + " not found in: " + instr.String())
}
func procBadArg(instr mir.Instr, d *T.Type, op mir.Operand) *Error {
	return eu.NewInternalSemanticError("argument " + op.String() + " doesn't match formal parameter (" + d.String() + ") in: " + instr.String())
}
func procBadRet(instr mir.Instr, d T.Type, op mir.Operand) *Error {
	return eu.NewInternalSemanticError("return " + op.String() + " doesn't match formal return " + d.String() + " in: " + instr.String())
}

func nilInstr(s *state) *Error {
	return eu.NewInternalSemanticError("nil instruction in: " + s.proc.Label + " " + s.bb.Label)
}
func notAProc(instr mir.Instr) *Error {
	return eu.NewInternalSemanticError("not a procedure: " + instr.String())
}
func symbolNotFound(i int64) *Error {
	return eu.NewInternalSemanticError("Symbol not found in program: " + strconv.FormatInt(i, 10))
}
