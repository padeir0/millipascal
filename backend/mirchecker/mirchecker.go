package mirchecker

import (
	T "mpc/frontend/Type"
	mirc "mpc/frontend/enums/MIRClass"
	FT "mpc/frontend/enums/flowType"
	IT "mpc/frontend/enums/instrType"
	ST "mpc/frontend/enums/symbolType"
	"mpc/frontend/errors"
	"mpc/frontend/ir"
	eu "mpc/frontend/util/errors"

	"strconv"
	"strings"
)

func Check(M *ir.Module) *errors.CompilerError {
	for _, sy := range M.Globals {
		if sy.T == ST.Proc {
			s := newState(M)
			s.proc = sy.Proc
			s.proc.ResetVisited()
			s.InitArgs()
			err := checkCode(s, sy.Proc.Code)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

type region []*ir.Operand

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

func (r *region) Store(i int64, op *ir.Operand) {
	if i >= int64(len(*r)) {
		*r = append(*r, newRegion(i-int64(len(*r)+1))...)
	}
	(*r)[i] = op
}

func (r *region) Load(i int64) *ir.Operand {
	if i >= int64(len(*r)) {
		return nil
	}
	return (*r)[i]
}

func (r region) Clear(i int) {
	if i >= len(r) {
		return
	}
	r[i] = nil
}

type state struct {
	m    *ir.Module
	proc *ir.Proc
	bb   *ir.BasicBlock

	CalleeInterproc region
	CallerInterproc region
	Spill           region
	Registers	region
}

func newState(M *ir.Module) *state {
	return &state{
		m:               M,
		CalleeInterproc: newRegion(8),
		CallerInterproc: newRegion(8),
		Spill:           newRegion(8),
		Registers:       newRegion(8),
	}
}

func (s *state) InitArgs() {
	for i, arg := range s.proc.Args {
		argOp := newCallerOperand(arg, int64(i))
		s.CallerInterproc.Store(int64(i), argOp)
	}
}

func (s *state) String() string {
	return	s.proc.Name + s.bb.Label + "\n" + 
		"callee: " + s.CalleeInterproc.String() + "\n" +
		"caller: " + s.CallerInterproc.String() + "\n" +
		"spill: " + s.Spill.String() + "\n" +
		"registers: " + s.Registers.String() + "\n"
}

func (s *state) Copy() *state {
	caller := make(region, len(s.CallerInterproc))
	callee := make(region, len(s.CalleeInterproc))
	spill  := make(region, len(s.Spill))
	registers := make(region, len(s.Registers))
	copy(caller, s.CallerInterproc)
	copy(callee, s.CalleeInterproc)
	copy(spill, s.Spill)
	copy(registers, s.Registers)
	return &state{
		CallerInterproc: caller,
		CalleeInterproc: callee,
		Spill: spill,
		Registers: registers,
		bb: s.bb,
		m: s.m,
		proc: s.proc,
	}
}

func (s *state) SetReg(op *ir.Operand) {
	if op.Mirc != mirc.Register {
		panic("is not setting a register: " + op.String())
	}
	s.Registers.Store(op.Num, op)
}

func newCallerOperand(arg *ir.Symbol, i int64) *ir.Operand {
	return &ir.Operand{
		Mirc:   mirc.CallerInterproc,
		Num:    i,
		Symbol: arg,
		Type:   arg.Type,
	}
}

func newLocalOperand(arg *ir.Symbol) *ir.Operand {
	return &ir.Operand{
		Mirc:   mirc.Local,
		Num:    -1,
		Symbol: arg,
		Type:   arg.Type,
	}
}

func checkCode(s *state, bb *ir.BasicBlock) *errors.CompilerError {
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

func checkJump(s *state) *errors.CompilerError {
	bb := s.bb
	switch bb.Out.T {
	case FT.Jmp:
		return checkCode(s, bb.Out.True)
	case FT.If:
		s2 := s.Copy()
		err := checkCode(s, bb.Out.True)
		if err != nil {
			return err
		}
		return checkCode(s2, bb.Out.False)
	case FT.Return:
		return checkRet(s)
	}
	return nil
}

func checkRet(s *state) *errors.CompilerError {
	for i, ret := range s.proc.Rets {
		op := s.CallerInterproc.Load(int64(i))
		if op == nil {
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
	Class func(mirc.MIRClass) bool
	Type  func(*T.Type) bool
}

func (c *Checker) Check(op *ir.Operand) bool {
	return c.Type(op.Type) && c.Class(op.Mirc)
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

func checkInstr(s *state, instr *ir.Instr) *errors.CompilerError {
	if instr == nil {
		return nilInstr(s)
	}
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
	case IT.UnaryMinus, IT.UnaryPlus:
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

func checkArith(s *state, instr *ir.Instr) *errors.CompilerError {
	err := checkForm(instr, 2, 1)
	if err != nil {
		return err
	}
	a := instr.Operands[0]
	b := instr.Operands[1]
	dest := instr.Destination[0]

	err = checkRegs(s, instr)
	if err != nil {
		return err
	}
	s.SetReg(dest)

	err = checkEqual(instr, instr.Type, a.Type, b.Type, dest.Type)
	if err != nil {
		return err
	}
	return checkBinary(instr, num_imme, num_imme, num_reg)
}

func checkComp(s *state, instr *ir.Instr) *errors.CompilerError {
	err := checkForm(instr, 2, 1)
	if err != nil {
		return err
	}
	a := instr.Operands[0]
	b := instr.Operands[1]
	dest := instr.Destination[0]

	err = checkRegs(s, instr)
	if err != nil {
		return err
	}
	s.SetReg(dest)

	err = checkEqual(instr, instr.Type, a.Type, b.Type)
	if err != nil {
		return err
	}
	return checkBinary(instr, basic_imme, basic_imme, bool_reg)
}

func checkLogical(s *state, instr *ir.Instr) *errors.CompilerError {
	err := checkForm(instr, 2, 1)
	if err != nil {
		return err
	}
	a := instr.Operands[0]
	b := instr.Operands[1]
	dest := instr.Destination[0]

	err = checkRegs(s, instr)
	if err != nil {
		return err
	}
	s.SetReg(dest)

	err = checkEqual(instr, instr.Type, a.Type, b.Type, dest.Type)
	if err != nil {
		return err
	}
	return checkBinary(instr, bool_imme, bool_imme, bool_reg)
}

func checkUnaryArith(s *state, instr *ir.Instr) *errors.CompilerError {
	err := checkForm(instr, 1, 1)
	if err != nil {
		return err
	}
	a := instr.Operands[0]
	dest := instr.Destination[0]

	err = checkRegs(s, instr)
	if err != nil {
		return err
	}
	s.SetReg(dest)

	err = checkEqual(instr, instr.Type, a.Type, dest.Type)
	if err != nil {
		return err
	}
	return checkUnary(instr, num_imme, num_reg)
}

func checkNot(s *state, instr *ir.Instr) *errors.CompilerError {
	err := checkForm(instr, 1, 1)
	if err != nil {
		return err
	}
	a := instr.Operands[0]
	dest := instr.Destination[0]

	err = checkRegs(s, instr)
	if err != nil {
		return err
	}
	s.SetReg(dest)

	err = checkEqual(instr, instr.Type, a.Type, dest.Type)
	if err != nil {
		return err
	}
	return checkUnary(instr, bool_imme, bool_reg)
}

func checkConvert(s *state, instr *ir.Instr) *errors.CompilerError {
	err := checkForm(instr, 1, 1)
	if err != nil {
		return err
	}
	dest := instr.Destination[0]

	err = checkRegs(s, instr)
	if err != nil {
		return err
	}
	s.SetReg(dest)

	err = checkEqual(instr, instr.Type, dest.Type)
	if err != nil {
		return err
	}
	return checkUnary(instr, basic_imme, basic_reg)
}

func checkLoadPtr(s *state, instr *ir.Instr) *errors.CompilerError {
	err := checkForm(instr, 1, 1)
	if err != nil {
		return err
	}
	dest := instr.Destination[0]

	err = checkRegs(s, instr)
	if err != nil {
		return err
	}
	s.SetReg(dest)

	err = checkEqual(instr, instr.Type, dest.Type)
	if err != nil {
		return err
	}
	return checkUnary(instr, ptr_imme, basicOrProc_reg)
}

func checkStorePtr(s *state, instr *ir.Instr) *errors.CompilerError {
	err := checkForm(instr, 2, 0)
	if err != nil {
		return err
	}
	a := instr.Operands[0]
	dest := instr.Operands[1]

	err = checkRegs(s, instr)
	if err != nil {
		return err
	}

	err = checkEqual(instr, instr.Type, a.Type)
	if err != nil {
		return err
	}
	if basicOrProc_imme.Check(a) &&
		ptr_imme.Check(dest) {
		return nil
	}
	return malformedTypeOrClass(instr)
}

func checkLoad(s *state, instr *ir.Instr) *errors.CompilerError {
	err := checkForm(instr, 1, 1)
	if err != nil {
		return err
	}
	a := instr.Operands[0]
	dest := instr.Destination[0]

	s.SetReg(dest)

	err = checkEqual(instr, instr.Type, a.Type, dest.Type)
	if err != nil {
		return err
	}
	err = checkUnary(instr, basicOrProc_addr, basicOrProc_reg)
	if err != nil {
		return err
	}

	return checkLoadState(s, instr)
}

func checkStore(s *state, instr *ir.Instr) *errors.CompilerError {
	err := checkForm(instr, 1, 1)
	if err != nil {
		return err
	}
	a := instr.Operands[0]
	dest := instr.Destination[0]

	err = checkRegs(s, instr)
	if err != nil {
		return err
	}

	err = checkEqual(instr, instr.Type, a.Type, dest.Type)
	if err != nil {
		return err
	}
	err = checkUnary(instr, basicOrProc_imme, basicOrProc_addr)
	if err != nil {
		return err
	}

	return checkStoreState(s, instr)
}

func checkCopy(s *state, instr *ir.Instr) *errors.CompilerError {
	err := checkForm(instr, 1, 1)
	if err != nil {
		return err
	}
	a := instr.Operands[0]
	dest := instr.Destination[0]

	err = checkRegs(s, instr)
	if err != nil {
		return err
	}
	s.SetReg(dest)

	err = checkEqual(instr, instr.Type, a.Type, dest.Type)
	if err != nil {
		return err
	}
	err = checkUnary(instr, basicOrProc_imme, basicOrProc_reg)
	if err != nil {
		return err
	}

	return nil
}

func checkCall(s *state, instr *ir.Instr) *errors.CompilerError {
	err := checkForm(instr, 1, 0)
	if err != nil {
		return err
	}
	procOp := instr.Operands[0]
	if procOp.Symbol == nil {
		return procNotFound(instr)
	}
	proc := procOp.Type.Proc

	for i, formal_arg := range proc.Args {
		real_arg := s.CalleeInterproc.Load(int64(i))
		if real_arg == nil {
			return errorCallLoadingGarbage(instr)
		}
		if !formal_arg.Equals(real_arg.Type) {
			return procBadArg(instr, formal_arg, real_arg)
		}
		s.CalleeInterproc.Clear(i)
	}

	for i, formal_ret := range proc.Rets {
		op := &ir.Operand{Mirc: mirc.CalleeInterproc, Num: int64(i), Type: formal_ret}
		s.CalleeInterproc.Store(int64(i), op)
	}
	return nil
}

func checkLoadState(s *state, instr *ir.Instr) *errors.CompilerError {
	loadOp := instr.Operands[0]
	dest := instr.Destination[0]
	var source *ir.Operand
	switch loadOp.Mirc {
	case mirc.Spill:
		source = s.Spill.Load(loadOp.Num)
	case mirc.CalleeInterproc:
		source = s.CalleeInterproc.Load(loadOp.Num)
	case mirc.CallerInterproc:
		source = s.CallerInterproc.Load(loadOp.Num)
	case mirc.Local:
		source = newLocalOperand(loadOp.Symbol)
	default:
		panic("oh no")
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
	switch dest.Mirc {
	case mirc.Spill:
		s.Spill.Store(dest.Num, source)
	case mirc.CalleeInterproc:
		s.CalleeInterproc.Store(dest.Num, source)
	case mirc.CallerInterproc:
		s.CallerInterproc.Store(dest.Num, source)
	case mirc.Local:
		// TODO: BUG: should it do nothing?
	default:
		panic("oh no")
	}
	return nil
}

func checkRegs(s *state, instr *ir.Instr) *errors.CompilerError {
	for _, op := range instr.Operands {
		if op.Mirc == mirc.Register {
			o := s.Registers.Load(op.Num)
			if o == nil {
				return errorUsingRegisterGarbage(instr, op)
			}
			if o.Num != op.Num || o.Mirc != op.Mirc || !o.Type.Equals(op.Type) {
				return errorIncorrectValueInRegister(instr, o, op)
			}
		}
	}
	return nil
}

func checkEqual(instr *ir.Instr, types ...*T.Type) *errors.CompilerError {
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

func checkBinary(instr *ir.Instr, checkA, checkB, checkC Checker) *errors.CompilerError {
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

func checkUnary(instr *ir.Instr, checkA, checkC Checker) *errors.CompilerError {
	a := instr.Operands[0]
	dest := instr.Destination[0]

	if checkA.Check(a) &&
		checkC.Check(dest) {
		return nil
	}
	return malformedTypeOrClass(instr)
}

func checkInvalidClass(instr *ir.Instr) *errors.CompilerError {
	for _, op := range instr.Operands {
		if op.Mirc == mirc.InvalidMIRClass {
			return invalidClass(instr)
		}
	}
	for _, dest := range instr.Destination {
		if dest.Mirc == mirc.InvalidMIRClass {
			return invalidClass(instr)
		}
	}
	return nil
}

func checkForm(instr *ir.Instr, numOperands int, numDest int) *errors.CompilerError {
	if len(instr.Operands) != numOperands ||
		len(instr.Destination) != numDest {
		return malformedInstr(instr)
	}
	for _, op := range instr.Operands {
		if op == nil {
			return malformedInstr(instr)
		}
	}
	for _, op := range instr.Destination {
		if op == nil {
			return malformedInstr(instr)
		}
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
func invalidClass(instr *ir.Instr) *errors.CompilerError {
	return eu.NewInternalSemanticError("invalid class: " + instr.String())
}
func errorLoadingGarbage(instr *ir.Instr) *errors.CompilerError {
	return eu.NewInternalSemanticError("loading garbage: " + instr.String())
}
func errorCallLoadingGarbage(instr *ir.Instr) *errors.CompilerError {
	return eu.NewInternalSemanticError("call loading garbage: " + instr.String())
}
func errorUsingRegisterGarbage(instr *ir.Instr, op *ir.Operand) *errors.CompilerError {
	return eu.NewInternalSemanticError("using register garbage: " + op.String() + " of " + instr.String())
}
func errorIncorrectValueInRegister(instr *ir.Instr, o, op *ir.Operand) *errors.CompilerError {
	return eu.NewInternalSemanticError("incorrect value in register ("+o.String()+"): " + op.String() + " of " + instr.String())
}
func errorLoadingIncorrectType(instr *ir.Instr) *errors.CompilerError {
	return eu.NewInternalSemanticError("load of incorrect type: " + instr.String())
}
func procNotFound(instr *ir.Instr) *errors.CompilerError {
	return eu.NewInternalSemanticError("procedure not found: " + instr.String())
}
func procArgNotFound(instr *ir.Instr, d *ir.Symbol) *errors.CompilerError {
	return eu.NewInternalSemanticError("argument " + d.Name + " not found in: " + instr.String())
}
func procInvalidNumOfArgs(instr *ir.Instr, p *ir.Proc) *errors.CompilerError {
	n := strconv.Itoa(len(p.Args))
	beepBop := strconv.Itoa(len(instr.Operands) - 1)
	return eu.NewInternalSemanticError("expected " + n + " arguments, instead found: " + beepBop)
}
func procInvalidNumOfRets(instr *ir.Instr, p *ir.Proc) *errors.CompilerError {
	n := strconv.Itoa(len(p.Rets))
	beepBop := strconv.Itoa(len(instr.Destination))
	return eu.NewInternalSemanticError("expected " + n + " returns, instead found: " + beepBop)
}
func procBadArg(instr *ir.Instr, d *T.Type, op *ir.Operand) *errors.CompilerError {
	return eu.NewInternalSemanticError("argument " + op.String() + " doesn't match formal parameter (" + d.String() + ") in: " + instr.String())
}
func procBadRet(instr *ir.Instr, d T.Type, op *ir.Operand) *errors.CompilerError {
	return eu.NewInternalSemanticError("return " + op.String() + " doesn't match formal return " + d.String() + " in: " + instr.String())
}

func nilInstr(s *state) *errors.CompilerError {
	return eu.NewInternalSemanticError("nil instruction in: " + s.proc.Name + " " + s.bb.Label)
}
