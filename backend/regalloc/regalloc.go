package regalloc

import (
	hirc "mpc/frontend/enums/HIRClass"
	mirc "mpc/frontend/enums/MIRClass"
	T "mpc/frontend/enums/Type"
	FT "mpc/frontend/enums/flowType"
	IT "mpc/frontend/enums/instrType"
	ST "mpc/frontend/enums/symbolType"

	"mpc/frontend/ir"
	IRU "mpc/frontend/util/ir"

	"fmt"
	"strconv"
	"strings"
)

type reg int
type spill int
type calleeInterproc int
type callerInterproc int

type value struct {
	Class  hirc.HIRClass
	Symbol *ir.Symbol
	Num    int
}

func (v value) String() string {
	if v.Symbol != nil {
		return v.Class.String() + " " + v.Symbol.Name + " " + strconv.Itoa(v.Num)
	}
	return v.Class.String() + " " + strconv.Itoa(v.Num)
}

type stack struct {
	items []int
	top   int
}

func (s *stack) String() string {
	output := []string{}
	for _, item := range s.items[:s.top+1] {
		output = append(output, strconv.Itoa(item))
	}
	return "[" + strings.Join(output, ", ") + "]"
}

func newStack(size int) *stack {
	items := make([]int, size)
	for i := range items {
		items[i] = size - i - 1
	}
	return &stack{
		items: items,
		top:   size - 1,
	}
}

func (s *stack) HasItems() bool {
	return s.top >= 0
}

func (s *stack) Push(i int) {
	s.top++
	if s.top >= len(s.items) {
		s.items = append(s.items, make([]int, 2)...)
	}
	s.items[s.top] = i
}

func (s *stack) Pop() int {
	if s.top < 0 {
		return s.items[0]
	}
	item := s.items[s.top]
	s.top--
	return item
}

func (s *stack) Size() int {
	return s.top
}

type StorageClass int

func (s StorageClass) String() string {
	switch s {
	case Register:
		return "register"
	case Local:
		return "local"
	case CalleeInterProc:
		return "callee_interproc"
	case CallerInterProc:
		return "caller_interproc"
	case Spill:
		return "spill"
	}
	return "?"
}

func (s StorageClass) IsAddressable() bool {
	return s == Local || s == Spill || s == CalleeInterProc || s == CallerInterProc
}

func (s StorageClass) ToMirc() mirc.MIRClass {
	switch s {
	case Register:
		return mirc.Register
	case Local:
		return mirc.Local
	case CalleeInterProc:
		return mirc.CalleeInterproc
	case Spill:
		return mirc.Spill
	}
	return mirc.InvalidMIRClass
}

const (
	InvalidStorageClass StorageClass = iota

	Register
	Local
	CalleeInterProc
	CallerInterProc
	Spill
)

type useInfo struct {
	Place StorageClass
	Num   int
	T     T.Type
}

func (u useInfo) String() string {
	return u.Place.String() + " " + strconv.Itoa(u.Num) + " " + u.T.String()
}

type deferredInstr struct {
	index int
	instr *ir.Instr
}

type state struct {
	AvailableRegs *stack
	// UsedRegs[ reg ] retuns the value stored in the register
	UsedRegs map[reg]value

	AvailableSpills *stack
	// UsedSpills[ spill ] retuns the value stored in the spill address
	UsedSpills map[spill]value

	// LiveValues[ value ] retuns the register, interproc or spill storing this value
	LiveValues map[value]useInfo

	MaxCalleeInterproc int

	queuedInstr map[int][]*ir.Instr
	lastQueued  int
	atEnd       []*ir.Instr

	// stores the index of the furthest use of each value
	valueUse map[value]int
	bb       *ir.BasicBlock
	p        *ir.Proc
}

func newState(numRegs int, p *ir.Proc, bb *ir.BasicBlock) *state {
	return &state{
		AvailableRegs: newStack(numRegs),
		UsedRegs:      map[reg]value{},

		AvailableSpills: newStack(16),
		UsedSpills:      map[spill]value{},

		LiveValues: map[value]useInfo{},
		valueUse:   map[value]int{},

		queuedInstr: map[int][]*ir.Instr{},
		lastQueued:  0,
		atEnd:       []*ir.Instr{},

		p:  p,
		bb: bb,
	}
}

func (s *state) ExpectValue(v value) useInfo {
	info, ok := s.LiveValues[v]
	if !ok {
		//fmt.Println(s.String())
		panic("value not found: " + v.String())
	}
	return info
}

func (s *state) HasFreeRegs() bool {
	return s.AvailableRegs.HasItems()
}

func (s *state) Free(v value) {
	loc, ok := s.LiveValues[v]
	if !ok {
		panic("freeing unfound value")
	}
	delete(s.LiveValues, v)

	switch loc.Place {
	case Register:
		r := reg(loc.Num)
		s.FreeReg(r)
	case Spill:
		a := spill(loc.Num)
		s.FreeSpill(a)
	case Local, CalleeInterProc:
		panic("i dont think this should happen: freeing " + loc.Place.String())
	}
}

func (s *state) FreeReg(r reg) {
	_, ok := s.UsedRegs[r]
	if ok {
		delete(s.UsedRegs, r)
		s.AvailableRegs.Push(int(r))
		return
	}
	//fmt.Println(s.String())
	panic("freeing unused register: " + fmt.Sprint(r))
}

func (s *state) FreeSpill(a spill) {
	_, ok := s.UsedSpills[a]
	if ok {
		delete(s.UsedSpills, a)
		s.AvailableSpills.Push(int(a))
		return
	}
	panic("freeing unused spill")
}

func (s *state) AllocReg(v value, t T.Type) reg {
	r := reg(s.AvailableRegs.Pop())
	s.UsedRegs[r] = v
	s.LiveValues[v] = useInfo{Place: Register, Num: int(r), T: t}
	return r
}

func (s *state) FurthestUse(index int) (reg, value) {
	biggestIndex := index
	bestReg := reg(-1)
	var holdingValue value
	for v, info := range s.LiveValues {
		lastUse := s.valueUse[v]
		if info.Place == Register && lastUse > biggestIndex {
			biggestIndex = lastUse
			bestReg = reg(info.Num)
			holdingValue = v
		}
	}

	return bestReg, holdingValue
}

func (s *state) Spill(r reg, t T.Type) spill {
	v, ok := s.UsedRegs[r]
	if !ok {
		sreg := strconv.Itoa(int(r))
		panic("spilling unused register: " + sreg)
	}
	s.FreeReg(r)
	a := spill(s.AvailableSpills.Pop())
	s.UsedSpills[a] = v
	s.LiveValues[v] = useInfo{Place: Spill, Num: int(a), T: t}
	return a
}

func (s *state) UpdateMaxCalleeInterproc(numargs int, numrets int) {
	if numargs > s.MaxCalleeInterproc {
		s.MaxCalleeInterproc = numargs
	}
	if numrets > s.MaxCalleeInterproc {
		s.MaxCalleeInterproc = numrets
	}
}

// must preserve insertion order
func (s *state) QueueInstr(index int, instr *ir.Instr) {
	if index >= len(s.bb.Code) {
		s.atEnd = append(s.atEnd, instr)
		return
	}
	if index < 0 {
		index = 0
	}
	s.queuedInstr[index] = append(s.queuedInstr[index], instr)
	s.lastQueued++
}

func (s *state) String() string {
	livevalues := "Live Values ["
	for value, useinfo := range s.LiveValues {
		livevalues += "(" + value.String() + ", " + useinfo.String() + ") "
	}
	livevalues += "]"

	registers := "Used Regs ["
	for r, v := range s.UsedRegs {
		registers += "(" + fmt.Sprintf("%v, %v", r, v) + ")"
	}
	registers += "]"

	return livevalues + "\n" + registers
}

func Allocate(M *ir.Module, numRegs int) {
	for _, sy := range M.Globals {
		if sy.T == ST.Proc {
			allocProc(M, sy.Proc, numRegs)
		}
	}
}

func allocProc(M *ir.Module, p *ir.Proc, numRegs int) {
	var worklist = ir.FlattenGraph(p.Code)
	p.NumOfSpills = 0
	for _, curr := range worklist {
		s := newState(numRegs, p, curr)
		findUses(s)
		allocBlock(s)
		storeLiveLocals(s)
		if s.bb.Out.T == FT.Return {
			transformReturn(s)
		}
		insertQueuedInstrs(s)
		calcSpill(s)
	}
}

func calcSpill(s *state) {
	top := s.AvailableSpills.Size()
	if int(top) > s.p.NumOfSpills {
		s.p.NumOfSpills = int(top)
	}
}

func transformReturn(s *state) {
	end := len(s.bb.Code)
	for i, ret := range s.bb.Out.V {
		immediateRet := ensureImmediate(s, end, ret)
		callerInterproc := newOp(ret, mirc.CallerInterproc, i)
		loadRet := IRU.Store(immediateRet, callerInterproc)
		s.QueueInstr(end, loadRet)
	}
	s.bb.Out.V = nil
}

func findUses(s *state) {
	for index, instr := range s.bb.Code {
		vals := getUsedValues(instr)
		for _, v := range vals {
			s.valueUse[v] = index
		}
	}

	// check if value is returned or used in branching
	maxIndex := len(s.bb.Code)
	for _, op := range s.bb.Out.V {
		if op.Hirc == hirc.Temp || op.Hirc == hirc.Local || op.Hirc == hirc.Arg {
			v := toValue(op)
			s.valueUse[v] = maxIndex
		}
	}
}

func getUsedValues(instr *ir.Instr) []value {
	output := []value{}
	for _, op := range instr.Operands {
		if op.Hirc == hirc.Temp || op.Hirc == hirc.Local || op.Hirc == hirc.Arg {
			output = append(output, toValue(op))
		}
	}
	for _, dest := range instr.Destination {
		if dest.Hirc == hirc.Temp || dest.Hirc == hirc.Local || dest.Hirc == hirc.Arg {
			output = append(output, toValue(dest))
		}
	}
	return output
}

func insertQueuedInstrs(s *state) {
	newBlock := make([]*ir.Instr, len(s.bb.Code)+s.lastQueued+2)[0:0]
	for i, oldInstr := range s.bb.Code {
		newInstrs, ok := s.queuedInstr[i]
		if ok {
			for _, newInstr := range newInstrs {
				newBlock = append(newBlock, newInstr)
			}
		}
		newBlock = append(newBlock, oldInstr)
		delete(s.queuedInstr, i)
	}
	s.bb.Code = append(newBlock, s.atEnd...)
}

func allocBlock(s *state) {
	//fmt.Println(s.bb.Label)
	for i, instr := range s.bb.Code {
		//fmt.Println(s.String())
		//fmt.Print(instr, " >> ")
		switch instr.T {
		case IT.Add, IT.Sub, IT.Mult, IT.Div, IT.Rem,
			IT.Eq, IT.Diff, IT.Less,
			IT.More, IT.LessEq, IT.MoreEq,
			IT.Or, IT.And,
			IT.Offset:
			allocBinary(s, instr, i)
		case IT.Not,
			IT.UnaryMinus, IT.UnaryPlus,
			IT.Convert, IT.LoadPtr:
			allocUnary(s, instr, i)
		case IT.StorePtr:
			allocStorePtr(s, instr, i)
		case IT.Copy:
			allocCopy(s, instr, i)
		case IT.Call:
			allocCall(s, instr, i)
		}
		//fmt.Println(instr, "\n")
	}
}

func allocBinary(s *state, instr *ir.Instr, index int) {
	a := instr.Operands[0]
	b := instr.Operands[1]
	c := instr.Destination[0]
	ensureOperands(s, instr, index, a, b)
	instr.Destination[0] = ensureImmediate(s, index, c)
}

func allocUnary(s *state, instr *ir.Instr, index int) {
	a := instr.Operands[0]
	c := instr.Destination[0]
	ensureOperands(s, instr, index, a)
	instr.Destination[0] = ensureImmediate(s, index, c)
}

func allocStorePtr(s *state, instr *ir.Instr, index int) {
	a := instr.Operands[0]
	b := instr.Operands[1]
	ensureOperands(s, instr, index, a, b)
}

// Combination of possible Copy instructions
// Notation is: hirc (mirc) -> hirc (mirc)
// 	temp (spill|reg|calleeInter) -> temp (reg)
// 	temp (spill|reg|calleeInter) -> local (reg|local)
// 	temp (spill|reg|calleeInter) -> arg (reg|callerInter)
// 	local (reg|local) -> temp (reg)
// 	local (reg|local) -> local (reg|local)
// 	local (reg|local) -> arg (reg|callerInter)
// 	arg (reg|callerInter) -> temp (reg)
// 	arg (reg|callerInter) -> local (reg|local)
// 	arg (reg|callerInter) -> arg (reg|callerInter)
// 	global (static) -> temp (reg)
// 	global (static) -> local
// 	global (static) -> arg (reg|callerInter)
// 	lit (lit) -> temp (reg)
// 	lit (lit) -> local (reg|local)
// 	lit (lit) -> arg (reg|callerInter)
// c.HirC can only be Temp, Local or Arg
func allocCopy(s *state, instr *ir.Instr, index int) {
	a := instr.Operands[0]
	aAddr := isAddressable(s, a)
	instr.Operands[0] = toMirc(s, a)

	c := instr.Destination[0]
	cValue := toValue(c)
	cAddr := isAddressable(s, c)
	instr.Destination[0] = toMirc(s, c)

	if aAddr {
		if cAddr {
			// LOAD  a   -> reg
			// STORE reg -> c
			instr.T = IT.Load
			reg := allocReg(s, cValue, c.Type, index)
			instr.Destination[0] = reg
			s.QueueInstr(index, IRU.Store(reg, c))
		} else {
			// LOAD  a   -> c
			instr.T = IT.Load
		}
	} else {
		if cAddr {
			// STORE a -> c
			instr.T = IT.Store
		} else {
			// COPY  a -> c
		}
	}
	freeIfNotNeeded(s, index, toValue(a))
}

// transforms call instructions from:
// 	call <proc>, <arg1>, ..., <argN> -> <ret1>, ..., <retN>
// to:
// 	store arg1 -> interproc1
// 	...
// 	store argN -> interprocN
// 	call <proc>
//
// ret1 is assumed to be in interproc1
// retN is assumed to be in interprocN
func allocCall(s *state, instr *ir.Instr, index int) {
	spillAllLiveInterproc(s, index) // TODO: spill only the ones being corrupted
	loadArguments(s, instr, index)
	spillAllLiveRegisters(s, index)

	clearVolatiles(s, instr.Operands[0].Symbol.Proc) // TODO: clear only the ones being corrupted

	for i, dest := range instr.Destination {
		v := toValue(dest)
		callee := calleeInterproc(i)
		switch dest.Hirc {
		case hirc.Temp:
			s.LiveValues[v] = useInfo{Place: CalleeInterProc, Num: i, T: dest.Type}
		case hirc.Arg:
			op := loadCalleeInterproc(s, callee, v, dest.Type, index)
			r := reg(op.Num)
			arg := callerInterproc(v.Num)
			storeArg(s, r, arg, dest.Type, index)
		case hirc.Local:
			op := loadCalleeInterproc(s, callee, v, dest.Type, index)
			r := reg(op.Num)
			storeLocal(s, r, v.Symbol, index)
		}
	}

	s.UpdateMaxCalleeInterproc(len(instr.Operands)-1, len(instr.Destination))
	// removes operands from instruction
	instr.Operands = instr.Operands[0:1]
	instr.Destination = nil
}

func clearVolatiles(s *state, proc *ir.Proc) {
	toFree := []value{}
	for val, info := range s.LiveValues {
		if info.Place != Local && info.Place != Spill { 
			toFree = append(toFree, val)
		}
	}
	for _, v := range toFree {
		s.Free(v)
	}
}

func loadArguments(s *state, instr *ir.Instr, index int) {
	// ensure immediate, then store
	for i, op := range instr.Operands[1:] {
		immediate := ensureImmediate(s, index, op)
		arg := newOp(op, mirc.CalleeInterproc, i)
		storeArg := IRU.Store(immediate, arg)
		s.QueueInstr(index, storeArg)
		freeIfNotNeeded(s, index, toValue(op))
	}
}

func isAddressable(s *state, o *ir.Operand) bool {
	switch o.Hirc {
	case hirc.Temp:
		info, ok := s.LiveValues[toValue(o)]
		if ok {
			return info.Place.IsAddressable()
		}
		return false
	case hirc.Local, hirc.Arg:
		info, ok := s.LiveValues[toValue(o)]
		if ok {
			return info.Place.IsAddressable()
		}
		return true
	case hirc.Lit, hirc.Global:
		return false
	}
	panic("isAddressable: wtf")
}

func toMirc(s *state, o *ir.Operand) *ir.Operand {
	info, ok := s.LiveValues[toValue(o)]
	if ok {
		return newOp(o, info.Place.ToMirc(), info.Num)
	}
	switch o.Hirc {
	case hirc.Temp:
		panic("toMirc: temp is not alive")
	case hirc.Local:
		return newOp(o, mirc.Local, o.Num)
	case hirc.Arg:
		return newOp(o, mirc.CallerInterproc, o.Num)
	case hirc.Global:
		return newOp(o, mirc.Static, o.Num)
	case hirc.Lit:
		return newOp(o, mirc.Lit, o.Num)
	}
	panic("toMirc: wtf")
}

func storeLiveLocals(s *state) {
	end := len(s.bb.Code)
	for val, info := range s.LiveValues {
		if info.Place == Register {
			if val.Class == hirc.Local {
				r := reg(info.Num)
				storeLocal(s, r, val.Symbol, end)
			}
			if val.Class == hirc.Arg {
				r := reg(info.Num)
				it := callerInterproc(val.Num)
				storeArg(s, r, it, info.T, end)
			}
		}
	}
}

func spillAllLiveInterproc(s *state, index int) {
	for val, info := range s.LiveValues {
		if info.Place == CalleeInterProc && val.Class == hirc.Temp {
			callee := calleeInterproc(info.Num)
			op := loadCalleeInterproc(s, callee, val, info.T, index)
			r := reg(op.Num)
			spillTemp(s, r, info.T, index)
			s.FreeReg(r)
		}
	}
}

func spillAllLiveRegisters(s *state, index int) {
	for val, info := range s.LiveValues {
		if info.Place == Register {
			switch val.Class {
			case hirc.Local:
				r := reg(info.Num)
				storeLocal(s, r, val.Symbol, index)
			case hirc.Arg:
				r := reg(info.Num)
				arg := callerInterproc(val.Num)
				storeArg(s, r, arg, info.T, index)
			case hirc.Temp:
				spillTemp(s, reg(info.Num), info.T, index)
			}
		}
	}
}

func ensureOperands(s *state, instr *ir.Instr, index int, ops ...*ir.Operand) {
	newOps := make([]*ir.Operand, len(ops))
	for i, op := range ops {
		newOps[i] = ensureImmediate(s, index, op)
	}
	for _, op := range ops {
		if op.Hirc == hirc.Local || op.Hirc == hirc.Arg || op.Hirc == hirc.Temp {
			freeIfNotNeeded(s, index, toValue(op))
		}
	}
	instr.Operands = newOps
}

func ensureImmediate(s *state, index int, op *ir.Operand) *ir.Operand {
	v := toValue(op)
	info, ok := s.LiveValues[v]
	if ok {
		switch info.Place {
		case Register:
			return newRegOp(reg(info.Num), op.Type)
		case Spill:
			return loadSpill(s, v, info, index)
		case CalleeInterProc:
			callee := calleeInterproc(info.Num)
			return loadCalleeInterproc(s, callee, v, info.T, index)
		case CallerInterProc:
			caller := callerInterproc(info.Num)
			return loadCallerInterproc(s, v, caller, index)
		case Local:
			return loadLocal(s, v, info.T, index)
		}
		panic("ensureImmediate: Invalid StorageClass")
	}
	switch op.Hirc {
	case hirc.Temp:
		return allocReg(s, v, op.Type, index)
	case hirc.Local:
		return loadLocal(s, v, op.Type, index)
	case hirc.Arg:
		return loadArg(s, v, op.Type, index)
	case hirc.Global:
		return newOp(op, mirc.Static, op.Num)
	case hirc.Lit:
		return newOp(op, mirc.Lit, op.Num)
	}
	panic("ensureImmediate: Invalid HIRClass")
}

func newOp(op *ir.Operand, m mirc.MIRClass, num int) *ir.Operand {
	return &ir.Operand{
		Hirc:   op.Hirc,
		Mirc:   m,
		Type:   op.Type,
		Symbol: op.Symbol,
		Num:    num,
	}
}

func loadCalleeInterproc(s *state, callee calleeInterproc, v value, t T.Type, index int) *ir.Operand {
	newOp := newCalleeInterprocOperand(callee, t)
	rOp := allocReg(s, v, t, index)
	load := IRU.Load(newOp, rOp)
	s.QueueInstr(index, load)
	return rOp
}

func loadCallerInterproc(s *state, v value, caller callerInterproc, index int) *ir.Operand {
	t := v.Symbol.Type
	newOp := newCallerInterprocOperand(caller, t)
	rOp := allocReg(s, v, t, index)
	load := IRU.Load(newOp, rOp)
	s.QueueInstr(index, load)
	return rOp
}

func loadLocal(s *state, v value, t T.Type, index int) *ir.Operand {
	newOp := newLocalOperand(v.Symbol)
	rOp := allocReg(s, v, t, index)
	load := IRU.Load(newOp, rOp)
	s.QueueInstr(index, load)
	return rOp
}

// TODO: test loadArg
func loadArg(s *state, v value, t T.Type, index int) *ir.Operand {
	newOp := newCallerInterprocOperand(callerInterproc(v.Num), t)
	rOp := allocReg(s, v, t, index)
	load := IRU.Load(newOp, rOp)
	s.QueueInstr(index, load)
	return rOp
}

func loadSpill(s *state, v value, info useInfo, index int) *ir.Operand {
	sp := spill(info.Num)
	newOp := newSpillOperand(sp, info.T)
	s.FreeSpill(sp)
	rOp := allocReg(s, v, info.T, index)
	load := IRU.Load(newOp, rOp)
	s.QueueInstr(index, load)
	return rOp
}

func allocReg(s *state, v value, t T.Type, index int) *ir.Operand {
	if s.HasFreeRegs() {
		r := s.AllocReg(v, t)
		return newRegOp(r, t)
	}
	return spillUsedAndAlloc(s, v, t, index)
}

func spillUsedAndAlloc(s *state, v value, t T.Type, index int) *ir.Operand {
	r, val := s.FurthestUse(index)
	if r == -1 {
		panic("not enough registers")
	}
	switch val.Class {
	case hirc.Temp:
		spillTemp(s, r, t, index)
	case hirc.Local:
		storeLocal(s, r, val.Symbol, index-1)
		s.Free(val)
	case hirc.Arg:
		arg := callerInterproc(val.Num)
		storeArg(s, r, arg, t, index-1)
		s.Free(val)
	case hirc.Lit, hirc.Global:
		panic("what the fuck are we even doing")
	}

	r2 := s.AllocReg(v, t)
	if r != r2 {
		panic("spillRegister: " + fmt.Sprintf("%v\n", s.AvailableRegs))
	}
	return newRegOp(r, t)
}

func spillTemp(s *state, r reg, t T.Type, index int) {
	sNum := s.Spill(r, t)
	spillOp := newSpillOperand(sNum, t)
	regOp := newRegOp(r, t)
	store := IRU.Store(regOp, spillOp)
	s.QueueInstr(index, store)
}

func storeLocal(s *state, r reg, symbol *ir.Symbol, index int) {
	reg := newRegOp(r, symbol.Type)
	loc := newLocalOperand(symbol)
	instr := IRU.Store(reg, loc)
	s.QueueInstr(index, instr)
}

func storeArg(s *state, r reg, num callerInterproc, t T.Type, index int) {
	//fmt.Printf("\n\n--\n\n%v\nRegister: %v, Caller: %v, Index: %v\n--\n\n", s, r, num, index)
	reg := newRegOp(r, t)
	loc := newCallerInterprocOperand(num, t)
	instr := IRU.Store(reg, loc)
	s.QueueInstr(index, instr)
}

func newRegOp(r reg, t T.Type) *ir.Operand {
	return &ir.Operand{
		Mirc: mirc.Register,
		Num:  int(r),
		Type: t,
	}
}

func newSpillOperand(sNum spill, t T.Type) *ir.Operand {
	return &ir.Operand{
		Mirc: mirc.Spill,
		Num:  int(sNum),
		Type: t,
	}
}

func newLocalOperand(sy *ir.Symbol) *ir.Operand {
	return &ir.Operand{
		Mirc:   mirc.Local,
		Symbol: sy,
		Type:   sy.Type,
	}
}

func newCalleeInterprocOperand(i calleeInterproc, t T.Type) *ir.Operand {
	return &ir.Operand{
		Mirc: mirc.CalleeInterproc,
		Type: t,
		Num:  int(i),
	}
}

func newCallerInterprocOperand(i callerInterproc, t T.Type) *ir.Operand {
	return &ir.Operand{
		Mirc: mirc.CallerInterproc,
		Type: t,
		Num:  int(i),
	}
}

// can only insert free after current instruction
// TODO: whenever freeing dirty values, see if it's necessary to store them back (mutated)
func freeIfNotNeeded(s *state, index int, v value) {
	useInfo, ok := s.LiveValues[v]
	if !ok {
		return // already freed (i hope)
	}
	if isNeeded(s, index, v, useInfo) {
		return
	}
	if v.Class == hirc.Local {
		r := reg(useInfo.Num)
		storeLocal(s, r, v.Symbol, index+1)
	}
	if v.Class == hirc.Arg {
		r := reg(useInfo.Num)
		arg := callerInterproc(v.Num)
		storeArg(s, r, arg, useInfo.T, index+1)
	}
	s.Free(v)
}

func isNeeded(s *state, index int, v value, useInfo useInfo) bool {
	lastUse := s.valueUse[v]
	if lastUse > index {
		return true
	}
	return false
}

func toValue(op *ir.Operand) value {
	return value{
		Class:  op.Hirc,
		Symbol: op.Symbol,
		Num:    op.Num,
	}
}
