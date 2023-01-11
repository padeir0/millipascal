package resalloc

import (
	T "mpc/frontend/Type"
	hirc "mpc/frontend/enums/HIRClass"
	mirc "mpc/frontend/enums/MIRClass"
	FT "mpc/frontend/enums/flowType"
	IT "mpc/frontend/enums/instrType"
	ST "mpc/frontend/enums/symbolType"

	"mpc/frontend/ir"
	IRU "mpc/frontend/util/ir"

	"fmt"
	"strconv"
	"strings"
)

type reg int64
type spill int64
type calleeInterproc int64
type callerInterproc int64

type value struct {
	Class  hirc.HIRClass
	Symbol *ir.Symbol
	Num    int64
}

func (v value) String() string {
	if v.Symbol != nil {
		return v.Class.String() + " " + v.Symbol.Name + " " + strconv.FormatInt(v.Num, 10)
	}
	return v.Class.String() + " " + strconv.FormatInt(v.Num, 10)
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
	Place   StorageClass
	Num     int64
	T       *T.Type
	Mutated bool
}

func (u useInfo) String() string {
	return u.Place.String() + " " + strconv.FormatInt(u.Num, 10) + " " + u.T.String()
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

	prependInstr map[int][]*ir.Instr
	appendInstr  map[int][]*ir.Instr
	lastQueued   int

	atEnd []*ir.Instr

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

		prependInstr: map[int][]*ir.Instr{},
		appendInstr:  map[int][]*ir.Instr{},
		lastQueued:   0,
		atEnd:        []*ir.Instr{},

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

func (s *state) Mark(v value) {
	info, ok := s.LiveValues[v]
	if !ok {
		panic("marking dead value!")
	}
	info.Mutated = true
	s.LiveValues[v] = info
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
	case CalleeInterProc:
		// no need to keep track of this
	case Local, CallerInterProc:
		panic("freeing " + loc.Place.String())
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

func (s *state) AllocReg(v value, t *T.Type) reg {
	info, ok := s.LiveValues[v]
	if ok && info.Place == Register {
		// this should be fine, live values shouldn't be corrupt
		return reg(info.Num)
	}
	r := reg(s.AvailableRegs.Pop())
	s.UsedRegs[r] = v
	s.LiveValues[v] = useInfo{Place: Register, Num: int64(r), T: t}
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

func (s *state) Spill(r reg, t *T.Type) spill {
	v, ok := s.UsedRegs[r]
	if !ok {
		sreg := strconv.Itoa(int(r))
		panic("spilling unused register: " + sreg)
	}
	useinfo := s.LiveValues[v]
	s.FreeReg(r)
	a := spill(s.AvailableSpills.Pop())
	s.UpdateMaxSpill(int(a) + 1)
	s.UsedSpills[a] = v
	s.LiveValues[v] = useInfo{Place: Spill, Num: int64(a), T: t, Mutated: useinfo.Mutated}
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

func (s *state) UpdateMaxSpill(spill int) {
	if spill > s.p.NumOfSpills {
		s.p.NumOfSpills = spill
	}
}

// must preserve insertion order
func (s *state) PrependInstr(index int, instr *ir.Instr) {
	if index == len(s.bb.Code) {
		s.atEnd = append(s.atEnd, instr)
		s.lastQueued++
		return
	}
	if index < 0 || index > len(s.bb.Code) {
		panic(fmt.Sprintf("out of bounds: %v with instr %v", index, instr.String()))
	}
	s.prependInstr[index] = append(s.prependInstr[index], instr)
	s.lastQueued++
}

// must preserve insertion order
// we let index == len(block.Code) pass to insert at end
func (s *state) AppendInstr(index int, instr *ir.Instr) {
	if index == len(s.bb.Code) {
		s.atEnd = append(s.atEnd, instr)
		s.lastQueued++
		return
	}
	if index < 0 || index > len(s.bb.Code) {
		panic(fmt.Sprintf("out of bounds: %v with instr %v", index, instr.String()))
	}
	s.appendInstr[index] = append(s.appendInstr[index], instr)
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
	allocate(M, numRegs)
	M.ResetVisited()
}

func allocate(M *ir.Module, numRegs int) {
	if M.Visited {
		return
	}
	for _, dep := range M.Dependencies {
		allocate(dep.M, numRegs)
	}
	M.Visited = true
	for _, sy := range M.Globals {
		if sy.T == ST.Proc && !sy.External {
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
		if !s.bb.IsTerminal() {
			storeLiveLocals(s)
		}
		transformFlow(s)
		insertQueuedInstrs(s)
		calcRegions(s)
	}
	p.NumOfVars = len(p.Vars)
}

func calcRegions(s *state) {
	if s.p.NumOfMaxCalleeArguments < s.MaxCalleeInterproc {
		s.p.NumOfMaxCalleeArguments = s.MaxCalleeInterproc
	}
}

func transformFlow(s *state) {
	switch s.bb.Out.T {
	case FT.Return:
		transformReturn(s)
	case FT.Exit, FT.If:
		s.bb.Out.V = []*ir.Operand{toMirc(s, s.bb.Out.V[0])}
	}
}

// TODO: OPT: transformReturn should look if the value is already where it needs to be (in the respective Caller Interproc)
func transformReturn(s *state) {
	type RetVal struct {
		Index int64
		Op    *ir.Operand
	}

	notAlive := []RetVal{}
	end := len(s.bb.Code)

	// load the already immediate ones first
	for i, ret := range s.bb.Out.V {
		rVal := toValue(ret)
		info, ok := s.LiveValues[rVal]
		if ok && info.Place == Register {
			regOp := newRegOp(reg(info.Num), info.T)
			callerInterproc := newOp(ret, mirc.CallerInterproc, int64(i))
			loadRet := IRU.Store(regOp, callerInterproc)
			s.AppendInstr(end, loadRet)
			s.Free(rVal)
		} else {
			rv := RetVal{Index: int64(i), Op: ret}
			notAlive = append(notAlive, rv)
		}
	}

	// then load the remaining
	for _, ret := range notAlive {
		immediateRet := ensureImmediate(s, end, toValue(ret.Op), ret.Op.Type)
		callerInterproc := newOp(ret.Op, mirc.CallerInterproc, ret.Index)
		loadRet := IRU.Store(immediateRet, callerInterproc)
		s.AppendInstr(end, loadRet)
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
	maxIndex := 1 << 31 // ensure it's after the s.atEnd instructions
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
		newPrepInstrs, ok := s.prependInstr[i]
		if ok {
			for _, newInstr := range newPrepInstrs {
				newBlock = append(newBlock, newInstr)
			}
		}

		newBlock = append(newBlock, oldInstr)

		newAppInstrs, ok := s.appendInstr[i]
		if ok {
			for _, newInstr := range newAppInstrs {
				newBlock = append(newBlock, newInstr)
			}
		}
		delete(s.prependInstr, i)
		delete(s.appendInstr, i)
	}
	s.bb.Code = append(newBlock, s.atEnd...)
}

func allocBlock(s *state) {
	//fmt.Println(s.bb.Label)
	for i, instr := range s.bb.Code {
		//fmt.Println(s.String())
		//fmt.Println("hirc: ", instr)
		switch instr.T {
		case IT.Add, IT.Sub, IT.Mult, IT.Div, IT.Rem,
			IT.Eq, IT.Diff, IT.Less,
			IT.More, IT.LessEq, IT.MoreEq,
			IT.Or, IT.And:
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
		//fmt.Println("mirc: ", instr, "\n")
	}
}

func freeOperands(s *state, index int, ops ...*ir.Operand) {
	for _, op := range ops {
		freeIfNotNeeded(s, index, toValue(op))
	}
}

func allocBinary(s *state, instr *ir.Instr, index int) {
	a := instr.Operands[0]
	b := instr.Operands[1]
	c := instr.Destination[0]
	cv := toValue(c)
	ensureOperands(s, instr, index, a, b)
	instr.Destination[0] = ensureImmediate(s, index, cv, c.Type)
	s.Mark(cv)
	freeOperands(s, index, a, b)
}

func allocUnary(s *state, instr *ir.Instr, index int) {
	a := instr.Operands[0]
	c := instr.Destination[0]
	cv := toValue(c)
	ensureOperands(s, instr, index, a)
	instr.Destination[0] = ensureImmediate(s, index, cv, c.Type)
	s.Mark(cv)
	freeOperands(s, index, a)
}

func allocStorePtr(s *state, instr *ir.Instr, index int) {
	a := instr.Operands[0]
	b := instr.Operands[1]
	ensureOperands(s, instr, index, a, b)
	freeOperands(s, index, a, b)
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
	source := instr.Operands[0]
	sourceIsAddr := isAddressable(s, source)

	dest := instr.Destination[0]
	destIsAddr := isAddressable(s, dest)

	if sourceIsAddr {
		if destIsAddr {
			// TODO: OPT: see if value is already in register
			// LOAD  source -> reg
			// STORE reg    -> dest
			instr.T = IT.Load
			instr.Operands[0] = toMirc(s, source)
			reg := allocReg(s, toValue(source), source.Type, index)
			instr.Destination[0] = reg

			destMirc := toMirc(s, dest)
			corruptOldVersion(s, dest)
			// insert after the Load instruction
			s.AppendInstr(index, IRU.Store(reg, destMirc))
		} else {
			// LOAD source -> dest
			instr.T = IT.Load
			instr.Operands[0] = toMirc(s, source)
			instr.Destination[0] = toMirc(s, dest)
			s.Mark(toValue(dest))
		}
	} else {
		if destIsAddr {
			// STORE source -> dest
			instr.T = IT.Store
			instr.Operands[0] = toMirc(s, source)
			instr.Destination[0] = toMirc(s, dest)
			corruptOldVersion(s, dest)
		} else {
			//fmt.Println(source, dest)
			// COPY source -> dest
			instr.Operands[0] = toMirc(s, source)
			instr.Destination[0] = toMirc(s, dest)
			s.Mark(toValue(dest))
		}
	}
	freeOperands(s, index, source)
}

func corruptOldVersion(s *state, op *ir.Operand) {
	v := toValue(op)
	info, ok := s.LiveValues[v]
	if ok && info.Place == Register {
		s.Free(v)
	}
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
	// TODO: OPT: spillAllLiveInterproc should only spill the ones being corrupted
	spillAllLiveInterproc(s, index)
	loadArguments(s, instr, index)
	spillAllLiveRegisters(s, index)
	// TODO: OPT: clearVolatiles should only clear the ones being corrupted
	clearVolatiles(s, instr.Operands[0].Symbol.Proc)

	for i, dest := range instr.Destination {
		v := toValue(dest)
		callee := calleeInterproc(i)
		switch dest.Hirc {
		case hirc.Temp:
			s.LiveValues[v] = useInfo{Place: CalleeInterProc, Num: int64(i), T: dest.Type}
		case hirc.Arg:
			load, op := loadCalleeInterproc(s, callee, v, dest.Type, index)
			s.AppendInstr(index, load)
			r := reg(op.Num)
			arg := callerInterproc(v.Num)
			store := storeArg(r, arg, dest.Type)
			s.AppendInstr(index, store)
		case hirc.Local:
			load, op := loadCalleeInterproc(s, callee, v, dest.Type, index)
			s.AppendInstr(index, load)
			r := reg(op.Num)
			store := storeLocal(r, v.Num, v.Symbol)
			s.AppendInstr(index, store)
		}
	}
	freeOperands(s, index, instr.Operands[1:]...)

	s.UpdateMaxCalleeInterproc(len(instr.Operands)-1, len(instr.Destination))
	// removes operands from instruction
	instr.Operands = []*ir.Operand{toMirc(s, instr.Operands[0])}
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
		v := toValue(op)
		info, ok := s.LiveValues[v]
		if ok && info.Place == CalleeInterProc && info.Num == int64(i) {
			// if it's already where it needs to be
			continue
		}
		immediate := ensureImmediate(s, index, v, op.Type)
		arg := newOp(op, mirc.CalleeInterproc, int64(i))
		storeArg := IRU.Store(immediate, arg)
		s.PrependInstr(index, storeArg)
	}
}

func isAddressable(s *state, o *ir.Operand) bool {
	switch o.Hirc {
	case hirc.Temp:
		info, ok := s.LiveValues[toValue(o)]
		if ok {
			return info.Place.IsAddressable()
		}
		panic("isAddressable: temp is not alive")
	case hirc.Lit, hirc.Global:
		return false
	case hirc.Local, hirc.Arg:
		return true
	}
	panic("isAddressable: wtf")
}

func toMirc(s *state, o *ir.Operand) *ir.Operand {
	switch o.Hirc {
	case hirc.Temp:
		info, ok := s.LiveValues[toValue(o)]
		if ok {
			return newOp(o, info.Place.ToMirc(), info.Num)
		}
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
	for val, info := range s.LiveValues {
		if info.Place == Register && info.Mutated {
			if val.Class == hirc.Local {
				r := reg(info.Num)
				instr := storeLocal(r, val.Num, val.Symbol)
				s.AppendInstr(len(s.bb.Code), instr)
			}
			if val.Class == hirc.Arg {
				r := reg(info.Num)
				it := callerInterproc(val.Num)
				instr := storeArg(r, it, info.T)
				s.AppendInstr(len(s.bb.Code), instr)
			}
		}
	}
}

func spillAllLiveInterproc(s *state, index int) {
	for val, info := range s.LiveValues {
		lastUse := s.valueUse[val]
		if info.Place == CalleeInterProc && lastUse > index {
			callee := calleeInterproc(info.Num)
			instr, op := loadCalleeInterproc(s, callee, val, info.T, index)
			s.PrependInstr(index, instr)
			r := reg(op.Num)
			spill := spillTemp(s, r, info.T)
			s.PrependInstr(index, spill)
		}
	}
}

func spillAllLiveRegisters(s *state, index int) {
	for val, info := range s.LiveValues {
		if info.Place == Register {
			switch val.Class {
			case hirc.Local:
				r := reg(info.Num)
				s.PrependInstr(index, storeLocal(r, val.Num, val.Symbol))
			case hirc.Arg:
				r := reg(info.Num)
				arg := callerInterproc(val.Num)
				s.PrependInstr(index, storeArg(r, arg, info.T))
			case hirc.Temp:
				s.PrependInstr(index, spillTemp(s, reg(info.Num), info.T))
			}
		}
	}
}

func ensureOperands(s *state, instr *ir.Instr, index int, ops ...*ir.Operand) {
	newOps := make([]*ir.Operand, len(ops))
	for i, op := range ops {
		newOps[i] = ensureImmediate(s, index, toValue(op), op.Type)
	}
	instr.Operands = newOps
	for _, op := range ops {
		freeIfNotNeededAndNotMutated(s, index, toValue(op))
	}
}

func ensureImmediate(s *state, index int, v value, t *T.Type) *ir.Operand {
	info, ok := s.LiveValues[v]
	if ok {
		switch info.Place {
		case Register:
			return newRegOp(reg(info.Num), t)
		case Spill:
			instr, op := loadSpill(s, v, info, index)
			s.PrependInstr(index, instr)
			return op
		case CalleeInterProc:
			callee := calleeInterproc(info.Num)
			instr, op := loadCalleeInterproc(s, callee, v, info.T, index)
			s.PrependInstr(index, instr)
			return op
		case CallerInterProc:
			caller := callerInterproc(info.Num)
			instr, op := loadCallerInterproc(s, v, caller, index)
			s.PrependInstr(index, instr)
			return op
		case Local:
			instr, op := loadLocal(s, v, info.T, index)
			s.PrependInstr(index, instr)
			return op
		}
		panic("ensureImmediate: Invalid StorageClass")
	}
	switch v.Class {
	case hirc.Temp:
		return allocReg(s, v, t, index)
	case hirc.Local:
		instr, op := loadLocal(s, v, t, index)
		s.PrependInstr(index, instr)
		return op
	case hirc.Arg:
		instr, op := loadArg(s, v, t, index)
		s.PrependInstr(index, instr)
		return op
	case hirc.Global:
		return newOpFromValue(v, t, mirc.Static, v.Num)
	case hirc.Lit:
		return newOpFromValue(v, t, mirc.Lit, v.Num)
	}
	panic("ensureImmediate: Invalid HIRClass")
}

func newOpFromValue(v value, t *T.Type, m mirc.MIRClass, num int64) *ir.Operand {
	return &ir.Operand{
		Hirc:   v.Class,
		Mirc:   m,
		Type:   t,
		Symbol: v.Symbol,
		Num:    num,
	}
}

func newOp(op *ir.Operand, m mirc.MIRClass, num int64) *ir.Operand {
	return &ir.Operand{
		Hirc:   op.Hirc,
		Mirc:   m,
		Type:   op.Type,
		Symbol: op.Symbol,
		Num:    num,
	}
}

func loadCalleeInterproc(s *state, callee calleeInterproc, v value, t *T.Type, index int) (*ir.Instr, *ir.Operand) {
	newOp := newCalleeInterprocOperand(callee, t)
	rOp := allocReg(s, v, t, index)
	load := IRU.Load(newOp, rOp)
	return load, rOp
}

func loadCallerInterproc(s *state, v value, caller callerInterproc, index int) (*ir.Instr, *ir.Operand) {
	t := v.Symbol.Type
	newOp := newCallerInterprocOperand(caller, t)
	rOp := allocReg(s, v, t, index)
	load := IRU.Load(newOp, rOp)
	return load, rOp
}

func loadLocal(s *state, v value, t *T.Type, index int) (*ir.Instr, *ir.Operand) {
	newOp := newLocalOperand(v.Num, v.Symbol)
	rOp := allocReg(s, v, t, index)
	load := IRU.Load(newOp, rOp)
	return load, rOp
}

func loadArg(s *state, v value, t *T.Type, index int) (*ir.Instr, *ir.Operand) {
	newOp := newCallerInterprocOperand(callerInterproc(v.Num), t)
	rOp := allocReg(s, v, t, index)
	load := IRU.Load(newOp, rOp)
	return load, rOp
}

func loadSpill(s *state, v value, info useInfo, index int) (*ir.Instr, *ir.Operand) {
	sp := spill(info.Num)
	newOp := newSpillOperand(sp, info.T)
	s.FreeSpill(sp)
	rOp := allocReg(s, v, info.T, index)
	load := IRU.Load(newOp, rOp)
	return load, rOp
}

func allocReg(s *state, v value, t *T.Type, index int) *ir.Operand {
	if s.HasFreeRegs() {
		r := s.AllocReg(v, t)
		return newRegOp(r, t)
	}
	r, val := s.FurthestUse(index)
	if r == -1 {
		//fmt.Print("\n------\n")
		//fmt.Println(s)
		//fmt.Printf("Value: %v, Type: %v, Index: %v", v, t, index)
		panic("not enough registers")
	}
	switch val.Class {
	case hirc.Temp:
		s.PrependInstr(index, spillTemp(s, r, t))
	case hirc.Local:
		s.PrependInstr(index, storeLocal(r, val.Num, val.Symbol))
		s.Free(val)
	case hirc.Arg:
		arg := callerInterproc(val.Num)
		s.PrependInstr(index, storeArg(r, arg, t))
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

func spillTemp(s *state, r reg, t *T.Type) *ir.Instr {
	sNum := s.Spill(r, t)
	spillOp := newSpillOperand(sNum, t)
	regOp := newRegOp(r, t)
	return IRU.Store(regOp, spillOp)
}

func storeLocal(r reg, position int64, symbol *ir.Symbol) *ir.Instr {
	reg := newRegOp(r, symbol.Type)
	loc := newLocalOperand(position, symbol)
	return IRU.Store(reg, loc)
}

func storeArg(r reg, num callerInterproc, t *T.Type) *ir.Instr {
	reg := newRegOp(r, t)
	loc := newCallerInterprocOperand(num, t)
	return IRU.Store(reg, loc)
}

func newRegOp(r reg, t *T.Type) *ir.Operand {
	return &ir.Operand{
		Mirc: mirc.Register,
		Num:  int64(r),
		Type: t,
	}
}

func newSpillOperand(sNum spill, t *T.Type) *ir.Operand {
	return &ir.Operand{
		Mirc: mirc.Spill,
		Num:  int64(sNum),
		Type: t,
	}
}

func newLocalOperand(position int64, sy *ir.Symbol) *ir.Operand {
	return &ir.Operand{
		Mirc:   mirc.Local,
		Symbol: sy,
		Type:   sy.Type,
		Num:    position,
	}
}

func newCalleeInterprocOperand(i calleeInterproc, t *T.Type) *ir.Operand {
	return &ir.Operand{
		Mirc: mirc.CalleeInterproc,
		Type: t,
		Num:  int64(i),
	}
}

func newCallerInterprocOperand(i callerInterproc, t *T.Type) *ir.Operand {
	return &ir.Operand{
		Mirc: mirc.CallerInterproc,
		Type: t,
		Num:  int64(i),
	}
}

// can only insert free after current instruction
func freeIfNotNeededAndNotMutated(s *state, index int, v value) {
	useInfo, ok := s.LiveValues[v]
	if !ok {
		return // already freed (i hope)
	}
	if isNeeded(s, index, v, useInfo) {
		return
	}
	if useInfo.Mutated {
		return
	}
	s.Free(v)
}

// can only insert free after current instruction
func freeIfNotNeeded(s *state, index int, v value) {
	useInfo, ok := s.LiveValues[v]
	if !ok {
		return // already freed (i hope)
	}
	if isNeeded(s, index, v, useInfo) {
		return
	}
	if !s.bb.IsTerminal() { // no need to restore if is terminal
		if v.Class == hirc.Local && useInfo.Mutated {
			r := reg(useInfo.Num)
			instr := storeLocal(r, v.Num, v.Symbol)
			s.AppendInstr(index, instr)
		}
		if v.Class == hirc.Arg && useInfo.Mutated {
			r := reg(useInfo.Num)
			arg := callerInterproc(v.Num)
			instr := storeArg(r, arg, useInfo.T)
			s.AppendInstr(index, instr)
		}
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
