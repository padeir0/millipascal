package regalloc

import (
	hirc "mpc/frontend/enums/HIRClass"
	mirc "mpc/frontend/enums/MIRClass"
	T "mpc/frontend/enums/Type"
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

type value struct {
	T      hirc.HIRClass
	Symbol *ir.Symbol
	Num    int
}

func (v value) String() string {
	if v.Symbol != nil {
		return v.T.String() + " " + v.Symbol.Name + " " + strconv.Itoa(v.Num)
	}
	return v.T.String() + " " + strconv.Itoa(v.Num)
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
	return "[" +strings.Join(output, ", ")+"]"
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
		return "interproc"
	case Spill:
		return "spill"
	}
	return "?"
}

func (s StorageClass) IsAddressable() bool {
	return s == Local || s == Spill || s == CalleeInterProc
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
	Spill
)

type useInfo struct {
	Place StorageClass
	Num   int
}

func (u useInfo) String() string {
	return u.Place.String() + " " + strconv.Itoa(u.Num)
}

type deferredInstr struct {
	index int
	instr *ir.Instr
}

// TODO: consider if there's any need for explicit Caller Interproc and Local regions
type state struct {
	AvailableRegs *stack
	// UsedRegs[ reg ] retuns the value stored in the register
	UsedRegs map[reg]value

	AvailableSpills *stack
	// UsedSpills[ spill ] retuns the value stored in the spill address
	UsedSpills map[spill]value

	AvailableCalleeInterproc *stack
	// UsedCalleeInterproc[ interproc ] retuns the value stored in the interproc address
	UsedCalleeInterproc map[calleeInterproc]value

	// LiveValues[ value ] retuns the register, interproc or spill storing this value
	LiveValues map[value]useInfo

	queuedInstr map[int][]*ir.Instr
	lastQueued  int

	// stores the index of the furthest use of each value
	valueUse map[value]int
	bb       *ir.BasicBlock
}

func newState(numRegs int, bb *ir.BasicBlock) *state {
	return &state{
		AvailableRegs: newStack(numRegs),
		UsedRegs:      map[reg]value{},

		AvailableSpills: newStack(16),
		UsedSpills:      map[spill]value{},

		LiveValues: map[value]useInfo{},
		valueUse:   map[value]int{},

		queuedInstr: map[int][]*ir.Instr{},
		lastQueued:  0,

		bb: bb,
	}
}

func (s *state) ExpectValue(v value) useInfo {
	info, ok := s.LiveValues[v]
	if !ok {
		//fmt.Println(s.String())
		panic("value not found: "+v.String())
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
	case CalleeInterProc:
		i := calleeInterproc(loc.Num)
		s.FreeInterProc(i)
	case Local:
		panic("i dont think this should happen")
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
	panic("freeing unused register: "+ fmt.Sprint(r))
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

func (s *state) FreeInterProc(i calleeInterproc) {
	_, ok := s.UsedCalleeInterproc[i]
	if ok {
		delete(s.UsedCalleeInterproc, i)
		s.AvailableCalleeInterproc.Push(int(i))
		return
	}
	panic("freeing unused interproc")
}

func (s *state) AllocReg(v value) reg {
	r := reg(s.AvailableRegs.Pop())
	s.UsedRegs[r] = v
	s.LiveValues[v] = useInfo{Place: Register, Num: int(r)}
	return r
}

func (s *state) FurthestUse() (reg, value) {
	biggestIndex := -1
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

func (s *state) Spill(r reg) spill {
	v, ok := s.UsedRegs[r]
	if !ok {
		sreg := strconv.Itoa(int(r))
		panic("spilling unused register: " + sreg)
	}
	s.FreeReg(r)
	a := spill(s.AvailableSpills.Pop())
	s.UsedSpills[a] = v
	s.LiveValues[v] = useInfo{Place: Spill, Num: int(a)}
	return a
}

// must preserve insertion order
func (s *state) QueueInstr(index int, instr *ir.Instr) {
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
		s := newState(numRegs, curr)
		findUses(s)
		allocBlock(s)
		top := s.AvailableSpills.Size()
		if int(top) > p.NumOfSpills {
			p.NumOfSpills = int(top)
		}
		insertQueuedInstrs(s, curr)
	}
}

func findUses(s *state) {
	for index, instr := range s.bb.Code {
		vals := getUsedValues(instr)
		for _, v := range vals {
			s.valueUse[v] = index
		}
	}
}

func getUsedValues(instr *ir.Instr) []value {
	output := []value{}
	for _, op := range instr.Operands {
		output = append(output, toValue(op))
	}
	for _, dest := range instr.Destination {
		output = append(output, toValue(dest))
	}
	return output
}

func insertQueuedInstrs(s *state, bb *ir.BasicBlock) {
	newBlock := make([]*ir.Instr, len(bb.Code)+s.lastQueued)
	offset := 0
	for i, oldInstr := range bb.Code {
		newBlock[i+offset] = oldInstr
		newInstrs, ok := s.queuedInstr[i]
		if ok {
			for _, newInstr := range newInstrs {
				offset += 1
				newBlock[i+offset] = newInstr
			}
		}
	}
	bb.Code = newBlock
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
			IT.Convert:
			allocUnary(s, instr, i)
		case IT.Copy:
			allocCopy(s, instr, i)
		case IT.LoadPtr:
			allocLoadPtr(s, instr, i)
		case IT.StorePtr:
			allocStorePtr(s, instr, i)
		case IT.Call:
			allocCall(s, instr, i)
		}
		//fmt.Println(instr, "\n")
	}
	storeLiveLocals(s)
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

// TODO: implement HIR -> MIR transformation for LoadPtr
func allocLoadPtr(s *state, instr *ir.Instr, index int) {
}

// TODO: implement HIR -> MIR transformation for StorePtr
func allocStorePtr(s *state, instr *ir.Instr, index int) {
}

// Combination of possible Copy instructions
// Notation is: hirc (mirc) -> hirc (mirc)
// 	temp (spill|reg|calleeInter) -> temp (reg)
// 	temp (spill|reg|calleeInter) -> local (reg|local)
// 	local (reg|local) -> local (reg|local)
// 	local (reg|local) -> temp (reg)
// 	global (static)   -> temp (reg)
// 	global (static)   -> local
// 	lit (lit)         -> temp
// 	lit (lit)         -> local
func allocCopy(s *state, instr *ir.Instr, index int) {
	a := instr.Operands[0]
	aAddr := isAddressable(s, a)
	instr.Operands[0] = toMirc(s, a)

	c := instr.Destination[0]
	// c.HirC can only be Temp or Local
	if c.HirC == hirc.Temp {
		c = allocReg(s, c, index)
	}
	cAddr := isAddressable(s, c)
	instr.Destination[0] = toMirc(s, c)

	if aAddr {
		if cAddr {
			// LOAD  a   -> reg
			// STORE reg -> c
			instr.T = IT.Load
			reg := allocReg(s, c, index)
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

// TODO: implement HIR -> MIR transformation for Call
func allocCall(s *state, instr *ir.Instr, index int) {
}

// if operand is Temp it must be allocated first
func isAddressable(s *state, o *ir.Operand) bool {
	switch o.HirC {
	case hirc.Temp:
		info := s.ExpectValue(toValue(o))
		return info.Place.IsAddressable()
	case hirc.Local:
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
	switch o.HirC {
	case hirc.Temp:
		panic("toMirc: temp is not alive")
	case hirc.Local:
		return newOp(o, mirc.Local, o.Num)
	case hirc.Global:
		return newOp(o, mirc.Static, o.Num)
	case hirc.Lit:
		return newOp(o, mirc.Lit, o.Num)
	}
	panic("toMirc: wtf")
}

func storeLiveLocals(s *state) {
	for val, info := range s.LiveValues {
		if val.T == hirc.Local {
			switch info.Place {
			case Register:
				r := reg(info.Num)
				restoreLocal(s, val, r, len(s.bb.Code)-1)
			case Spill, CalleeInterProc:
				panic("a local should never be in a spill or interproc")
			case Local:
				// do nothing
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
		if op.HirC == hirc.Local || op.HirC == hirc.Temp {
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
		case Local, CalleeInterProc, Spill:
			return loadLiveAddressable(s, op, info, index)
		}
		panic("ensureImmediate: Invalid StorageClass")
	}
	switch op.HirC {
	case hirc.Temp:
		return allocReg(s, op, index)
	case hirc.Local:
		return loadLocal(s, op, index)
	case hirc.Global:
		return newOp(op, mirc.Static, op.Num)
	case hirc.Lit:
		return newOp(op, mirc.Lit, op.Num)
	}
	panic("ensureImmediate: Invalid HIRClass")
}

func newOp(op *ir.Operand, m mirc.MIRClass, num int) *ir.Operand {
	return &ir.Operand{
		HirC:   op.HirC,
		MirC:   m,
		Type:   op.Type,
		Symbol: op.Symbol,
		Num:    num,
	}
}

func loadLiveAddressable(s *state, op *ir.Operand, info useInfo, index int) *ir.Operand {
	var newOp *ir.Operand
	switch info.Place {
	case Spill:
		s := spill(info.Num)
		newOp = newSpillOperand(s, op.Type)
	case CalleeInterProc:
		i := calleeInterproc(info.Num)
		newOp = newInterprocOperand(i, op.Type)
	case Local:
		newOp = newLocalOperand(op.Symbol)
	}

	rOp := allocReg(s, op, index)
	load := IRU.Load(newOp, rOp)
	s.QueueInstr(index-1, load)
	return rOp
}

func loadLocal(s *state, op *ir.Operand, index int) *ir.Operand {
	rOp := allocReg(s, op, index)
	newOp := newLocalOperand(op.Symbol)
	load := IRU.Load(newOp, rOp)
	s.QueueInstr(index-1, load)
	return rOp
}

func allocReg(s *state, op *ir.Operand, index int) *ir.Operand {
	if s.HasFreeRegs() {
		r := s.AllocReg(toValue(op))
		return newRegOp(r, op.Type)
	}
	return spillRegister(s, op, index)
}

func spillRegister(s *state, op *ir.Operand, index int) *ir.Operand {
	r, val := s.FurthestUse()
	if r == -1 {
		panic(s.String())
	}
	switch val.T {
	case hirc.Temp:
		spillTemp(s, r, op, index)
	case hirc.Local:
		restoreLocal(s, val, r, index-1)
		s.Free(val)
	case hirc.Lit, hirc.Global:
		panic("what the fuck are we even doing")
	}

	r2 := s.AllocReg(toValue(op))
	if r != r2 {
		panic("spillRegister: "+fmt.Sprintf("%v\n", s.AvailableRegs))
	}
	return newRegOp(r, op.Type)
}

func spillTemp(s *state, r reg, op *ir.Operand, index int) {
	sNum := s.Spill(r)
	spillOp := newSpillOperand(sNum, op.Type)
	regOp := newRegOp(r, op.Type)
	store := IRU.Store(regOp, spillOp)
	s.QueueInstr(index-1, store)
}

func restoreLocal(s *state, val value, r reg, index int) {
	reg := newRegOp(r, val.Symbol.Type)
	loc := newLocalOperand(val.Symbol)
	instr := IRU.Store(reg, loc)
	s.QueueInstr(index, instr)
}

func newRegOp(r reg, t T.Type) *ir.Operand {
	return &ir.Operand{
		MirC: mirc.Register,
		Num:  int(r),
		Type: t,
	}
}

func newSpillOperand(sNum spill, t T.Type) *ir.Operand {
	return &ir.Operand{
		MirC: mirc.Spill,
		Num:  int(sNum),
		Type: t,
	}
}

func newLocalOperand(sy *ir.Symbol) *ir.Operand {
	return &ir.Operand{
		MirC:   mirc.Local,
		Symbol: sy,
		Type:   sy.Type,
	}
}

func newInterprocOperand(i calleeInterproc, t T.Type) *ir.Operand {
	return &ir.Operand{
		MirC: mirc.CallerInterproc,
		Type: t,
		Num:  int(i),
	}
}

func freeIfNotNeeded(s *state, index int, v value) {
	useInfo, ok := s.LiveValues[v]
	if !ok {
		return // already freed (i hope)
	}
	if isNeeded(s, index, v, useInfo) {
		return
	}
	if v.T == hirc.Local {
		r := reg(useInfo.Num)
		restoreLocal(s, v, r, index)
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
		T:      op.HirC,
		Symbol: op.Symbol,
		Num:    op.Num,
	}
}
