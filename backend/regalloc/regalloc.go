package regalloc

import (
	ST "mpc/frontend/enums/symbolType"
	hirc "mpc/frontend/enums/HIRClass"
	mirc "mpc/frontend/enums/MIRClass"
	IT "mpc/frontend/enums/instrType"
	T "mpc/frontend/enums/Type"

	"mpc/frontend/ir"
	IRU "mpc/frontend/util/ir"

	"strconv"
	"strings"
	"fmt"
)

type reg int
type spill int

type stack struct {
	items []int
	top int
}

func (s *stack) String() string {
	output := []string{}
	for _, item := range s.items[:s.top+1] {
		output = append(output, strconv.Itoa(item))
	}
	return strings.Join(output, ", ")
}

func newStack(size int) *stack {
	items := make([]int, size)
	for i := range items {
		items[i] = size-i-1
	}
	return &stack{
		items: items,
		top: size-1,
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

type useInfo struct {
	IsReg bool
	Num int
	NextUse int
}

type deferredInstr struct {
	index int
	instr *ir.Instr
}

type Value struct {
	T mirc.MIRClass
	Num int
}

type state struct {
	AvailableRegs *stack
	// UsedRegs[ reg ] retuns the value stored in the register
	UsedRegs  map[reg]ir.Operand

	AvailableAddr *stack
	// UsedSpills[ spill ] retuns the value stored in the spill address
	UsedSpills  map[spill]ir.Operand

	// LiveValues[ value ] retuns the register or address storing this value
	LiveValues  map[ir.Operand]useInfo
	queuedInstr []deferredInstr

	bb *ir.BasicBlock
}

func newState(numRegs int) *state {
	return &state{
		AvailableRegs: newStack(numRegs),
		UsedRegs: map[reg]ir.Operand{},

		AvailableAddr: newStack(16),
		UsedSpills: map[spill]ir.Operand{},

		LiveValues: map[ir.Operand]useInfo{},
	}
}

func (s *state) HasFreeRegs() bool {
	return s.AvailableRegs.HasItems()
}

func (s *state) Free(v *ir.Operand) {
	loc, ok := s.LiveValues[*v]
	if !ok {
		panic("freeing unfound value")
	}
	delete(s.LiveValues, *v)

	if loc.IsReg {
		r := reg(loc.Num)
		s.FreeReg(r)
	} else {
		a := spill(loc.Num)
		s.FreeAddr(a)
	}
}

func (s *state) FreeReg(r reg) {
	_, ok := s.UsedRegs[r]
	if ok {
		delete(s.UsedRegs, r)
		s.AvailableRegs.Push(int(r))
		return
	}
	panic("freeing unused register")
}

func (s *state) FreeAddr(a spill) {
	_, ok := s.UsedSpills[a]
	if ok {
		delete(s.UsedSpills, a)
		s.AvailableAddr.Push(int(a))
		return
	}
	panic("freeing unused addr")
}

func (s *state) AllocReg(v *ir.Operand) reg {
	r := reg(s.AvailableRegs.Pop())
	s.UsedRegs[r] = *v
	s.LiveValues[*v] = useInfo{IsReg: true, Num: int(r), NextUse: -1}
	return r
}

func (r *state) FurthestUse() reg {
	biggestIndex := 0
	bestReg:= reg(-1)
	for _, info := range r.LiveValues {
		if info.IsReg && info.NextUse > biggestIndex {
			biggestIndex = info.NextUse
			bestReg = reg(info.Num)
		}
	}

	return bestReg
}

func (s *state) Spill(r reg) spill {
	v, ok := s.UsedRegs[r]
	if !ok {
		sreg := strconv.Itoa(int(r))
		panic("spilling unused register: " + sreg)
	}
	s.FreeReg(r)
	a := spill(s.AvailableAddr.Pop())
	s.LiveValues[v] = useInfo{IsReg: false, Num: int(a), NextUse: -1}
	return a
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
		s := newState(numRegs)
		allocBlock(s, curr)
		top := s.AvailableAddr.Size()
		if int(top) > p.NumOfSpills {
			p.NumOfSpills = int(top)
		}
		insertQueuedInstrs(s, curr)
	}
}

func insertQueuedInstrs(s *state, bb *ir.BasicBlock) {
	mapped := queuedToMap(s.queuedInstr)
	newBlock := make([]*ir.Instr, len(bb.Code) + len(s.queuedInstr))
	offset := 0
	for i, oldInstr := range bb.Code {
		newBlock[i+offset] = oldInstr
		newInstrs, ok := mapped[i]
		if ok {
			for _, newInstr := range newInstrs {
				offset += 1
				newBlock[i+offset] = newInstr
			}
		}
	}
}

func queuedToMap(queue []deferredInstr) map[int][]*ir.Instr {
	out := map[int][]*ir.Instr{}
	for _, defInstr := range queue {
		v, ok := out[defInstr.index]
		if ok {
			out[defInstr.index] = append(v, defInstr.instr)
		} else {
			out[defInstr.index] = []*ir.Instr{defInstr.instr}
		}
	}
	return out
}

func allocBlock(s *state, bb *ir.BasicBlock) {
	s.bb = bb
	for i, instr := range bb.Code {
		fmt.Print(instr, " >> ")
		switch instr.T {
		case IT.Add, IT.Sub, IT.Mult, IT.Div, IT.Rem:
			allocBinArith(s, instr, i)
		case IT.Eq, IT.Diff, IT.Less,
		     IT.More, IT.LessEq, IT.MoreEq:
			allocComp(s, instr, i)
		case IT.Or, IT.And:
			allocBinLogical(s, instr, i)
		case IT.Not:
			allocUnLogical(s, instr, i)
		case IT.UnaryMinus, IT.UnaryPlus:
			allocUnArith(s, instr, i)
		case IT.Convert:
			allocConvert(s, instr, i)
		case IT.Offset:
			allocOffset(s, instr, i)
		case IT.LoadPtr:
			allocLoadPtr(s, instr, i)
		case IT.StorePtr:
			allocStorePtr(s, instr, i)
		case IT.Store:
			allocStore(s, instr, i)
		case IT.Load:
			allocLoad(s, instr, i)
		case IT.Call:
			allocCall(s, instr, i)
		}
		fmt.Println(instr)
	}
}

func allocBinArith(s *state, instr *ir.Instr, index int) {
	a := instr.Operands[0]
	b := instr.Operands[1]
	c := instr.Destination[0]
	ensureOperands(s, instr, index, a, b)
	instr.Destination[0] = ensureImmediate(s, index, c)
}

func ensureOperands(s *state, instr *ir.Instr, index int, ops ...*ir.Operand) {
	for i, op := range ops {
		instr.Operands[i] = ensureImmediate(s, index, op)
	}
	for _, op := range ops {
		freeIfNotNeeded(s, index, op)
	}
}

func allocComp(s *state, instr *ir.Instr, index int) {
}

func allocBinLogical(s *state, instr *ir.Instr, index int) {
}

func allocUnLogical(s *state, instr *ir.Instr, index int) {
}

func allocUnArith(s *state, instr *ir.Instr, index int) {
}

func allocConvert(s *state, instr *ir.Instr, index int) {
}

func allocOffset(s *state, instr *ir.Instr, index int) {
}

func allocLoadPtr(s *state, instr *ir.Instr, index int) {
}

func allocStorePtr(s *state, instr *ir.Instr, index int) {
}

func allocStore(s *state, instr *ir.Instr, index int) {
}

func allocLoad(s *state, instr *ir.Instr, index int) {
}

func allocCall(s *state, instr *ir.Instr, index int) {
}


func allocArith(s *state, bb *ir.BasicBlock, instr *ir.Instr, i int) {
	dest := instr.Destination[0]
	if dest.HirC == hirc.Temp {
		instr.Destination[0] = allocTemp(s, dest, i)
	}
}

func ensureImmediate(s *state, index int, op *ir.Operand) *ir.Operand {
	switch op.HirC {
	case hirc.Temp:
		return ensureImmTemp(s, index, op)
	case hirc.Local:
		return ensureImmLocal(s, index, op)
	}
	panic("ensureImmediate: what")
}

func ensureImmLocal(s *state, index int, op *ir.Operand) *ir.Operand {
	panic("ensureLocal unimplemented")
}

func ensureImmTemp(s *state, index int, op *ir.Operand) *ir.Operand {
	loc, ok := s.LiveValues[*op]
	if !ok {
		return allocTemp(s, op, index)
	}
	if loc.IsReg {
		r := reg(loc.Num)
		return newRegOp(r, op.Type)
	}
	a := spill(loc.Num)
	return loadSpill(s, op, index, a)
}

func loadSpill(s *state, op *ir.Operand, index int, a spill) *ir.Operand {
	rOp := allocTemp(s, op, index)
	spillOp := newSpillOperand(a, op.Type)
	load := IRU.Load(spillOp, rOp)
	queueInstr(s, index-1, load)
	return rOp
}

func allocTemp(s *state, op *ir.Operand, index int) *ir.Operand {
	if s.HasFreeRegs() {
		r := s.AllocReg(op)
		return newRegOp(r, op.Type)
	}
	return spillRegister(s, op, index)
}

func spillRegister(s *state, op *ir.Operand, index int) *ir.Operand {
	calcNextUse(s, op, index)
	r := s.FurthestUse()
	sNum := s.Spill(r)
	spillOp := newSpillOperand(sNum, op.Type)
	regOp := newRegOp(r, op.Type)
	store := IRU.Store(regOp, spillOp)
	queueInstr(s, index-1, store)

	r2 := s.AllocReg(op)
	if r != r2 {
		panic("something went wrong")
	}
	return newRegOp(r, op.Type)
}

func newRegOp(r reg, t T.Type) *ir.Operand {
	return &ir.Operand{
		MirC: mirc.Register,
		Num: int(r),
		Type: t,
	}
}

func newSpillOperand(sNum spill, t T.Type) *ir.Operand {
	return &ir.Operand {
		MirC: mirc.Spill,
		Num: int(sNum),
		Type: t,
	}
}

func calcNextUse(s *state, opTemp *ir.Operand, index int) {
	for v := range s.LiveValues {
		isNeeded(s, index, &v)
	}
}

func freeIfNotNeeded(s *state, index int, op *ir.Operand) {
	if isNeeded(s, index, op) {
		return
	}
	s.Free(op)
}

func isNeeded(s *state, index int, v *ir.Operand) bool {
	if index + 1 >= len(s.bb.Code) {
		return false
	}
	useInfo, ok := s.LiveValues[*v]
	if !ok {
		panic("isNeeded: value not found")
	}
	if useInfo.NextUse != -1 &&
		useInfo.NextUse > index {
		return true
	}
	for i, instr := range s.bb.Code[index+1:] {
		for _, op := range instr.Operands {
			if op.HirC == hirc.Temp && op.Num == v.Num {
				useInfo.NextUse = i+index+1
				s.LiveValues[*v] = useInfo
				return true
			}
		}
	}
	return false
}

func queueInstr(s *state, index int, instr *ir.Instr) {
	di := deferredInstr{index: index, instr: instr}
	s.queuedInstr = append(s.queuedInstr, di)
}
