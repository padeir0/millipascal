package regalloc

import (
	ST "mpc/frontend/enums/symbolType"
	OT "mpc/frontend/enums/operandType"
	IT "mpc/frontend/enums/instrType"
	T "mpc/frontend/enums/Type"

	"mpc/frontend/ir"
	IRU "mpc/frontend/util/ir"

	"strconv"
	"strings"
	"fmt"
)

type value int
type reg int
type addr int

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

type state struct {
	AvailableRegs *stack
	// UsedRegs[ reg ] retuns the value stored in the register
	UsedRegs  map[reg]value

	AvailableAddr *stack
	// UsedAddr[ value ] retuns the value stored in the address
	UsedAddr  map[addr]value

	// LiveValues[ value ] retuns the register or address storing this value
	LiveValues  map[value]useInfo
	queuedInstr []deferredInstr
}

func newState(numRegs int) *state {
	return &state{
		AvailableRegs: newStack(numRegs),
		UsedRegs: map[reg]value{},

		AvailableAddr: newStack(16),
		UsedAddr: map[addr]value{},

		LiveValues: map[value]useInfo{},
	}
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

	if loc.IsReg {
		r := reg(loc.Num)
		s.FreeReg(r)
	} else {
		a := addr(loc.Num)
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

func (s *state) FreeAddr(a addr) {
	_, ok := s.UsedAddr[a]
	if ok {
		delete(s.UsedAddr, a)
		s.AvailableAddr.Push(int(a))
		return
	}
	panic("freeing unused addr")
}

func (s *state) AllocReg(v value) reg {
	r := reg(s.AvailableRegs.Pop())
	s.UsedRegs[r] = v
	s.LiveValues[v] = useInfo{IsReg: true, Num: int(r), NextUse: -1}
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

func (s *state) Spill(r reg) addr {
	v, ok := s.UsedRegs[r]
	if !ok {
		sreg := strconv.Itoa(int(r))
		panic("spilling unused register: " + sreg)
	}
	s.FreeReg(r)
	a := addr(s.AvailableAddr.Pop())
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
	for i, di := range s.queuedInstr {
		insertInstr(bb, di.index+i, di.instr)
	}
}

func allocBlock(s *state, bb *ir.BasicBlock) {
	for i, instr := range bb.Code {
		fmt.Print(instr, " >> ")
		for opIndex, op := range instr.Operands {
			switch op.T {
			case OT.Temp:
				instr.Operands[opIndex] = ensureTemp(s, bb, op, i)
				v := value(op.Num)
				freeIfNotNeeded(s, bb, v, i)
			case OT.Local:
				instr.Operands[opIndex] = ensureLocal(s, bb, op, i)
				v := value(op.Num)
				freeIfNotNeeded(s, bb, v, i)
			}
		}
		switch instr.T {
		case IT.Add,  IT.Sub,    IT.Mult,   IT.Div,
		     IT.Rem,  IT.Eq,     IT.Diff,   IT.Less,
		     IT.More, IT.LessEq, IT.MoreEq, IT.Or,
		     IT.And,  IT.Not:
			allocArith(s, bb, instr, i)
		}
		fmt.Println(instr)
	}
}

func allocArith(s *state, bb *ir.BasicBlock, instr *ir.Instr, i int) {
	dest := instr.Destination[0]
	if dest.T == OT.Temp {
		instr.Destination[0] = allocTemp(s, bb, dest, i)
	}
}

func ensureLocal(s *state, bb *ir.BasicBlock, op *ir.Operand, index int) *ir.Operand {
	panic("ensureLocal unimplemented")
}

func ensureTemp(s *state, bb *ir.BasicBlock, op *ir.Operand, index int) *ir.Operand {
	v := value(op.Num)
	loc, ok := s.LiveValues[v]
	if !ok {
		return allocTemp(s, bb, op, index)
	}
	if loc.IsReg {
		r := reg(loc.Num)
		return newRegOp(r, op.Type)
	}
	a := addr(loc.Num)
	return loadSpill(s, bb, op, index, a)
}

func loadSpill(s *state, bb *ir.BasicBlock, op *ir.Operand, index int, a addr) *ir.Operand {
	rOp := allocTemp(s, bb, op, index)
	spillOp := newSpillOperand(a, op.Type)
	load := IRU.Load(spillOp, rOp)
	queueInstr(s, index-1, load)
	return rOp
}

func allocTemp(s *state, bb *ir.BasicBlock, op *ir.Operand, index int) *ir.Operand {
	if s.HasFreeRegs() {
		v := value(op.Num)
		r := s.AllocReg(v)
		return newRegOp(r, op.Type)
	}
	return spillRegister(s, bb, op, index)
}

func spillRegister(s *state, bb *ir.BasicBlock, op *ir.Operand, index int) *ir.Operand {
	calcNextUse(s, bb, op, index)
	r := s.FurthestUse()
	sNum := s.Spill(r)
	spillOp := newSpillOperand(sNum, op.Type)
	regOp := newRegOp(r, op.Type)
	store := IRU.Store(regOp, spillOp)
	queueInstr(s, index-1, store)

	v := value(op.Num)
	r2 := s.AllocReg(v)
	if r != r2 {
		panic("something went wrong")
	}
	return newRegOp(r, op.Type)
}

func newRegOp(r reg, t T.Type) *ir.Operand {
	return &ir.Operand{
		T: OT.Register,
		Num: int(r),
		Type: t,
	}
}

func newSpillOperand(sNum addr, t T.Type) *ir.Operand {
	return &ir.Operand {
		T: OT.Spill,
		Num: int(sNum),
		Type: t,
	}
}

func calcNextUse(s *state, bb *ir.BasicBlock, opTemp *ir.Operand, index int) {
	for v := range s.LiveValues {
		isNeeded(s, bb, v, index)
	}
}

func freeIfNotNeeded(s *state, bb *ir.BasicBlock, v value, index int) {
	if isNeeded(s, bb, v, index) {
		return
	}
	s.Free(v)
}

func isNeeded(s *state, bb *ir.BasicBlock, v value, index int) bool {
	if index + 1 >= len(bb.Code) {
		return false
	}
	useInfo, ok := s.LiveValues[v]
	if !ok {
		panic("isNeeded: value not found")
	}
	if useInfo.NextUse != -1 &&
		useInfo.NextUse > index {
		return true
	}
	for i, instr := range bb.Code[index+1:] {
		for _, op := range instr.Operands {
			if op.T == OT.Temp && op.Num == int(v) {
				useInfo.NextUse = i+index+1
				s.LiveValues[v] = useInfo
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

func insertInstr(bb *ir.BasicBlock, index int, instr *ir.Instr) {
	begin := bb.Code[:index+1]
	end := bb.Code[index:]
	bb.Code = append(begin, end...)
	bb.Code[index+1] = instr
}
