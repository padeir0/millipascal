package regalloc

import (
	"mpc/frontend/ast"
	ST "mpc/frontend/enums/symbolType"
	OT "mpc/frontend/enums/operandType"
	IT "mpc/frontend/enums/instrType"

)

type value int
type reg int
type addr int

type stack struct {
	items []reg
	top int
}

func newStack(numRegs int) *stack {
	items := make([]reg, numRegs)
	for i := range items {
		items[i] = reg(numRegs-i-1)
	}
	return &stack{
		items: items,
		top: numRegs-1,
	}
}

func (s *stack) HasItems() bool {
	return s.top >= 0
}

func (s *stack) Push(r reg) {
	s.top++
	if s.top >= len(s.items) {
		s.items = append(s.items, make([]reg, 16)...)
	}
	s.items[s.top] = r
}

func (s *stack) Pop() reg {
	item := s.items[s.top]
	s.top--
	return item
}

/*
	Next[ reg ] returns the index of the next use of register
	Location[ value ] retuns the register storing this value
	Value[ reg ] retuns the value stored in the register
	Spills[ value ] retuns the address storing this value
*/
type state struct {
	Available *stack
	Next      map[reg]int
	Location  map[value]reg
	Value     map[reg]value

	Spills    map[value]addr
	NextSpill addr
}

func newState(numRegs int) *state {
	return &state{
		Available: newStack(numRegs),
		Next: map[reg]int{},
		Location: map[value]reg{},
		Value: map[reg]value{},

		Spills: map[value]addr{},
		NextSpill: 0,
	}
}

func (s *state) HasFreeRegs() bool {
	return s.Available.HasItems()
}

func (s *state) Free(r reg) {
	v := s.Value[r]
	delete(s.Value, r)
	delete(s.Location, v)
	delete(s.Next, r)
	s.Available.Push(r)
}

func (s *state) AllocReg(v value) reg {
	r := s.Available.Pop()
	s.Value[r] = v
	s.Location[v] = r
	return r
}

func (r *state) FurthestUse() reg {
	biggestDistance := 0
	bestReg := reg(0)
	for i, distance := range r.Next {
		if distance > biggestDistance {
			biggestDistance = distance
			bestReg = i
		}
	}
	return bestReg
}

func (s *state) Spill(r reg) addr {
	v := s.Value[r]
	s.Spills[v] = s.NextSpill
	sNum := s.NextSpill
	s.NextSpill++
	s.Free(r)
	return sNum
}

func Allocate(M *ast.Module, numRegs int) {
	for _, sy := range M.Globals {
		if sy.T == ST.Proc {
			allocProc(M, sy.Proc, numRegs)
		}
	}
}

func allocProc(M *ast.Module, p *ast.Proc, numRegs int) {
	var worklist = ast.FlattenGraph(p.Code)
	for _, curr := range worklist {
		s := newState(numRegs)
		allocBlock(s, curr)
	}
}

func allocBlock(s *state, bb *ast.BasicBlock) {
	for i, instr := range bb.Code {
		for _, op := range instr.Operands {
			if op.T == OT.Register {
				freeIfNotNeeded(s, bb, op, i)
			}
		}
		for _, op := range instr.Destination {
			if op.T == OT.Temp {
				allocTemp(s, bb, op, i)
			}
		}
	}
}

func allocTemp(s *state, bb *ast.BasicBlock, op *ast.Operand, index int) {
	if s.HasFreeRegs() {
		v := value(op.Num)
		r := s.AllocReg(v)
		op.T = OT.Register
		op.Num = int(r)
		return
	}
	spill(s, bb, op, index)
}

func spill(s *state, bb *ast.BasicBlock, op *ast.Operand, index int) {
	r := s.FurthestUse()
	sNum := s.Spill(r)

	begin := bb.Code[:index]
	end := bb.Code[index:]
	spillInstr := newSpillInstr(op, sNum)
	bb.Code = append(append(begin, spillInstr), end...)

	op.T = OT.Register
	op.Num = int(r)
	return
}

func newSpillInstr(op *ast.Operand, sNum addr) *ast.Instr {
	spillOp := &ast.Operand {
		T: OT.Spill,
		Num: int(sNum),
	}
	return &ast.Instr{
		T: IT.Spill,
		Operands: []*ast.Operand{op},
		Destination: []*ast.Operand{spillOp},
	}
}

func freeIfNotNeeded(s *state, bb *ast.BasicBlock, op *ast.Operand, index int) {
	if isNeeded(s, bb, op, index) {
		return
	}
	r := reg(op.Num)
	s.Free(r)
}

func isNeeded(s *state, bb *ast.BasicBlock, op *ast.Operand, index int) bool {
	if index + 1 >= len(bb.Code) {
		return false
	}
	r := reg(op.Num)
	if s.Next[r] > index {
		return true
	}
	for i, instr := range bb.Code[index+1:] {
		for _, op := range instr.Operands {
			if op.T == OT.Register && op.Num == int(r) {
				s.Next[r] = i+index+1
				return true
			}
		}
	}
	return false
}
