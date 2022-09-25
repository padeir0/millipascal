package irchecker

import (
	"mpc/frontend/ir"
	"mpc/frontend/errors"
	eu "mpc/frontend/util/errors"
	ST "mpc/frontend/enums/symbolType"
	FT "mpc/frontend/enums/flowType"
	IT "mpc/frontend/enums/instrType"

	"strings"
)


func Check(M *ir.Module) *errors.CompilerError {
	s := newState(M)
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

type ValueStack struct {
	items []*ir.Operand
	top int
}

func (s *ValueStack) String() string {
	output := []string{}
	for _, item := range s.items[:s.top+1] {
		output = append(output, item.String())
	}
	return strings.Join(output, ", ")
}

func newStack(size int) *ValueStack {
	items := make([]*ir.Operand, size)
	return &ValueStack{
		items: items,
		top: size-1,
	}
}

func (s *ValueStack) HasItems() bool {
	return s.top >= 0
}

func (s *ValueStack) Push(i *ir.Operand) {
	s.top++
	if s.top >= len(s.items) {
		s.items = append(s.items, make([]*ir.Operand, 2)...)
	}
	s.items[s.top] = i
}

func (s *ValueStack) Pop() *ir.Operand{
	if s.top < 0 {
		return s.items[0]
	}
	item := s.items[s.top]
	s.top--
	return item
}

func (s *ValueStack) Size() int {
	return s.top
}

func (s *ValueStack) Top() *ir.Operand {
	if s.top < 0 {
		return nil
	}
	return s.items[s.top]
}

type state struct {
	m    *ir.Module
	proc *ir.Proc
	bb   *ir.BasicBlock

	Stack *ValueStack
}

func newState(M *ir.Module) *state {
	return &state{
		m: M,
		Stack: newStack(100),
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
	for _, ret := range s.proc.Rets {
		op := s.Stack.Top()
		if op == nil {
			return eu.NewInternalSemanticError("return stack is empty, expected returns: " + s.proc.Returns())
		}
		if op.Type != ret {
			return eu.NewInternalSemanticError("return of type "+ret.String()+" doesn't match value in stack: " + s.Stack.String())
		}
		s.Stack.Pop()
	}
	return nil
}

func checkInstr(s *state, instr *ir.Instr) *errors.CompilerError {
	panic("checkInstr unimplemented")
	switch instr.T {
		case IT.Add:
	}
	return nil
}
