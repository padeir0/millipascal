package gen

import (
	"mpc/frontend/ast"
	T "mpc/frontend/enums/Type"
	lex "mpc/frontend/enums/lexType"
	OT "mpc/frontend/enums/operandType"
	IT "mpc/frontend/enums/instrType"
	ST "mpc/frontend/enums/symbolType"
	"mpc/frontend/errors"
	"strconv"
)

type context struct {
	Proc *ast.Proc

	CurrBlock *ast.BasicBlock

	LabelCounter int
	TempCounter  int
}

func newContext(proc *ast.Proc) *context {
	return &context{
		Proc:         proc,
		LabelCounter: 0,
		TempCounter:  0,
	}
}

func (c *context) NewBlock() *ast.BasicBlock {
	counter := strconv.Itoa(c.LabelCounter)
	b := &ast.BasicBlock{
		Label: ".L" + counter,
		Code:  []*ast.Instr{},
	}
	c.LabelCounter++
	return b
}

func (c *context) AllocTemp(t T.Type) *ast.Operand {
	op := &ast.Operand{
		T:   OT.Temp,
		Num: c.LabelCounter,
	}
	c.TempCounter++
	return op
}

func Generate(M *ast.Module) *errors.CompilerError {
	for _, sy := range M.Globals {
		switch sy.T {
		case ST.Proc:
			genProc(M, sy.Proc)
		case ST.Mem:
			genMem(M, sy.Mem)
		}
	}
	return nil
}

func genProc(M *ast.Module, proc *ast.Proc) *errors.CompilerError {
	c := newContext(proc)
	body := proc.N.Leaves[4]
	return genProcBody(M, c, body)
}

func genProcBody(M *ast.Module, c *context, body *ast.Node) *errors.CompilerError {
	for _, code := range body.Leaves {
		var err *errors.CompilerError
		switch code.Lex {
		case lex.IF:
			err = genIf(M, c, code)
		case lex.WHILE:
			err = genWhile(M, c, code)
		case lex.RETURN:
			err = genReturn(M, c, code)
		case lex.COPY:
			err = genCopy(M, c, code)
		case lex.SET:
			err = genSet(M, c, code)
		default:
			genExpr(M, c, code)
		}
		if err != nil {
			return err
		}
	}
	return nil
}

func genIf(M *ast.Module, c *context, body *ast.Node) *errors.CompilerError {
	return nil
}

func genWhile(M *ast.Module, c *context, body *ast.Node) *errors.CompilerError {
	return nil
}

func genReturn(M *ast.Module, c *context, body *ast.Node) *errors.CompilerError {
	return nil
}

func genCopy(M *ast.Module, c *context, body *ast.Node) *errors.CompilerError {
	return nil
}

func genSet(M *ast.Module, c *context, body *ast.Node) *errors.CompilerError {
	return nil
}

func genExpr(M *ast.Module, c *context, exp *ast.Node) *ast.Operand {
	switch exp.Lex {
	case lex.IDENTIFIER:
		return genExprID(M, c, exp)
	case lex.INT, lex.FALSE, lex.TRUE, lex.CHAR:
		return genLit(M, c, exp)
	case lex.PLUS, lex.MINUS:
		return genPlusMinus(M, c, exp)
	case lex.MULTIPLICATION, lex.DIVISION, lex.REMAINDER,
		lex.EQUALS, lex.DIFFERENT,
		lex.MORE, lex.MOREEQ, lex.LESS, lex.LESSEQ,
		lex.AND, lex.OR:
		return genBinaryOp(M, c, exp)
	case lex.COLON:
		return genConversion(M, c, exp)
	case lex.CALL:
		return genCall(M, c, exp)
	case lex.LEFTBRACKET:
		return genMemAccess(M, c, exp)
	case lex.NOT:
		return genUnaryOp(M, c, exp)
	}
	return nil
}

func genCall(M *ast.Module, c *context, id *ast.Node) *ast.Operand {
	return nil
}

func genMemAccess(M *ast.Module, c *context, id *ast.Node) *ast.Operand {
	return nil
}

func genExprID(M *ast.Module, c *context, id *ast.Node) *ast.Operand {
	_, ok := c.Proc.Names[id.Text]
	if ok {
		return &ast.Operand{
			T:     OT.Local,
			Label: id.Text,
		}
	}
	global, ok := M.Globals[id.Text]
	if !ok {
		panic("genExprID: global not found")
	}
	return globalToOperand(id, global)
}

func globalToOperand(id *ast.Node, global *ast.Symbol) *ast.Operand {
	switch global.T {
	case ST.Proc:
		return &ast.Operand{
			T:     OT.Proc,
			Label: id.Text,
		}
	case ST.Mem:
		return &ast.Operand{
			T:     OT.Mem,
			Label: id.Text,
		}
	}
	// Const
	return &ast.Operand{
		T:     OT.Lit,
		Label: global.N.Leaves[1].Text, // zero fucks
	}
}

func genConversion(M *ast.Module, c *context, colon *ast.Node) *ast.Operand {
	it := IT.Convert
	a := genExpr(M, c, colon.Leaves[0])
	dest := c.AllocTemp(colon.T)
	instr := &ast.Instr{
		T: it,
		Type: colon.T,
		Operands: []*ast.Operand{a},
		Destination: []*ast.Operand{dest},
	}
	c.CurrBlock.AddInstr(instr)
	return dest
}

func genLit(M *ast.Module, c *context, lit *ast.Node) *ast.Operand {
	return &ast.Operand{
		T:     OT.Lit,
		Label: lit.Text,
	}
}

func genPlusMinus(M *ast.Module, c *context, op *ast.Node) *ast.Operand {
	if len(op.Leaves) == 2 {
		return genBinaryOp(M, c, op)
	}
	return genUnaryOp(M, c, op)
}

func genBinaryOp(M *ast.Module, c *context, op *ast.Node) *ast.Operand {
	it := lexToBinaryOp(op.Lex)
	a := genExpr(M, c, op.Leaves[0])
	b := genExpr(M, c, op.Leaves[1])
	dest := c.AllocTemp(op.T)
	instr := &ast.Instr{
		T: it,
		Type: op.T,
		Operands: []*ast.Operand{a, b},
		Destination: []*ast.Operand{dest},
	}
	c.CurrBlock.AddInstr(instr)
	return dest
}

func lexToBinaryOp(op lex.TkType) IT.InstrType{
	switch op {
	case lex.MULTIPLICATION:
		return IT.Mult
	case lex.DIVISION:
		return IT.Div
	case lex.REMAINDER:
		return IT.Rem
	case lex.EQUALS:
		return IT.Eq
	case lex.DIFFERENT:
		return IT.Diff
	case lex.MORE:
		return IT.More
	case lex.MOREEQ:
		return IT.MoreEq
	case lex.LESS:
		return IT.Less
	case lex.LESSEQ:
		return IT.LessEq
	case lex.AND:
		return IT.And
	case lex.OR:
		return IT.Or
	}
	panic("lexToBinaryOp: unexpected binOp")
}

func genUnaryOp(M *ast.Module, c *context, op *ast.Node) *ast.Operand {
	it := lexToUnaryOp(op.Lex)
	a := genExpr(M, c, op.Leaves[0])
	dest := c.AllocTemp(op.T)
	instr := &ast.Instr{
		T: it,
		Type: op.T,
		Operands: []*ast.Operand{a},
		Destination: []*ast.Operand{dest},
	}
	c.CurrBlock.AddInstr(instr)
	return dest
}

func lexToUnaryOp(op lex.TkType) IT.InstrType{
	switch op {
	case lex.MINUS:
		return IT.UnaryMinus
	case lex.PLUS:
		return IT.UnaryPlus
	case lex.NOT:
		return IT.Not
	}
	panic("lexToUnaryOp: unexpected binOp")
}

func genMem(M *ast.Module, mem *ast.Mem) *errors.CompilerError {
	return nil
}
