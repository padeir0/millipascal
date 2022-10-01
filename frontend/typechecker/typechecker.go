package typechecker

import (
	"mpc/frontend/ir"
	T "mpc/frontend/enums/Type"
	lex "mpc/frontend/enums/lexType"
	ST "mpc/frontend/enums/symbolType"
	"mpc/frontend/errors"
	msg "mpc/frontend/messages"
)

func Check(M *ir.Module) *errors.CompilerError {
	for _, sy := range M.Globals {
		err := checkSymbol(M, sy)
		if err != nil {
			return err
		}
	}
	for _, sy := range M.Globals {
		if sy.T == ST.Proc {
			err := checkBlock(M, sy.Proc, sy.N.Leaves[4])
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func checkSymbol(M *ir.Module, sy *ir.Symbol) *errors.CompilerError {
	var err *errors.CompilerError
	switch sy.T {
	case ST.Proc:
		err = checkProc(M, sy.Proc)
	}
	return err
}

func checkProc(M *ir.Module, proc *ir.Proc) *errors.CompilerError {
	nArgs := proc.N.Leaves[1]
	nRets := proc.N.Leaves[2]
	nVars := proc.N.Leaves[3]

	if nArgs != nil {
		err := checkProcArgs(M, proc, nArgs)
		if err != nil {
			return err
		}
	}

	if nRets != nil {
		rets := getProcRets(M, nRets)
		proc.Rets = rets
	}

	if nVars != nil {
		err := checkProcVars(M, proc, nVars)
		if err != nil {
			return err
		}
	}
	return nil
}

func getProcRets(M *ir.Module, n *ir.Node) []T.Type {
	types := []T.Type{}
	for _, tNode := range n.Leaves {
		t := getType(tNode)
		types = append(types, t)
		tNode.T = t
	}
	return types
}

func checkProcArgs(M *ir.Module, proc *ir.Proc, n *ir.Node) *errors.CompilerError {
	for _, decl := range n.Leaves {
		var d *ir.Symbol
		if len(decl.Leaves) == 0 {
			d = &ir.Symbol{
				Name: decl.Text,
				N:    decl,
				Type: T.I64,
			}
		} else if len(decl.Leaves) == 2 {
			d = &ir.Symbol{
				Name: decl.Leaves[0].Text,
				N:    decl,
				Type: getType(decl.Leaves[1]),
			}
		}
		err := verifyIfDefined(M, proc, d)
		if err != nil {
			return err
		}
		proc.ArgMap[d.Name] = d
		proc.Args = append(proc.Args, d)
		decl.T = d.Type
	}
	return nil
}

func checkProcVars(M *ir.Module, proc *ir.Proc, n *ir.Node) *errors.CompilerError {
	for _, decl := range n.Leaves {
		var d *ir.Symbol
		if len(decl.Leaves) == 0 {
			d = &ir.Symbol{
				Name: decl.Text,
				N:    decl,
				Type: T.I64,
			}
		} else if len(decl.Leaves) == 2 {
			d = &ir.Symbol{
				Name: decl.Leaves[0].Text,
				N:    decl,
				Type: getType(decl.Leaves[1]),
			}
		}
		err := verifyIfDefined(M, proc, d)
		if err != nil {
			return err
		}
		proc.Vars[d.Name] = d
		decl.T = d.Type
	}
	return nil
}

func verifyIfDefined(M *ir.Module, proc *ir.Proc, d *ir.Symbol) *errors.CompilerError {
	l := getVarOrArg(proc, d.Name)
	if l != nil {
		return msg.ErrorNameAlreadyDefined(M, d.N, l.N)
	}
	return nil
}

func getType(n *ir.Node) T.Type {
	switch n.Lex {
	case lex.I8:
		return T.I8
	case lex.I16:
		return T.I16
	case lex.I32:
		return T.I32
	case lex.I64:
		return T.I64
	case lex.PTR:
		return T.Ptr
	case lex.BOOL:
		return T.Bool
	}
	panic("getType: what: " + ir.FmtNode(n))
}

func checkBlock(M *ir.Module, proc *ir.Proc, n *ir.Node) *errors.CompilerError {
	for _, code := range n.Leaves {
		err := checkStatement(M, proc, code)
		if err != nil {
			return err
		}
	}
	return nil
}

func checkStatement(M *ir.Module, proc *ir.Proc, n *ir.Node) *errors.CompilerError {
	switch n.Lex {
	case lex.EOF:
		return nil
	case lex.IF:
		return checkIf(M, proc, n)
	case lex.WHILE:
		return checkWhile(M, proc, n)
	case lex.RETURN:
		return checkReturn(M, proc, n)
	case lex.SET:
		return checkAssignment(M, proc, n)
	default:
		return checkExpr(M, proc, n)
	}
}

func checkIf(M *ir.Module, proc *ir.Proc, n *ir.Node) *errors.CompilerError {
	exp := n.Leaves[0]
	block := n.Leaves[1]
	elseifchain := n.Leaves[2]
	else_ := n.Leaves[3]

	err := checkExpr(M, proc, exp)
	if err != nil {
		return err
	}

	err = checkExprType(M, exp)
	if err != nil {
		return err
	}

	err = checkBlock(M, proc, block)
	if err != nil {
		return err
	}

	if elseifchain != nil {
		err = checkElseIfChain(M, proc, elseifchain)
		if err != nil {
			return err
		}
	}

	if else_ != nil {
		err = checkElse(M, proc, else_)
		if err != nil {
			return err
		}
	}

	return nil
}

func checkElse(M *ir.Module, proc *ir.Proc, n *ir.Node) *errors.CompilerError {
	err := checkBlock(M, proc, n.Leaves[0])
	if err != nil {
		return err
	}
	return nil
}

func checkElseIfChain(M *ir.Module, proc *ir.Proc, n *ir.Node) *errors.CompilerError {
	for _, elseif := range n.Leaves {
		err := checkElseIf(M, proc, elseif)
		if err != nil {
			return err
		}
	}
	return nil
}

func checkElseIf(M *ir.Module, proc *ir.Proc, n *ir.Node) *errors.CompilerError {
	err := checkExpr(M, proc, n.Leaves[0])
	if err != nil {
		return err
	}

	err = checkExprType(M, n.Leaves[0])
	if err != nil {
		return err
	}

	err = checkBlock(M, proc, n.Leaves[1])
	if err != nil {
		return err
	}
	return nil
}

func checkWhile(M *ir.Module, proc *ir.Proc, n *ir.Node) *errors.CompilerError {
	err := checkExpr(M, proc, n.Leaves[0])
	if err != nil {
		return err
	}

	err = checkExprType(M, n.Leaves[0])
	if err != nil {
		return err
	}

	err = checkBlock(M, proc, n.Leaves[1])
	if err != nil {
		return err
	}
	return nil
}

func checkReturn(M *ir.Module, proc *ir.Proc, n *ir.Node) *errors.CompilerError {
	for i, ret := range n.Leaves {
		if i >= len(proc.Rets) {
			return msg.ErrorInvalidNumberOfReturns(M, proc, ret)
		}
		err := checkExpr(M, proc, ret)
		if err != nil {
			return err
		}
		if proc.Rets[i] != ret.T {
			return msg.ErrorUnmatchingReturns(M, proc, ret, i)
		}
	}
	return nil
}

func checkAssignment(M *ir.Module, proc *ir.Proc, n *ir.Node) *errors.CompilerError {
	left := n.Leaves[0]
	right := n.Leaves[1]

	err := checkAssignees(M, proc, left)
	if err != nil {
		return err
	}

	err = checkExprList(M, proc, right)
	if err != nil {
		return err
	}

	if len(right.Leaves) == 1 &&
		right.Leaves[0].T == T.MultiRet {
		err := checkMultiAssignment(M, left, right.Leaves[0])
		if err != nil {
			return err
		}
	} else if len(right.Leaves) != len(left.Leaves) {
		return msg.ErrorMismatchedAssignment(M, n)
	} else {
		for i, assignee := range left.Leaves {
			if assignee.T != right.Leaves[i].T {
				return msg.ErrorMismatchedTypesInAssignment(M, assignee, right.Leaves[i])
			}
		}
	}

	return nil
}

func checkExprList(M *ir.Module, proc *ir.Proc, n *ir.Node) *errors.CompilerError {
	for _, exp := range n.Leaves {
		err := checkExpr(M, proc, exp)
		if err != nil {
			return err
		}
		if exp.T == T.Void {
			return msg.ErrorCannotUseVoid(M, exp.Leaves[1])
		}
	}
	return nil
}

func checkAssignees(M *ir.Module, proc *ir.Proc, left *ir.Node) *errors.CompilerError {
	for _, assignee := range left.Leaves {
		switch assignee.Lex {
		case lex.IDENTIFIER:
			err := checkIdAssignee(M, proc, assignee)
			if err != nil {
				return err
			}
		case lex.LEFTBRACKET:
			err := checkMemAccessAssignee(M, proc, assignee)
			if err != nil {
				return err
			}
		default:
			return msg.ErrorNotAssignable(M, assignee)
		}
	}
	return nil
}

func checkIdAssignee(M *ir.Module, proc *ir.Proc, assignee *ir.Node) *errors.CompilerError {
	d := getVarOrArg(proc, assignee.Text)
	if d != nil {
		assignee.T = d.Type
		return nil
	}
	global, ok := M.Globals[assignee.Text]
	if ok {
		return msg.ErrorCannotAssignGlobal(M, global, assignee)
	}
	return msg.ErrorNameNotDefined(M, assignee)
}

func getVarOrArg(proc *ir.Proc, name string) *ir.Symbol {
	def, ok := proc.ArgMap[name]
	if ok {
		return def
	}
	def, ok = proc.Vars[name]
	if ok {
		return def
	}
	return nil
}

func checkMultiAssignment(M *ir.Module, left *ir.Node, n *ir.Node) *errors.CompilerError {
	procName := n.Leaves[1].Text
	proc := M.Globals[procName]
	if len(proc.Proc.Rets) != len(left.Leaves) {
		return msg.ErrorMismatchedMultiRetAssignment(M, proc, n.Leaves[1], left)
	}
	for i, assignee := range left.Leaves {
		if assignee.T != proc.Proc.Rets[i] {
			return msg.ErrorMismatchedTypesInMultiAssignment(M, proc, left, i)
		}
	}
	return nil
}

func checkExpr(M *ir.Module, proc *ir.Proc, n *ir.Node) *errors.CompilerError {
	switch n.Lex {
	case lex.IDENTIFIER:
		return checkExprID(M, proc, n)
	case lex.INT, lex.FALSE, lex.TRUE, lex.SYSCALL:
		n.T = termToType(n.Lex)
		return nil
	case lex.PLUS, lex.MINUS:
		return plusMinus(M, proc, n)
	case lex.MULTIPLICATION, lex.DIVISION, lex.REMAINDER:
		return binaryOp(M, proc, n, number, outSame)
	case lex.EQUALS, lex.DIFFERENT,
		lex.MORE, lex.MOREEQ, lex.LESS, lex.LESSEQ:
		return binaryOp(M, proc, n, any, outBool)
	case lex.AND, lex.OR:
		return binaryOp(M, proc, n, _bool, outBool)
	case lex.COLON:
		err := checkExpr(M, proc, n.Leaves[1])
		if err != nil {
			return err
		}
		n.T = getType(n.Leaves[0])
	case lex.CALL:
		return checkCall(M, proc, n)
	case lex.LEFTBRACKET:
		return checkMemAccess(M, proc, n)
	case lex.NOT:
		return unaryOp(M, proc, n, _bool, outBool)
	}
	return nil
}

func checkMemAccess(M *ir.Module, proc *ir.Proc, n *ir.Node) *errors.CompilerError {
	tp := n.Leaves[1]
	mem := n.Leaves[2]
	local := getVarOrArg(proc, mem.Text)
	if local != nil {
		return msg.ErrorExpectedMemGotLocal(M, local, mem)
	}
	global, ok := M.Globals[mem.Text]
	if !ok {
		return msg.ErrorNameNotDefined(M, mem)
	}
	if global.T != ST.Mem {
		return msg.ErrorExpectedMem(M, global, mem)
	}
	err := checkExpr(M, proc, n.Leaves[0])
	if err != nil {
		return err
	}
	if tp == nil {
		n.T = T.I8
	} else {
		n.T = getType(tp)
	}
	return nil
}

func checkCall(M *ir.Module, proc *ir.Proc, n *ir.Node) *errors.CompilerError {
	callee := n.Leaves[1]
	local := getVarOrArg(proc, callee.Text)
	if local != nil {
		return msg.ErrorExpectedProcedureGotLocal(M, local, callee)
	}
	global, ok := M.Globals[callee.Text]
	if ok {
		if global.T != ST.Proc {
			return msg.ErrorExpectedProcedure(M, global, callee)
		}
		return checkCallProc(M, proc, global.Proc, n)
	}
	return msg.ErrorNameNotDefined(M, callee)
}

func checkCallProc(M *ir.Module, proc, callee *ir.Proc, n *ir.Node) *errors.CompilerError {
	exprs := n.Leaves[0]
	if len(exprs.Leaves) != len(callee.Args) {
		return msg.ErrorInvalidNumberOfArgs(M, callee, n)
	}
	for i, param := range exprs.Leaves {
		err := checkExpr(M, proc, param)
		if err != nil {
			return err
		}
		if param.T != callee.Args[i].Type {
			return msg.ErrorMismatchedTypeForArgument(M, param, callee.Args[i])
		}
	}
	if len(callee.Rets) == 1 {
		n.T = callee.Rets[0]
	} else if len(callee.Rets) == 0 {
		n.T = T.Void
	} else {
		n.T = T.MultiRet
	}
	return nil
}

func checkExprID(M *ir.Module, proc *ir.Proc, n *ir.Node) *errors.CompilerError {
	local := getVarOrArg(proc, n.Text)
	if local != nil {
		n.T = local.Type
		return nil
	}
	return msg.ErrorNameNotDefined(M, n)
}

func termToType(tp lex.TkType) T.Type {
	switch tp {
	case lex.INT:
		return T.I64
	case lex.TRUE:
		return T.Bool
	case lex.FALSE:
		return T.Bool
	case lex.SYSCALL:
		return T.Syscall
	}
	return T.Invalid
}

func plusMinus(M *ir.Module, proc *ir.Proc, n *ir.Node) *errors.CompilerError {
	if len(n.Leaves) == 1 {
		return unaryOp(M, proc, n, number, outSame)
	}
	return binaryOp(M, proc, n, number, outSame)
}

type deriver func(types ...T.Type) T.Type

func outSame(a ...T.Type) T.Type {
	// a homogeneous, all items must be of the same type
	return a[0]
}

func outBool(a ...T.Type) T.Type {
	return T.Bool
}

type class struct {
	Description string
	Checker func(t T.Type) bool
}

var any = class{
	Description: "i8, i16, i32, i64, bool or ptr",
	Checker: T.IsAny,
}

var _bool = class{
	Description: "bool",
	Checker: T.IsBool,
}

var number = class{
	Description: "i8, i16, i32 or i64",
	Checker: T.IsNumber,
}  
var ptr = class {
	Description: "ptr",
	Checker: T.IsPtr,
}

// a op b where type(a) = type(b) and type(a op b) = deriver(type(a), type(b))
// and both type(a), type(b) is of the class specified
func binaryOp(M *ir.Module, proc *ir.Proc, op *ir.Node, c class, der deriver) *errors.CompilerError {
	if len(op.Leaves) != 2 {
		panic(M.Name + ": internal error, binary operator should have two leaves")
	}
	left := op.Leaves[0]
	err := checkExpr(M, proc, left)
	if err != nil {
		return err
	}
	right := op.Leaves[1]
	err = checkExpr(M, proc, right)
	if err != nil {
		return err
	}

	err = checkExprType(M, left)
	if err != nil {
		return err
	}
	err = checkExprType(M, right)
	if err != nil {
		return err
	}

	if !c.Checker(left.T) {
		return msg.ErrorInvalidClassForExpr(M, left, c.Description)
	}

	if !c.Checker(right.T) {
		return msg.ErrorInvalidClassForExpr(M, right, c.Description)
	}

	if left.T != right.T {
		return msg.ErrorOperationBetweenUnequalTypes(M, op)
	}

	op.T = der(left.T, right.T)
	return nil
}

func checkExprType(M *ir.Module, n *ir.Node) *errors.CompilerError {
	if n.T == T.MultiRet {
		return msg.ErrorCannotUseMultipleValuesInExpr(M, n)
	}
	if n.T == T.Syscall {
		return msg.ErrorCannotUseSyscallAsValue(M, n)
	}
	if n.T == T.Void {
		return msg.ErrorCannotUseVoid(M, n)
	}
	if n.T == T.Invalid {
		return msg.ErrorInvalidType(M, n)
	}
	return nil
}

// op a where type(op a) = deriver(type(a))
// and type(a) is of the class specified
func unaryOp(M *ir.Module, proc *ir.Proc, op *ir.Node, c class, der deriver) *errors.CompilerError {
	if len(op.Leaves) != 1 {
		panic(M.Name + ": internal error, unary operator should have one leaf")
	}
	operand := op.Leaves[0]
	err := checkExpr(M, proc, operand)
	if err != nil {
		return err
	}
	err = checkExprType(M, operand)
	if err != nil {
		return err
	}

	if !c.Checker(operand.T) {
		return msg.ErrorInvalidClassForExpr(M, operand, c.Description)
	}

	op.T = der(operand.T)
	return nil
}

func checkMemAccessAssignee(M *ir.Module, proc *ir.Proc, n *ir.Node) *errors.CompilerError {
	id := n.Leaves[0]
	if id.Lex != lex.IDENTIFIER {
		return msg.ErrorBadIndex(M, id)
	}

	local := getVarOrArg(proc, id.Text)
	if local != nil {
		return msg.ErrorCannotIndexLocal(M, local, n)
	}

	global, ok := M.Globals[id.Text]
	if !ok {
		return msg.ErrorNameNotDefined(M, n)
	}

	if global.T != ST.Mem {
		return msg.ErrorCanOnlyIndexMemory(M, global, n)
	}

	err := checkExpr(M, proc, n.Leaves[1])
	if err != nil {
		return err
	}

	n.T = T.I8
	return nil
}
