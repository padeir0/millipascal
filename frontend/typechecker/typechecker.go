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
			err := checkBlock(M, sy, sy.N.Leaves[4])
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
		err = checkProc(M, sy)
	}
	return err
}

func checkProc(M *ir.Module, sy *ir.Symbol) *errors.CompilerError {
	nArgs := sy.N.Leaves[1]
	nRets := sy.N.Leaves[2]
	nVars := sy.N.Leaves[3]

	if nArgs != nil {
		args, err := checkProcDecls(M, sy, nArgs)
		if err != nil {
			return err
		}
		sy.Proc.Args = args
	}

	if nRets != nil {
		rets, err := checkProcRets(M, sy, nRets)
		if err != nil {
			return err
		}
		sy.Proc.Rets = rets
	}

	if nVars != nil {
		vars, err := checkProcDecls(M, sy, nVars)
		if err != nil {
			return err
		}
		sy.Proc.Vars = vars
	}
	return nil
}

func checkProcRets(M *ir.Module, sy *ir.Symbol, n *ir.Node) ([]T.Type, *errors.CompilerError) {
	types := []T.Type{}
	for _, tNode := range n.Leaves {
		t := getType(tNode)
		types = append(types, t)
		tNode.T = t
	}
	return types, nil
}

func checkProcDecls(M *ir.Module, sy *ir.Symbol, n *ir.Node) ([]*ir.Decl, *errors.CompilerError) {
	decls := []*ir.Decl{}
	for _, decl := range n.Leaves {
		var d *ir.Decl
		if len(decl.Leaves) == 0 {
			d = &ir.Decl{
				Name: decl.Text,
				N:    decl,
				Type: T.I64,
			}
		} else if len(decl.Leaves) == 2 {
			d = &ir.Decl{
				Name: decl.Leaves[0].Text,
				N:    decl,
				Type: getType(decl.Leaves[1]),
			}
		}
		v, ok := sy.Proc.Names[d.Name]
		if ok {
			return nil, msg.ErrorNameAlreadyDefined(M, decl, v.N)
		}
		sy.Proc.Names[d.Name] = d
		decls = append(decls, d)
		decl.T = d.Type
	}
	return decls, nil
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

func checkBlock(M *ir.Module, sy *ir.Symbol, n *ir.Node) *errors.CompilerError {
	for _, code := range n.Leaves {
		err := checkStatement(M, sy, code)
		if err != nil {
			return err
		}
	}
	return nil
}

func checkStatement(M *ir.Module, sy *ir.Symbol, n *ir.Node) *errors.CompilerError {
	switch n.Lex {
	case lex.EOF:
		return nil
	case lex.IF:
		return checkIf(M, sy, n)
	case lex.WHILE:
		return checkWhile(M, sy, n)
	case lex.RETURN:
		return checkReturn(M, sy, n)
	case lex.SET:
		return checkAssignment(M, sy, n)
	default:
		return checkExpr(M, sy, n)
	}
}

func checkIf(M *ir.Module, sy *ir.Symbol, n *ir.Node) *errors.CompilerError {
	exp := n.Leaves[0]
	block := n.Leaves[1]
	elseifchain := n.Leaves[2]
	else_ := n.Leaves[3]

	err := checkExpr(M, sy, exp)
	if err != nil {
		return err
	}

	err = checkExprType(M, exp)
	if err != nil {
		return err
	}

	err = checkBlock(M, sy, block)
	if err != nil {
		return err
	}

	if elseifchain != nil {
		err = checkElseIfChain(M, sy, elseifchain)
		if err != nil {
			return err
		}
	}

	if else_ != nil {
		err = checkElse(M, sy, else_)
		if err != nil {
			return err
		}
	}

	return nil
}

func checkElse(M *ir.Module, sy *ir.Symbol, n *ir.Node) *errors.CompilerError {
	err := checkBlock(M, sy, n.Leaves[0])
	if err != nil {
		return err
	}
	return nil
}

func checkElseIfChain(M *ir.Module, sy *ir.Symbol, n *ir.Node) *errors.CompilerError {
	for _, elseif := range n.Leaves {
		err := checkElseIf(M, sy, elseif)
		if err != nil {
			return err
		}
	}
	return nil
}

func checkElseIf(M *ir.Module, sy *ir.Symbol, n *ir.Node) *errors.CompilerError {
	err := checkExpr(M, sy, n.Leaves[0])
	if err != nil {
		return err
	}

	err = checkExprType(M, n.Leaves[0])
	if err != nil {
		return err
	}

	err = checkBlock(M, sy, n.Leaves[1])
	if err != nil {
		return err
	}
	return nil
}

func checkWhile(M *ir.Module, sy *ir.Symbol, n *ir.Node) *errors.CompilerError {
	err := checkExpr(M, sy, n.Leaves[0])
	if err != nil {
		return err
	}

	err = checkExprType(M, n.Leaves[0])
	if err != nil {
		return err
	}

	err = checkBlock(M, sy, n.Leaves[1])
	if err != nil {
		return err
	}
	return nil
}

func checkReturn(M *ir.Module, sy *ir.Symbol, n *ir.Node) *errors.CompilerError {
	for i, ret := range n.Leaves {
		if i >= len(sy.Proc.Rets) {
			return msg.ErrorInvalidNumberOfReturns(M, sy, ret)
		}
		err := checkExpr(M, sy, ret)
		if err != nil {
			return err
		}
		if sy.Proc.Rets[i] != ret.T {
			return msg.ErrorUnmatchingReturns(M, sy, ret, i)
		}
	}
	return nil
}

func checkAssignment(M *ir.Module, sy *ir.Symbol, n *ir.Node) *errors.CompilerError {
	left := n.Leaves[0]
	right := n.Leaves[1]

	err := checkAssignees(M, sy, left)
	if err != nil {
		return err
	}

	err = checkExprList(M, sy, right)
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

func checkExprList(M *ir.Module, sy *ir.Symbol, n *ir.Node) *errors.CompilerError {
	for _, exp := range n.Leaves {
		err := checkExpr(M, sy, exp)
		if err != nil {
			return err
		}
		if exp.T == T.Void {
			return msg.ErrorCannotUseVoid(M, exp.Leaves[1])
		}
	}
	return nil
}

func checkAssignees(M *ir.Module, sy *ir.Symbol, left *ir.Node) *errors.CompilerError {
	for _, assignee := range left.Leaves {
		switch assignee.Lex {
		case lex.IDENTIFIER:
			err := checkIdAssignee(M, sy, assignee)
			if err != nil {
				return err
			}
		case lex.LEFTBRACKET:
			err := checkMemAccessAssignee(M, sy, assignee)
			if err != nil {
				return err
			}
		default:
			return msg.ErrorNotAssignable(M, assignee)
		}
	}
	return nil
}

func checkIdAssignee(M *ir.Module, sy *ir.Symbol, assignee *ir.Node) *errors.CompilerError {
	local, ok := sy.Proc.Names[assignee.Text]
	if ok {
		assignee.T = local.Type
		return nil
	}
	global, ok := M.Globals[assignee.Text]
	if ok {
		return msg.ErrorCannotAssignGlobal(M, global, assignee)
	}
	return msg.ErrorNameNotDefined(M, sy, assignee)
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

func checkExpr(M *ir.Module, sy *ir.Symbol, n *ir.Node) *errors.CompilerError {
	switch n.Lex {
	case lex.IDENTIFIER:
		return checkExprID(M, sy, n)
	case lex.INT, lex.FALSE, lex.TRUE, lex.SYSCALL:
		n.T = termToType(n.Lex)
		return nil
	case lex.PLUS, lex.MINUS:
		return plusMinus(M, sy, n)
	case lex.MULTIPLICATION, lex.DIVISION, lex.REMAINDER:
		return binaryOp(M, sy, n, number, outSame)
	case lex.EQUALS, lex.DIFFERENT,
		lex.MORE, lex.MOREEQ, lex.LESS, lex.LESSEQ:
		return binaryOp(M, sy, n, any, outBool)
	case lex.AND, lex.OR:
		return binaryOp(M, sy, n, _bool, outBool)
	case lex.COLON:
		err := checkExpr(M, sy, n.Leaves[1])
		if err != nil {
			return err
		}
		n.T = getType(n.Leaves[0])
	case lex.CALL:
		return checkCall(M, sy, n)
	case lex.LEFTBRACKET:
		return checkMemAccess(M, sy, n)
	case lex.NOT:
		return unaryOp(M, sy, n, _bool, outBool)
	}
	return nil
}

func checkMemAccess(M *ir.Module, sy *ir.Symbol, n *ir.Node) *errors.CompilerError {
	tp := n.Leaves[1]
	mem := n.Leaves[2]
	local, ok := sy.Proc.Names[mem.Text]
	if ok {
		return msg.ErrorExpectedMemGotLocal(M, local, mem)
	}
	global, ok := M.Globals[mem.Text]
	if !ok {
		return msg.ErrorNameNotDefined(M, sy, mem)
	}
	if global.T != ST.Mem {
		return msg.ErrorExpectedMem(M, global, mem)
	}
	err := checkExpr(M, sy, n.Leaves[0])
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

func checkCall(M *ir.Module, sy *ir.Symbol, n *ir.Node) *errors.CompilerError {
	proc := n.Leaves[1]
	local, ok := sy.Proc.Names[proc.Text]
	if ok {
		return msg.ErrorExpectedProcedureGotLocal(M, local, proc)
	}
	global, ok := M.Globals[proc.Text]
	if ok {
		if global.T != ST.Proc {
			return msg.ErrorExpectedProcedure(M, global, proc)
		}
		return checkCallProc(M, sy, global, n)
	}
	return msg.ErrorNameNotDefined(M, sy, proc)
}

func checkCallProc(M *ir.Module, sy, proc *ir.Symbol, n *ir.Node) *errors.CompilerError {
	exprs := n.Leaves[0]
	if len(exprs.Leaves) != len(proc.Proc.Args) {
		return msg.ErrorInvalidNumberOfArgs(M, proc, n)
	}
	for i, param := range exprs.Leaves {
		err := checkExpr(M, sy, param)
		if err != nil {
			return err
		}
		if param.T != proc.Proc.Args[i].Type {
			return msg.ErrorMismatchedTypeForArgument(M, param, proc, i)
		}
	}
	if len(proc.Proc.Rets) == 1 {
		n.T = proc.Proc.Rets[0]
	} else if len(proc.Proc.Rets) == 0 {
		n.T = T.Void
	} else {
		n.T = T.MultiRet
	}
	return nil
}

func checkExprID(M *ir.Module, sy *ir.Symbol, n *ir.Node) *errors.CompilerError {
	local, ok := sy.Proc.Names[n.Text]
	if ok {
		n.T = local.Type
		return nil
	}
	global, ok := M.Globals[n.Text]
	if ok {
		if global.T != ST.Const {
			return msg.ErrorExpectedConst(M, global, n)
		}
		n.T = global.N.T
	}
	return msg.ErrorNameNotDefined(M, sy, n)
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

func plusMinus(M *ir.Module, sy *ir.Symbol, n *ir.Node) *errors.CompilerError {
	if len(n.Leaves) == 1 {
		return unaryOp(M, sy, n, number, outSame)
	}
	return binaryOp(M, sy, n, number, outSame)
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
func binaryOp(M *ir.Module, sy *ir.Symbol, op *ir.Node, c class, der deriver) *errors.CompilerError {
	if len(op.Leaves) != 2 {
		panic(M.Name + ": internal error, binary operator should have two leaves")
	}
	left := op.Leaves[0]
	err := checkExpr(M, sy, left)
	if err != nil {
		return err
	}
	right := op.Leaves[1]
	err = checkExpr(M, sy, right)
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
func unaryOp(M *ir.Module, sy *ir.Symbol, op *ir.Node, c class, der deriver) *errors.CompilerError {
	if len(op.Leaves) != 1 {
		panic(M.Name + ": internal error, unary operator should have one leaf")
	}
	operand := op.Leaves[0]
	err := checkExpr(M, sy, operand)
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

func checkMemAccessAssignee(M *ir.Module, sy *ir.Symbol, n *ir.Node) *errors.CompilerError {
	id := n.Leaves[0]
	if id.Lex != lex.IDENTIFIER {
		return msg.ErrorBadIndex(M, id)
	}

	local, ok := sy.Proc.Names[id.Text]
	if ok {
		return msg.ErrorCannotIndexLocal(M, local, n)
	}

	global, ok := M.Globals[id.Text]
	if !ok {
		return msg.ErrorNameNotDefined(M, sy, n)
	}

	if global.T != ST.Mem {
		return msg.ErrorCanOnlyIndexMemory(M, global, n)
	}

	err := checkExpr(M, sy, n.Leaves[1])
	if err != nil {
		return err
	}

	n.T = T.I8
	return nil
}
