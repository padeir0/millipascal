package typechecker

import (
	"mpc/frontend/ast"
	"mpc/frontend/errors"
	T "mpc/frontend/lexType"
	msg "mpc/frontend/messages"
	"strconv"
)

func Check(M *ast.Module) *errors.CompilerError {
	for _, sy := range M.Globals {
		err := checkSymbol(M, sy)
		if err != nil {
			return err
		}
	}
	for _, sy := range M.Globals {
		if sy.T == ast.SProc {
			err := checkBlock(M, sy, sy.N.Leaves[4])
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func checkSymbol(M *ast.Module, sy *ast.Symbol) *errors.CompilerError {
	var err *errors.CompilerError
	switch sy.T {
	case ast.SProc:
		err = checkProc(M, sy)
	case ast.SConst:
		err = checkConst(M, sy)
	case ast.SMem:
		err = checkMem(M, sy)
	}
	return err
}

func checkMem(M *ast.Module, sy *ast.Symbol) *errors.CompilerError {
	res := sy.N.Leaves[1]
	if res.Lex == T.RES {
		err := checkMemRes(M, sy, res)
		if err != nil {
			return err
		}
	} else {
		err := checkMemDef(M, sy, res)
		if err != nil {
			return err
		}
	}
	return nil
}

func checkMemDef(M *ast.Module, sy *ast.Symbol, n *ast.Node) *errors.CompilerError {
	for _, term := range n.Leaves {
		err := checkMemDefTerm(M, sy, term)
		if err != nil {
			return err
		}
	}
	return nil
}

func checkMemDefTerm(M *ast.Module, sy *ast.Symbol, n *ast.Node) *errors.CompilerError {
	switch n.Lex {
	case T.INT:
		if sy.Mem.Type != ast.Word {
			return msg.ErrorInvalidInitForMemType(M, sy, n)
		}
		sy.Mem.Size += 1
	case T.CHAR:
		if sy.Mem.Type != ast.Byte {
			return msg.ErrorInvalidInitForMemType(M, sy, n)
		}
		sy.Mem.Size += 1
	case T.STRING:
		if sy.Mem.Type != ast.Byte {
			return msg.ErrorInvalidInitForMemType(M, sy, n)
		}
		sy.Mem.Size += len(n.Text)
	}
	sy.Mem.Init = append(sy.Mem.Init, n)
	return nil
}

func checkMemRes(M *ast.Module, sy *ast.Symbol, n *ast.Node) *errors.CompilerError {
	size, e := strconv.Atoi(n.Leaves[0].Text)
	if e != nil {
		panic(e)
	}
	sy.Mem.Size = size
	sy.Mem.Type = getType(n.Leaves[1])
	err := checkMemResTerm(M, sy, n.Leaves[2])
	if err != nil {
		return err
	}
	return nil
}

func checkMemResTerm(M *ast.Module, sy *ast.Symbol, n *ast.Node) *errors.CompilerError {
	switch n.Lex {
	case T.INT:
		sy.Mem.Init = []*ast.Node{n}
		return nil
	case T.CHAR:
		sy.Mem.Init = []*ast.Node{n}
		return nil
	default:
		return msg.ErrorMemResAllowsOnlyIntAndChar(M, n)
	}
}

func checkConst(M *ast.Module, sy *ast.Symbol) *errors.CompilerError {
	lit := sy.N.Leaves[0]
	switch lit.Lex {
	case T.STRING, T.IDENTIFIER:
		return msg.ErrorConstOnlyWithIntOrChar(M, sy)
	}
	return nil
}

func checkProc(M *ast.Module, sy *ast.Symbol) *errors.CompilerError {
	decls, err := checkProcDecls(M, sy, sy.N.Leaves[1])
	if err != nil {
		return err
	}
	sy.Proc.Args = decls

	decls, err = checkProcDecls(M, sy, sy.N.Leaves[2])
	if err != nil {
		return err
	}
	sy.Proc.Rets = decls

	decls, err = checkProcDecls(M, sy, sy.N.Leaves[3])
	if err != nil {
		return err
	}
	sy.Proc.Vars = decls
	return nil
}

func checkProcDecls(M *ast.Module, sy *ast.Symbol, n *ast.Node) ([]*ast.Decl, *errors.CompilerError) {
	decls := []*ast.Decl{}
	for _, decl := range n.Leaves {
		var d *ast.Decl
		if len(decl.Leaves) == 0 {
			d = &ast.Decl{
				Name: decl.Text,
				Type: ast.Word,
			}
		} else if len(decl.Leaves) == 2 {
			d = &ast.Decl{
				Name: decl.Leaves[0].Text,
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

func getType(n *ast.Node) ast.Type {
	switch n.Lex {
	case T.BYTE:
		return ast.Byte
	case T.WORD:
		return ast.Word
	case T.DWORD:
		return ast.DWord
	case T.QWORD:
		return ast.QWord
	}
	panic("getType: what")
}

func checkBlock(M *ast.Module, sy *ast.Symbol, n *ast.Node) *errors.CompilerError {
	for _, code := range n.Leaves {
		err := checkStatement(M, sy, code)
		if err != nil {
			return err
		}
	}
	return nil
}

func checkStatement(M *ast.Module, sy *ast.Symbol, n *ast.Node) *errors.CompilerError {
	switch n.Lex {
	case T.EOF:
		return nil
	case T.IF:
		return checkIf(M, sy, n)
	case T.WHILE:
		return checkWhile(M, sy, n)
	case T.RETURN:
		return checkReturn(M, sy, n)
	case T.COPY:
		return checkCopy(M, sy, n)
	case T.SET:
		return checkAssignment(M, sy, n)
	default:
		return checkExpr(M, sy, n)
	}
}

func checkIf(M *ast.Module, sy *ast.Symbol, n *ast.Node) *errors.CompilerError {
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
	if len(n.Leaves) == 3 {
		err = checkElse(M, sy, n.Leaves[2])
		if err != nil {
			return err
		}
	}
	return nil
}

func checkElse(M *ast.Module, sy *ast.Symbol, n *ast.Node) *errors.CompilerError {
	switch n.Lex {
	case T.ELSE:
		err := checkBlock(M, sy, n.Leaves[0])
		if err != nil {
			return err
		}
	case T.ELSEIF:
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
	}
	return nil
}

func checkWhile(M *ast.Module, sy *ast.Symbol, n *ast.Node) *errors.CompilerError {
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

func checkReturn(M *ast.Module, sy *ast.Symbol, n *ast.Node) *errors.CompilerError {
	for i, ret := range n.Leaves {
		if i >= len(sy.Proc.Rets) {
			return msg.ErrorInvalidNumberOfReturns(M, sy, ret)
		}
		err := checkExpr(M, sy, ret)
		if err != nil {
			return err
		}
		if sy.Proc.Rets[i].Type != ret.T {
			return msg.ErrorUnmatchingReturns(M, sy, ret, i)
		}
	}
	return nil
}

func checkCopy(M *ast.Module, sy *ast.Symbol, n *ast.Node) *errors.CompilerError {
	memSy, err := checkCopyID(M, sy, n.Leaves[1])
	if err != nil {
		return err
	}

	err = checkTerms(M, sy, memSy, n.Leaves[0])
	if err != nil {
		return err
	}
	return nil
}

func checkCopyID(M *ast.Module, sy *ast.Symbol, n *ast.Node) (*ast.Symbol, *errors.CompilerError) {
	local, ok := sy.Proc.Names[n.Text]
	if ok {
		return nil, msg.ErrorCannotCopyToLocal(M, local, n)
	}

	global, ok := M.Globals[n.Text]
	if !ok {
		return nil, msg.ErrorNameNotDefined(M, sy, n)
	}

	if global.T != ast.SMem {
		return nil, msg.ErrorCanOnlyCopyToMemory(M, global, n)
	}

	return global, nil
}

func checkTerms(M *ast.Module, sy *ast.Symbol, memSy *ast.Symbol, n *ast.Node) *errors.CompilerError {
	total := 0
	for _, term := range n.Leaves {
		switch term.Lex {
		case T.INT:
			if memSy.Mem.Type != ast.Word {
				return msg.ErrorInvalidCopyForMemType(M, memSy, term)
			}
			total += 1
		case T.CHAR:
			if sy.Mem.Type != ast.Byte {
				return msg.ErrorInvalidCopyForMemType(M, memSy, term)
			}
			total += 1
		case T.STRING:
			if sy.Mem.Type != ast.Byte {
				return msg.ErrorInvalidCopyForMemType(M, memSy, term)
			}
			total += len(n.Text)
		}
	}
	if total > memSy.Mem.Size {
		return msg.ErrorCopyTooBigForMem(M, memSy, n)
	}
	return nil
}

func checkAssignment(M *ast.Module, sy *ast.Symbol, n *ast.Node) *errors.CompilerError {
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

	if len(right.Leaves) == 1 {
		exp := right.Leaves[0]
		if exp.T == ast.MultiRet {
			err := checkMultiAssignment(M, left, exp)
			if err != nil {
				return err
			}
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

func checkExprList(M *ast.Module, sy *ast.Symbol, n *ast.Node) *errors.CompilerError {
	for _, exp := range n.Leaves {
		err := checkExpr(M, sy, exp)
		if err != nil {
			return err
		}
	}
	return nil
}

func checkMultiAssignment(M *ast.Module, left *ast.Node, n *ast.Node) *errors.CompilerError {
	procName := n.Leaves[0].Text
	proc := M.Globals[procName]
	if len(proc.Proc.Args) != len(left.Leaves) {
		return msg.ErrorMismatchedMultiRetAssignment(M, proc, n)
	}
	for i, assignee := range left.Leaves {
		if assignee.T != proc.Proc.Args[i].Type {
			return msg.ErrorMismatchedTypesInMultiAssignment(M, proc, left, i)
		}
	}
	return nil
}

func checkExpr(M *ast.Module, sy *ast.Symbol, n *ast.Node) *errors.CompilerError {
	switch n.Lex {
	case T.STRING:
		return msg.ErrorCantUseStringInExpr(M, n)
	case T.IDENTIFIER:
		return checkExprID(M, sy, n)
	case T.INT, T.FALSE, T.TRUE, T.CHAR:
		n.T = termToType(n.Lex)
		return nil
	case T.PLUS, T.MINUS:
		return plusMinus(M, sy, n)
	case T.MULTIPLICATION, T.DIVISION, T.REMAINDER,
		T.EQUALS, T.DIFFERENT,
		T.MORE, T.MOREEQ, T.LESS, T.LESSEQ,
		T.AND, T.OR:
		return binaryOp(M, sy, n)
	case T.COLON:
		err := checkExpr(M, sy, n.Leaves[0])
		if err != nil {
			return err
		}
		n.T = getType(n.Leaves[1])
	case T.CALL:
		return checkCall(M, sy, n)
	case T.LEFTBRACKET:
		return checkMemAccess(M, sy, n)
	case T.NOT:
		return unaryOp(M, sy, n)
	}
	return nil
}

func checkMemAccess(M *ast.Module, sy *ast.Symbol, n *ast.Node) *errors.CompilerError {
	mem := n.Leaves[0]
	local, ok := sy.Proc.Names[mem.Text]
	if ok {
		return msg.ErrorExpectedMemGotLocal(M, local, mem)
	}
	global, ok := M.Globals[mem.Text]
	if ok {
		if global.T != ast.SMem{
			return msg.ErrorExpectedMem(M, global, mem)
		}
		err := checkExpr(M, sy, n.Leaves[1])
		if err != nil {
			return err
		}
		n.T = global.Mem.Type
	}
	return msg.ErrorNameNotDefined(M, sy, mem)
}

func checkCall(M *ast.Module, sy *ast.Symbol, n *ast.Node) *errors.CompilerError {
	proc := n.Leaves[0]
	local, ok := sy.Proc.Names[proc.Text]
	if ok {
		return msg.ErrorExpectedProcedureGotLocal(M, local, proc)
	}
	global, ok := M.Globals[proc.Text]
	if ok {
		if global.T != ast.SProc {
			return msg.ErrorExpectedProcedure(M, global, proc)
		}
		return checkCallProc(M, sy, global, n)
	}
	return msg.ErrorNameNotDefined(M, sy, proc)
}

func checkCallProc(M *ast.Module, sy, proc *ast.Symbol, n *ast.Node) *errors.CompilerError {
	exprs := n.Leaves[1]
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
		n.T = proc.Proc.Rets[0].Type
	} else {
		n.T = ast.MultiRet
	}
	return nil
}

func checkExprID(M *ast.Module, sy *ast.Symbol, n *ast.Node) *errors.CompilerError {
	local, ok := sy.Proc.Names[n.Text]
	if ok {
		n.T = local.Type
		return nil
	}
	global, ok := M.Globals[n.Text]
	if ok {
		if global.T != ast.SConst {
			return msg.ErrorExpectedConst(M, global, n)
		}
		n.T = global.N.T
	}
	return msg.ErrorNameNotDefined(M, sy, n)
}

func termToType(tp T.TkType) ast.Type {
	switch tp {
	case T.INT:
		return ast.Word
	case T.TRUE:
		return ast.Word
	case T.FALSE:
		return ast.Word
	case T.CHAR:
		return ast.Byte
	}
	return ast.InvalidType
}

func plusMinus(M *ast.Module, sy *ast.Symbol, n *ast.Node) *errors.CompilerError {
	if len(n.Leaves) == 1 {
		return unaryOp(M, sy, n)
	}
	return binaryOp(M, sy, n)
}

// a op b where type(a) = type(b) and type(a op b) = type(a) = type(b)
func binaryOp(M *ast.Module, sy *ast.Symbol, op *ast.Node) *errors.CompilerError {
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

	if left.T != right.T {
		return msg.ErrorOperationBetweenUnequalTypes(M, op)
	}

	op.T = left.T
	return nil
}

func checkExprType(M *ast.Module, n *ast.Node) *errors.CompilerError {
	if n.T == ast.MultiRet {
		return msg.ErrorCannotUseMultipleValuesInExpr(M, n)
	}
	if n.T == ast.InvalidType {
		return msg.ErrorInvalidType(M, n)
	}
	return nil
}

// op b where type(op b) = type(b)
func unaryOp(M *ast.Module, sy *ast.Symbol, op *ast.Node) *errors.CompilerError {
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

	op.T = operand.T
	return nil
}

func checkAssignees(M *ast.Module, sy *ast.Symbol, n *ast.Node) *errors.CompilerError {
	for _, assignee := range n.Leaves {
		switch assignee.Lex {
		case T.IDENTIFIER:
			return checkIdAssignee(M, sy, assignee)
		case T.LEFTBRACKET:
			return checkMemAccessAssignee(M, sy, assignee)
		default:
			return msg.ErrorNotAssignable(M, assignee)
		}
	}
	return nil
}

func checkIdAssignee(M *ast.Module, sy *ast.Symbol, n *ast.Node) *errors.CompilerError {
	local, ok := sy.Proc.Names[n.Text]
	if ok {
		n.T = local.Type
		return nil
	}
	global, ok := M.Globals[n.Text]
	if ok {
		return msg.ErrorCannotAssignGlobal(M, global, n)
	}
	return msg.ErrorNameNotDefined(M, sy, n)
}

func checkMemAccessAssignee(M *ast.Module, sy *ast.Symbol, n *ast.Node) *errors.CompilerError {
	id := n.Leaves[0]
	if id.Lex != T.IDENTIFIER {
		return msg.ErrorBadIndex(M, id)
	}

	local, ok := sy.Proc.Names[n.Text]
	if ok {
		return msg.ErrorCannotIndexLocal(M, local, n)
	}

	global, ok := M.Globals[n.Text]
	if !ok {
		return msg.ErrorNameNotDefined(M, sy, n)
	}

	if global.T != ast.SMem {
		return msg.ErrorCanOnlyIndexMemory(M, global, n)
	}

	err := checkExpr(M, sy, n.Leaves[1])
	if err != nil {
		return err
	}

	n.T = global.Mem.Type
	return nil
}
