package typechecker

import (
	T "mpc/frontend/Type"
	lex "mpc/frontend/enums/lexType"
	ST "mpc/frontend/enums/symbolType"
	"mpc/frontend/errors"
	"mpc/frontend/ir"
	msg "mpc/frontend/messages"
)

func Check(M *ir.Module) *errors.CompilerError {
	err := checkModule(M)
	if err != nil {
		return err
	}
	M.ResetVisited()
	return checkMain(M) // only for first module
}

func checkModule(M *ir.Module) *errors.CompilerError {
	M.Visited = true
	for _, dep := range M.Dependencies {
		err := checkModule(dep.M)
		if err != nil {
			return err
		}
	}

	addBuiltins(M)
	for _, sy := range M.Globals {
		if !sy.External {
			err := checkSymbol(M, sy)
			if err != nil {
				return err
			}
		}
	}
	for _, sy := range M.Globals {
		if sy.T == ST.Proc && !sy.External {
			err := checkBlock(M, sy.Proc, sy.N.Leaves[4])
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func checkMain(M *ir.Module) *errors.CompilerError {
	p, ok := M.Globals["main"]
	if !ok {
		return msg.ProgramWithoutEntry(M)
	}
	if p.Proc == nil || !T.IsProc(p.Type) || !T.T_MainProc.Equals(p.Type) {
		return msg.InvalidMain(M, p)
	}
	return nil
}

func addBuiltins(M *ir.Module) {
	w := &T.Type{Proc: &T.ProcType{Args: []*T.Type{T.T_Ptr, T.T_I64}, Rets: []*T.Type{}}}
	r := &T.Type{Proc: &T.ProcType{Args: []*T.Type{T.T_Ptr, T.T_I64}, Rets: []*T.Type{T.T_I64}}}
	write := &ir.Symbol{Name: "write", T: ST.Builtin, Type: w, Proc: nil}
	error := &ir.Symbol{Name: "error", T: ST.Builtin, Type: w, Proc: nil}
	read := &ir.Symbol{Name: "read", T: ST.Builtin, Type: r, Proc: nil}
	M.Globals["write"] = write
	M.Globals["read"] = read
	M.Globals["error"] = error
}

func checkSymbol(M *ir.Module, sy *ir.Symbol) *errors.CompilerError {
	switch sy.T {
	case ST.Proc:
		err := checkProc(M, sy.Proc)
		if err != nil {
			return err
		}
		sy.Type = sy.Proc.T
	case ST.Mem:
		err := checkMem(M, sy.Mem)
		if err != nil {
			return err
		}
		sy.Type = T.T_Ptr
	}
	return nil
}

func checkMem(M *ir.Module, mem *ir.Mem) *errors.CompilerError {
	switch mem.Init.Lex {
	case lex.STRING_LIT:
		size := stringSize(mem.Init.Text)
		mem.Size = int64(size)
		mem.Contents = mem.Init.Text
	case lex.I64_LIT, lex.I32_LIT, lex.I16_LIT, lex.I8_LIT:
		mem.Size = mem.Init.Value
	case lex.PTR_LIT:
		return msg.ErrorPtrCantBeUsedAsMemSize(M, mem.Init)
	}
	return nil
}

func stringSize(oldtext string) int {
	text := oldtext[1 : len(oldtext)-1]
	size := 0
	for i := 0; i < len(text); i++ {
		if text[i] == '\\' {
			i++
		}
		size += 1
	}
	return size
}

func checkProc(M *ir.Module, proc *ir.Proc) *errors.CompilerError {
	nArgs := proc.N.Leaves[1]
	nRets := proc.N.Leaves[2]
	nVars := proc.N.Leaves[3]
	var err *errors.CompilerError
	var args, rets []*T.Type

	if nArgs != nil {
		args, err = checkProcArgs(M, proc, nArgs)
		if err != nil {
			return err
		}
	}

	if nRets != nil {
		rets = getProcRets(M, nRets)
		proc.Rets = rets
	}

	if nVars != nil {
		err := checkProcVars(M, proc, nVars)
		if err != nil {
			return err
		}
	}
	t := &T.Type{Proc: &T.ProcType{Args: args, Rets: rets}}
	proc.T = t
	proc.N.T = t
	return nil
}

func getProcRets(M *ir.Module, n *ir.Node) []*T.Type {
	types := []*T.Type{}
	for _, tNode := range n.Leaves {
		t := getType(tNode)
		types = append(types, t)
		tNode.T = t
	}
	return types
}

func checkProcArgs(M *ir.Module, proc *ir.Proc, n *ir.Node) ([]*T.Type, *errors.CompilerError) {
	tps := []*T.Type{}
	for i, decl := range n.Leaves {
		var d *ir.Symbol
		if len(decl.Leaves) == 0 {
			d = &ir.Symbol{
				Name: decl.Text,
				N:    decl,
				T:    ST.Arg,
				Type: T.T_I64,
			}
		} else if len(decl.Leaves) == 2 {
			d = &ir.Symbol{
				Name: decl.Leaves[0].Text,
				N:    decl,
				T:    ST.Arg,
				Type: getType(decl.Leaves[1]),
			}
		}
		err := verifyIfDefined(M, proc, d)
		if err != nil {
			return nil, err
		}
		decl.T = d.Type
		tps = append(tps, d.Type)
		proc.ArgMap[d.Name] = ir.PositionalSymbol{Position: i, Symbol: d}
		proc.Args = append(proc.Args, d)
	}
	return tps, nil
}

func checkProcVars(M *ir.Module, proc *ir.Proc, n *ir.Node) *errors.CompilerError {
	for i, decl := range n.Leaves {
		var d *ir.Symbol
		if len(decl.Leaves) == 0 {
			d = &ir.Symbol{
				Name: decl.Text,
				N:    decl,
				T:    ST.Var,
				Type: T.T_I64,
			}
		} else if len(decl.Leaves) == 2 {
			d = &ir.Symbol{
				Name: decl.Leaves[0].Text,
				N:    decl,
				T:    ST.Var,
				Type: getType(decl.Leaves[1]),
			}
		}
		err := verifyIfDefined(M, proc, d)
		if err != nil {
			return err
		}
		decl.T = d.Type
		proc.Vars[d.Name] = ir.PositionalSymbol{Position: i, Symbol: d}
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

func getType(n *ir.Node) *T.Type {
	switch n.Lex {
	case lex.I8:
		return T.T_I8
	case lex.I16:
		return T.T_I16
	case lex.I32:
		return T.T_I32
	case lex.I64:
		return T.T_I64
	case lex.PTR:
		return T.T_Ptr
	case lex.BOOL:
		return T.T_Bool
	case lex.PROC:
		return getProcType(n)
	}
	panic("getType: what: " + ir.FmtNode(n))
}

func getProcType(n *ir.Node) *T.Type {
	args := n.Leaves[0].Leaves
	argTypes := make([]*T.Type, len(args))
	for i, arg := range args {
		argTypes[i] = getType(arg)
	}

	retTypes := make([]*T.Type, 0)
	if len(n.Leaves) > 1 && n.Leaves[1] != nil {
		rets := n.Leaves[1].Leaves
		retTypes = make([]*T.Type, len(rets))
		for i, ret := range rets {
			retTypes[i] = getType(ret)
		}
	}

	return &T.Type{
		Proc: &T.ProcType{
			Args: argTypes,
			Rets: retTypes,
		},
	}
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
	case lex.EXIT:
		return checkExit(M, proc, n)
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
		if !ret.T.Equals(proc.Rets[i]) {
			return msg.ErrorUnmatchingReturns(M, proc, ret, i)
		}
	}
	return nil
}

func checkExit(M *ir.Module, proc *ir.Proc, n *ir.Node) *errors.CompilerError {
	exp := n.Leaves[0]
	err := checkExpr(M, proc, exp)
	if err != nil {
		return err
	}
	if !exp.T.Equals(T.T_I8) {
		return msg.ExitMustBeI8(M, exp)
	}
	n.T = T.T_Void
	return nil
}

func checkAssignment(M *ir.Module, proc *ir.Proc, n *ir.Node) *errors.CompilerError {
	left := n.Leaves[0]
	op := n.Leaves[1]
	right := n.Leaves[2]

	err := checkAssignees(M, proc, left)
	if err != nil {
		return err
	}

	err = checkExpr(M, proc, right)
	if err != nil {
		return err
	}

	if (T.IsMultiRet(right.T) || len(left.Leaves) > 1) &&
		op.Lex != lex.ASSIGNMENT {
		return msg.ErrorCanOnlyUseNormalAssignment(M, op)
	}

	if !T.IsMultiRet(right.T) && len(left.Leaves) > 1 ||
		T.IsMultiRet(right.T) && len(left.Leaves) == 1 {
		return msg.ErrorMismatchedAssignment(M, n)
	}

	if T.IsVoid(right.T) {
		return msg.ErrorCannotUseVoid(M, right)
	}

	if T.IsMultiRet(right.T) {
		err := checkMultiAssignment(M, left, right)
		if err != nil {
			return err
		}
	} else {
		if !left.Leaves[0].T.Equals(right.T) {
			return msg.ErrorMismatchedTypesInAssignment(M, left.Leaves[0], right)
		}
		if op.Lex != lex.ASSIGNMENT && !T.IsNumber(left.Leaves[0].T) {
			return msg.ExpectedNumber(M, op, left.T)
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
		if T.IsVoid(exp.T) {
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
		case lex.AT:
			err := checkDeref(M, proc, assignee)
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
	posSy, ok := proc.ArgMap[name]
	if ok {
		return posSy.Symbol
	}
	def, ok := proc.Vars[name]
	if ok {
		return def.Symbol
	}
	return nil
}

func checkMultiAssignment(M *ir.Module, left *ir.Node, n *ir.Node) *errors.CompilerError {
	procName := n.Leaves[1].Text
	proc := M.GetSymbol(procName)
	if len(proc.Proc.Rets) != len(left.Leaves) {
		return msg.ErrorMismatchedMultiRetAssignment(M, proc, n.Leaves[1], left)
	}
	for i, assignee := range left.Leaves {
		if !assignee.T.Equals(proc.Proc.Rets[i]) {
			return msg.ErrorMismatchedTypesInMultiAssignment(M, proc, left, i)
		}
	}
	return nil
}

func checkExpr(M *ir.Module, proc *ir.Proc, n *ir.Node) *errors.CompilerError {
	switch n.Lex {
	case lex.IDENTIFIER:
		return checkID(M, proc, n)
	case lex.DOUBLECOLON:
		return checkExternalID(M, proc, n)
	case lex.I64_LIT, lex.I32_LIT, lex.I16_LIT, lex.I8_LIT,
		lex.FALSE, lex.TRUE, lex.PTR_LIT, lex.STRING_LIT,
		lex.CHAR_LIT:
		n.T = termToType(n.Lex)
		return nil
	case lex.PLUS, lex.MINUS:
		return plusMinus(M, proc, n)
	case lex.MULTIPLICATION, lex.DIVISION, lex.REMAINDER:
		return binaryOp(M, proc, n, number, outSame)
	case lex.EQUALS, lex.DIFFERENT,
		lex.MORE, lex.MOREEQ, lex.LESS, lex.LESSEQ:
		return binaryOp(M, proc, n, basic, outBool)
	case lex.AND, lex.OR:
		return binaryOp(M, proc, n, _bool, outBool)
	case lex.COLON:
		return conversion(M, proc, n)
	case lex.CALL:
		return checkCall(M, proc, n)
	case lex.AT:
		return checkDeref(M, proc, n)
	case lex.NOT:
		return unaryOp(M, proc, n, _bool, outBool)
	case lex.DOT:
		return propertyAccess(M, proc, n)
	}
	return nil
}

func conversion(M *ir.Module, proc *ir.Proc, n *ir.Node) *errors.CompilerError {
	err := checkExpr(M, proc, n.Leaves[1])
	if err != nil {
		return err
	}
	n.T = getType(n.Leaves[0])
	if !T.IsBasic(n.T) {
		return msg.ErrorExpectedBasicType(M, n)
	}
	n.Leaves[0].T = n.T
	return nil
}

func checkCall(M *ir.Module, proc *ir.Proc, n *ir.Node) *errors.CompilerError {
	callee := n.Leaves[1]
	err := checkExpr(M, proc, callee)
	if err != nil {
		return err
	}
	if !T.IsProc(callee.T) {
		return msg.ErrorExpectedProcedure(M, callee)
	}
	return checkCallProc(M, proc, n)
}

func checkCallProc(M *ir.Module, proc *ir.Proc, n *ir.Node) *errors.CompilerError {
	callee := n.Leaves[1].T.Proc
	exprs := n.Leaves[0]
	if len(exprs.Leaves) != len(callee.Args) {
		return msg.ErrorInvalidNumberOfArgs(M, callee, n)
	}
	for i, param := range exprs.Leaves {
		err := checkExpr(M, proc, param)
		if err != nil {
			return err
		}
		if !param.T.Equals(callee.Args[i]) {
			return msg.ErrorMismatchedTypeForArgument(M, param, callee.Args[i])
		}
	}
	if len(callee.Rets) == 1 {
		n.T = callee.Rets[0]
	} else if len(callee.Rets) == 0 {
		n.T = T.T_Void
	} else {
		n.T = T.T_MultiRet
	}
	return nil
}

func checkExternalID(M *ir.Module, proc *ir.Proc, dcolon *ir.Node) *errors.CompilerError {
	mod := dcolon.Leaves[0].Text
	id := dcolon.Leaves[1].Text

	dep, ok := M.Dependencies[mod]
	if !ok {
		return msg.ErrorNameNotDefined(M, dcolon.Leaves[0])
	}

	sy, ok := dep.M.Exported[id]
	if !ok {
		return msg.NameNotExported(M, dcolon.Leaves[1])
	}

	dcolon.Leaves[1].T = sy.Type
	dcolon.T = sy.Type
	return nil
}

func checkID(M *ir.Module, proc *ir.Proc, id *ir.Node) *errors.CompilerError {
	local := getVarOrArg(proc, id.Text)
	if local != nil {
		id.T = local.Type
		return nil
	}
	global, ok := M.Globals[id.Text]
	if ok {
		id.T = global.Type
		if global.External {
			id.T = global.N.T
		}
		return nil
	}
	return msg.ErrorNameNotDefined(M, id)
}

func termToType(tp lex.TkType) *T.Type {
	switch tp {
	case lex.I64_LIT:
		return T.T_I64
	case lex.I32_LIT:
		return T.T_I32
	case lex.I16_LIT:
		return T.T_I16
	case lex.I8_LIT:
		return T.T_I8
	case lex.CHAR_LIT:
		return T.T_I8
	case lex.STRING_LIT:
		return T.T_Ptr
	case lex.TRUE:
		return T.T_Bool
	case lex.FALSE:
		return T.T_Bool
	case lex.PTR_LIT:
		return T.T_Ptr
	}
	panic("termToType: invalid type")
}

func plusMinus(M *ir.Module, proc *ir.Proc, n *ir.Node) *errors.CompilerError {
	if len(n.Leaves) == 1 {
		return unaryOp(M, proc, n, number, outSame)
	}
	return binaryOp(M, proc, n, number, outSame)
}

type deriver func(types ...*T.Type) *T.Type

func outSame(a ...*T.Type) *T.Type {
	// a homogeneous, all items must be of the same type
	return a[0]
}

func outBool(a ...*T.Type) *T.Type {
	return T.T_Bool
}

type class struct {
	Description string
	Checker     func(t *T.Type) bool
}

var basic = class{
	Description: "i8, i16, i32, i64, bool or ptr",
	Checker:     T.IsBasic,
}

var _bool = class{
	Description: "bool",
	Checker:     T.IsBool,
}

var number = class{
	Description: "i8, i16, i32, i64 or ptr",
	Checker:     T.IsNumber,
}

var ptr = class{
	Description: "ptr",
	Checker:     T.IsPtr,
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

	if !left.T.Equals(right.T) {
		return msg.ErrorOperationBetweenUnequalTypes(M, op)
	}

	op.T = der(left.T, right.T)
	return nil
}

func checkExprType(M *ir.Module, n *ir.Node) *errors.CompilerError {
	if T.IsMultiRet(n.T) {
		return msg.ErrorCannotUseMultipleValuesInExpr(M, n)
	}
	if T.IsVoid(n.T) {
		return msg.ErrorCannotUseVoid(M, n)
	}
	if T.IsInvalid(n.T) {
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

func checkDeref(M *ir.Module, proc *ir.Proc, n *ir.Node) *errors.CompilerError {
	exp := n.Leaves[1]
	t := n.Leaves[0]
	t.T = getType(t)
	n.T = t.T

	err := checkExpr(M, proc, exp)
	if err != nil {
		return err
	}
	if !T.IsPtr(exp.T) {
		return msg.ErrorBadDeref(M, n, exp.T)
	}
	return nil
}

func propertyAccess(M *ir.Module, proc *ir.Proc, n *ir.Node) *errors.CompilerError {
	mem := n.Leaves[1]
	prop := n.Leaves[0]
	sy := M.GetSymbol(mem.Text)
	if mem.Lex != lex.IDENTIFIER || sy == nil || sy.Mem == nil {
		return msg.ErrorExpectedMem(M, mem)
	}
	if prop.Lex != lex.IDENTIFIER || isInvalidProp(prop.Text) {
		return msg.ErrorInvalidProp(M, mem)
	}
	n.T = T.T_I64
	return nil
}

func isInvalidProp(text string) bool {
	return text != "size"
}
