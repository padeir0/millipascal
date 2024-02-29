package typechecker

import (
	. "mpc/core"
	mod "mpc/core/module"
	lk "mpc/core/module/lexkind"
	ST "mpc/core/module/symbolkind"
	msg "mpc/messages"
	T "mpc/pir/types"

	"fmt"
)

func Check(M *mod.Module) *Error {
	err := checkModule(M)
	if err != nil {
		return err
	}
	M.ResetVisited()
	return checkMain(M) // only for first module
}

func checkModule(M *mod.Module) *Error {
	if M.Visited {
		return nil
	}
	M.Visited = true
	for _, dep := range M.Dependencies {
		err := checkModule(dep.M)
		if err != nil {
			return err
		}
	}

	err := checkSymbolsTpl(M)
	if err != nil {
		return err
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

func checkSymbolsTpl(M *mod.Module) *Error {
	for _, sy := range M.Globals {
		if !sy.External {
			err := checkSymbol(M, sy)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func checkMain(M *mod.Module) *Error {
	p, ok := M.Globals["main"]
	if !ok {
		return msg.ProgramWithoutEntry(M)
	}
	if p.Proc == nil || !T.IsProc(p.Type) || !T.T_MainProc.Equals(p.Type) {
		return msg.InvalidMain(M, p)
	}
	return nil
}

func checkSymbol(M *mod.Module, sy *mod.Symbol) *Error {
	if sy.Visited {
		return nil
	}
	err := checkSyDeps(M, sy)
	if err != nil {
		return err
	}
	sy.Visited = true
	switch sy.T {
	case ST.Proc:
		err := checkProc(M, sy.Proc)
		if err != nil {
			return err
		}
		sy.Type = sy.Proc.T
	case ST.Data:
		err := checkData(M, sy)
		if err != nil {
			return err
		}
		sy.Type = T.T_Ptr
		sy.N.T = T.T_Ptr
	case ST.Const:
		expr := sy.N.Leaves[1]
		err := checkExpr(M, nil, expr)
		if err != nil {
			return err
		}
		sy.Type = expr.T
		sy.N.T = expr.T
		if !T.IsBasic(sy.Type) {
			return msg.InvalidTypeForConst(M, sy.N)
		}
	}
	return nil
}

func checkSyDeps(M *mod.Module, sy *mod.Symbol) *Error {
	for _, ref := range sy.Refs {
		err := checkSymbol(M, ref)
		if err != nil {
			return err
		}
	}
	return nil
}

func checkData(M *mod.Module, sy *mod.Symbol) *Error {
	dt := sy.Data
	switch dt.Init.Lex {
	case lk.STRING_LIT:
		dt.DataType = T.T_I8
	case lk.BLOB:
		blob := sy.N.Leaves[1]
		annot := blob.Leaves[0]
		contents := blob.Leaves[1]
		var err *Error
		if annot != nil {
			annotT := annot.Leaves[0]
			t, err := getType(M, sy, annot.Leaves[0])
			if err != nil {
				return err
			}
			annotT.T = t
			dt.DataType = annotT.T
			err = checkBlob(M, sy, contents, annotT.T)
			if err != nil {
				return err
			}
		} else {
			err = checkBlob(M, sy, contents, T.T_I64)
			if err != nil {
				return err
			}
		}
	default:
		expr := sy.N.Leaves[1]
		dt.DataType = T.T_I8
		err := checkExpr(M, nil, expr)
		if err != nil {
			return err
		}
	}
	return nil
}

func checkBlob(M *mod.Module, sy *mod.Symbol, contents *mod.Node, t *T.Type) *Error {
	for _, leaf := range contents.Leaves {
		err := checkExpr(M, nil, leaf)
		if err != nil {
			return err
		}
		if !leaf.T.Equals(t) {
			return msg.DoesntMatchBlobAnnot(M, leaf, t)
		}
	}
	return nil
}

func checkProc(M *mod.Module, proc *mod.Proc) *Error {
	nArgs := proc.N.Leaves[1]
	nRets := proc.N.Leaves[2]
	nVars := proc.N.Leaves[3]
	var err *Error
	var args, rets []*T.Type

	if nArgs != nil {
		args, err = checkProcArgs(M, proc, nArgs)
		if err != nil {
			return err
		}
	}

	if nRets != nil {
		rets, err = getProcRets(M, nRets)
		if err != nil {
			return err
		}
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

func getProcRets(M *mod.Module, n *mod.Node) ([]*T.Type, *Error) {
	types := []*T.Type{}
	for _, tNode := range n.Leaves {
		t, err := getType(M, nil, tNode)
		if err != nil {
			return nil, err
		}
		types = append(types, t)
		tNode.T = t
	}
	return types, nil
}

func checkProcArgs(M *mod.Module, proc *mod.Proc, n *mod.Node) ([]*T.Type, *Error) {
	tps := []*T.Type{}
	position := 0
	for _, decl := range n.Leaves {
		tp, err := getType(M, nil, decl.Leaves[1])
		if err != nil {
			return nil, err
		}
		idlist := decl.Leaves[0]
		for _, id := range idlist.Leaves {
			d := &mod.Symbol{
				Name: id.Text,
				N:    id,
				T:    ST.Arg,
				Type: tp,
			}
			err := verifyIfDefined(M, proc, d)
			if err != nil {
				return nil, err
			}
			decl.T = d.Type
			tps = append(tps, d.Type)
			proc.ArgMap[d.Name] = mod.PositionalSymbol{
				Position: position, Symbol: d,
			}
			proc.Args = append(proc.Args, d)
			position++
		}
	}
	return tps, nil
}

func checkProcVars(M *mod.Module, proc *mod.Proc, n *mod.Node) *Error {
	position := 0
	for _, decl := range n.Leaves {
		idlist := decl.Leaves[0]
		tp, err := getType(M, nil, decl.Leaves[1])
		if err != nil {
			return err
		}
		for _, id := range idlist.Leaves {
			d := &mod.Symbol{
				Name: id.Text,
				N:    id,
				T:    ST.Var,
				Type: tp,
			}
			err := verifyIfDefined(M, proc, d)
			if err != nil {
				return err
			}
			decl.T = d.Type
			proc.Vars[d.Name] = mod.PositionalSymbol{
				Position: position, Symbol: d,
			}
			position++
		}
	}
	return nil
}

func verifyIfDefined(M *mod.Module, proc *mod.Proc, d *mod.Symbol) *Error {
	l := getVarOrArg(proc, d.Name)
	if l != nil {
		return msg.ErrorNameAlreadyDefined(M, d.N)
	}
	return nil
}

// sy might be nil
func getType(M *mod.Module, sy *mod.Symbol, n *mod.Node) (*T.Type, *Error) {
	switch n.Lex {
	case lk.I8:
		return T.T_I8, nil
	case lk.I16:
		return T.T_I16, nil
	case lk.I32:
		return T.T_I32, nil
	case lk.I64:
		return T.T_I64, nil
	case lk.U8:
		return T.T_U8, nil
	case lk.U16:
		return T.T_U16, nil
	case lk.U32:
		return T.T_U32, nil
	case lk.U64:
		return T.T_U64, nil
	case lk.PTR:
		return T.T_Ptr, nil
	case lk.BOOL:
		return T.T_Bool, nil
	case lk.PROC:
		return getProcType(M, sy, n)
	case lk.IDENTIFIER, lk.DOUBLECOLON:
		return getIDType(M, sy, n)
	case lk.DOT:
		return nil, msg.ErrorBadType(M, n)
	}
	panic("getType: what: " + n.String())
}

func getProcType(M *mod.Module, sy *mod.Symbol, n *mod.Node) (*T.Type, *Error) {
	argTypes := make([]*T.Type, 0)
	if n.Leaves[0] != nil {
		args := n.Leaves[0].Leaves
		argTypes = make([]*T.Type, len(args))
		for i, arg := range args {
			t, err := getType(M, sy, arg)
			if err != nil {
				return nil, err
			}
			argTypes[i] = t
		}
	}

	retTypes := make([]*T.Type, 0)
	if len(n.Leaves) > 1 && n.Leaves[1] != nil {
		rets := n.Leaves[1].Leaves
		retTypes = make([]*T.Type, len(rets))
		for i, ret := range rets {
			t, err := getType(M, sy, ret)
			if err != nil {
				return nil, err
			}
			retTypes[i] = t
		}
	}

	return &T.Type{
		Proc: &T.ProcType{
			Args: argTypes,
			Rets: retTypes,
		},
	}, nil
}

func getIDType(M *mod.Module, sy *mod.Symbol, n *mod.Node) (*T.Type, *Error) {
	switch n.Lex {
	case lk.IDENTIFIER: // name in scope
		if sy == nil { // not inside a struct
			name := n.Text
			source := M.GetSymbol(name)
			if source == nil {
				return nil, msg.ErrorNameNotDefined(M, n)
			}
			if source.T != ST.Struct {
				return nil, msg.ErrorBadType(M, n)
			}
			return source.Type, nil
		}
		if sy.T == ST.Struct {
			// TODO: check if identifier exists in fields of struct
		}
	case lk.DOUBLECOLON: // accessing a name in module
		moduleName := n.Leaves[0].Text
		symbolName := n.Leaves[1].Text
		source := M.GetExternalSymbol(moduleName, symbolName)
		if source == nil {
			return nil, msg.ErrorNameNotDefined(M, n)
		}
		if source.T != ST.Struct {
			return nil, msg.ErrorBadType(M, n)
		}
	}
	panic("unreachable")
}

func checkBlock(M *mod.Module, proc *mod.Proc, n *mod.Node) *Error {
	for _, code := range n.Leaves {
		err := checkStatement(M, proc, code)
		if err != nil {
			return err
		}
	}
	return nil
}

func checkStatement(M *mod.Module, proc *mod.Proc, n *mod.Node) *Error {
	switch n.Lex {
	case lk.EOF:
		return nil
	case lk.IF:
		return checkIf(M, proc, n)
	case lk.WHILE:
		return checkWhile(M, proc, n)
	case lk.RETURN:
		return checkReturn(M, proc, n)
	case lk.SET:
		return checkAssignment(M, proc, n)
	case lk.EXIT:
		return checkExit(M, proc, n)
	default:
		return checkExpr(M, proc, n)
	}
}

func checkIf(M *mod.Module, proc *mod.Proc, n *mod.Node) *Error {
	exp := n.Leaves[0]
	block := n.Leaves[1]
	elseifchain := n.Leaves[2]
	else_ := n.Leaves[3]

	err := checkExpr(M, proc, exp)
	if err != nil {
		return err
	}

	if !exp.T.Equals(T.T_Bool) {
		return msg.ExpectedBool(M, exp)
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

func checkElse(M *mod.Module, proc *mod.Proc, n *mod.Node) *Error {
	err := checkBlock(M, proc, n.Leaves[0])
	if err != nil {
		return err
	}
	return nil
}

func checkElseIfChain(M *mod.Module, proc *mod.Proc, n *mod.Node) *Error {
	for _, elseif := range n.Leaves {
		err := checkElseIf(M, proc, elseif)
		if err != nil {
			return err
		}
	}
	return nil
}

func checkElseIf(M *mod.Module, proc *mod.Proc, n *mod.Node) *Error {
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

func checkWhile(M *mod.Module, proc *mod.Proc, n *mod.Node) *Error {
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

func checkReturn(M *mod.Module, proc *mod.Proc, n *mod.Node) *Error {
	if len(n.Leaves) != len(proc.Rets) {
		return msg.ErrorInvalidNumberOfReturns(M, proc, n)
	}
	for i, ret := range n.Leaves {
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

func checkExit(M *mod.Module, proc *mod.Proc, n *mod.Node) *Error {
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

func checkAssignment(M *mod.Module, proc *mod.Proc, n *mod.Node) *Error {
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

	if right.T == nil {
		fmt.Println(n)
		panic("right side is nil!!")
	}
	if (T.IsMultiRet(right.T) || len(left.Leaves) > 1) &&
		op.Lex != lk.ASSIGNMENT {
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
		if op.Lex != lk.ASSIGNMENT && !T.IsNumber(left.Leaves[0].T) {
			return msg.ExpectedNumber(M, op, left.T)
		}
	}

	return nil
}

func checkExprList(M *mod.Module, proc *mod.Proc, n *mod.Node) *Error {
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

func checkAssignees(M *mod.Module, proc *mod.Proc, left *mod.Node) *Error {
	for _, assignee := range left.Leaves {
		switch assignee.Lex {
		case lk.IDENTIFIER:
			err := checkIdAssignee(M, proc, assignee)
			if err != nil {
				return err
			}
		case lk.AT:
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

func checkIdAssignee(M *mod.Module, proc *mod.Proc, assignee *mod.Node) *Error {
	d := getVarOrArg(proc, assignee.Text)
	if d != nil {
		assignee.T = d.Type
		return nil
	}
	_, ok := M.Globals[assignee.Text]
	if ok {
		return msg.ErrorCannotAssignGlobal(M, assignee)
	}
	return msg.ErrorNameNotDefined(M, assignee)
}

func getVarOrArg(proc *mod.Proc, name string) *mod.Symbol {
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

func checkMultiAssignment(M *mod.Module, left *mod.Node, n *mod.Node) *Error {
	procName := n.Leaves[1].Text
	proc := M.GetSymbol(procName)
	if len(proc.Proc.Rets) != len(left.Leaves) {
		return msg.ErrorMismatchedMultiRetAssignment(M, proc, n.Leaves[1], left)
	}
	for i, assignee := range left.Leaves {
		if !assignee.T.Equals(proc.Proc.Rets[i]) {
			return msg.ErrorMismatchedTypesInMultiAssignment(M, proc, assignee, i)
		}
	}
	return nil
}

func checkExpr(M *mod.Module, proc *mod.Proc, n *mod.Node) *Error {
	switch n.Lex {
	case lk.IDENTIFIER:
		return checkID(M, proc, n)
	case lk.SIZEOF:
		return checkSizeof(M, proc, n)
	case lk.DOUBLECOLON:
		return checkExternalID(M, n)
	case lk.I64_LIT, lk.I32_LIT, lk.I16_LIT, lk.I8_LIT,
		lk.U64_LIT, lk.U32_LIT, lk.U16_LIT, lk.U8_LIT,
		lk.FALSE, lk.TRUE, lk.PTR_LIT, lk.STRING_LIT,
		lk.CHAR_LIT:
		n.T = termToType(n.Lex)
		return nil
	case lk.NEG, lk.BITWISENOT:
		return unaryOp(M, proc, n, number, outSame)
	case lk.PLUS, lk.MINUS, lk.MULTIPLICATION,
		lk.DIVISION, lk.REMAINDER, lk.BITWISEAND,
		lk.BITWISEXOR, lk.BITWISEOR, lk.SHIFTLEFT,
		lk.SHIFTRIGHT:
		return binaryOp(M, proc, n, number, outSame)
	case lk.EQUALS, lk.DIFFERENT:
		return binaryOp(M, proc, n, comparable, outBool)
	case lk.MORE, lk.MOREEQ, lk.LESS, lk.LESSEQ:
		return binaryOp(M, proc, n, basic, outBool)
	case lk.AND, lk.OR:
		return binaryOp(M, proc, n, _bool, outBool)
	case lk.COLON:
		return conversion(M, proc, n)
	case lk.CALL:
		return checkCall(M, proc, n)
	case lk.AT:
		return checkDeref(M, proc, n)
	case lk.NOT:
		return unaryOp(M, proc, n, _bool, outBool)
	case lk.DOT:
		return dotAccess(M, n)
	case lk.ARROW:
		return arrowAccess(M, n)
	}
	return nil
}

func checkSizeof(M *mod.Module, proc *mod.Proc, n *mod.Node) *Error {
	thing := n.Leaves[0]
	// special cases of sizeof: data declarations, struct fields
	switch thing.Lex {
	case lk.DOT: // struct fields
		left := n.Leaves[0]
		// right := n.Leaves[1]
		switch left.Lex {
		case lk.IDENTIFIER: // struct
		case lk.DOUBLECOLON: // external struct
		}
		panic("unimplemented")
	case lk.IDENTIFIER: // data declarations
		sy := M.GetSymbol(thing.Text)
		if sy == nil {
			return msg.ErrorNameNotDefined(M, n)
		}
		if sy.T != ST.Data {
			return msg.ErrorExpectedData(M, n)
		}
	case lk.DOUBLECOLON: // external data declarations
		moduleName := thing.Leaves[0].Text
		symbolName := thing.Leaves[1].Text
		sy := M.GetExternalSymbol(moduleName, symbolName)
		if sy == nil {
			return msg.ErrorNameNotDefined(M, n)
		}
		if sy.T != ST.Data {
			return msg.ErrorExpectedData(M, n)
		}
	default:
		t, err := getType(M, nil, thing)
		if err != nil {
			return err
		}
		thing.T = t
	}
	n.T = T.T_I64
	return nil
}

func conversion(M *mod.Module, proc *mod.Proc, n *mod.Node) *Error {
	err := checkExpr(M, proc, n.Leaves[1])
	if err != nil {
		return err
	}
	n.T, err = getType(M, nil, n.Leaves[0])
	if err != nil {
		return err
	}
	if !T.IsBasicOrProc(n.T) {
		return msg.ErrorExpectedBasicOrProc(M, n)
	}
	n.Leaves[0].T = n.T
	return nil
}

func checkCall(M *mod.Module, proc *mod.Proc, n *mod.Node) *Error {
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

func checkCallProc(M *mod.Module, proc *mod.Proc, n *mod.Node) *Error {
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

func checkExternalID(M *mod.Module, dcolon *mod.Node) *Error {
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

func checkID(M *mod.Module, proc *mod.Proc, id *mod.Node) *Error {
	if proc != nil { // means we're inside a procedure
		local := getVarOrArg(proc, id.Text)
		if local != nil {
			id.T = local.Type
			return nil
		}
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

func termToType(tp lk.LexKind) *T.Type {
	switch tp {
	case lk.I64_LIT:
		return T.T_I64
	case lk.I32_LIT:
		return T.T_I32
	case lk.I16_LIT:
		return T.T_I16
	case lk.I8_LIT:
		return T.T_I8
	case lk.U64_LIT:
		return T.T_U64
	case lk.U32_LIT:
		return T.T_U32
	case lk.U16_LIT:
		return T.T_U16
	case lk.U8_LIT:
		return T.T_U8
	case lk.CHAR_LIT:
		return T.T_I8
	case lk.STRING_LIT:
		return T.T_Ptr
	case lk.TRUE:
		return T.T_Bool
	case lk.FALSE:
		return T.T_Bool
	case lk.PTR_LIT:
		return T.T_Ptr
	}
	panic("termToType: invalid type")
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
	Description: "i8 ~ i64, u8 ~ u64, ptr or bool",
	Checker:     T.IsBasic,
}

var _bool = class{
	Description: "bool",
	Checker:     T.IsBool,
}

var number = class{
	Description: "i8 ~ i64, u8 ~ u64 or ptr",
	Checker:     T.IsNumber,
}

var comparable = class{
	Description: "all types",
	Checker:     T.IsBasicOrProc,
}

var ptr = class{
	Description: "ptr",
	Checker:     T.IsPtr,
}

// a op b where type(a) = type(b) and type(a op b) = deriver(type(a), type(b))
// and both type(a), type(b) is of the class specified
func binaryOp(M *mod.Module, proc *mod.Proc, op *mod.Node, c class, der deriver) *Error {
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
		return msg.ErrorInvalidClassForExpr(M, op, left, c.Description)
	}

	if !c.Checker(right.T) {
		return msg.ErrorInvalidClassForExpr(M, op, right, c.Description)
	}

	if !left.T.Equals(right.T) {
		return msg.ErrorOperationBetweenUnequalTypes(M, op)
	}

	op.T = der(left.T, right.T)
	return nil
}

func checkExprType(M *mod.Module, n *mod.Node) *Error {
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
func unaryOp(M *mod.Module, proc *mod.Proc, op *mod.Node, c class, der deriver) *Error {
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
		return msg.ErrorInvalidClassForExpr(M, op, operand, c.Description)
	}

	op.T = der(operand.T)
	return nil
}

func checkDeref(M *mod.Module, proc *mod.Proc, n *mod.Node) *Error {
	exp := n.Leaves[1]
	t := n.Leaves[0]
	var err *Error
	t.T, err = getType(M, nil, t)
	if err != nil {
		return err
	}
	n.T = t.T

	err = checkExpr(M, proc, exp)
	if err != nil {
		return err
	}
	if !T.IsPtr(exp.T) {
		return msg.ErrorBadDeref(M, n, exp.T)
	}
	return nil
}

// Cases:
//     STRUCT.field yielding a pointer offset
//     p.field
//         where p is an expression of struct type
//         and field is a valid field in the struct type.
//         yields (p + STRUCT.field) of type pointer
func dotAccess(M *mod.Module, n *mod.Node) *Error {
	panic("unimplemented")
}

// p->field where p is an expression of struct type and field is a
// valid field in the struct, yields (p + STRUCT.field)@fieldtype,
// where fieldtype is the type specified at the struct declaration
func arrowAccess(M *mod.Module, n *mod.Node) *Error {
	panic("unimplemented")
}
