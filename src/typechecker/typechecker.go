package typechecker

import (
	. "mpc/core"
	mod "mpc/core/module"
	GK "mpc/core/module/globalkind"
	LxK "mpc/core/module/lexkind"
	LcK "mpc/core/module/localkind"
	msg "mpc/messages"
	T "mpc/pir/types"
)

func Check(M *mod.Module) *Error {
	err := checkModule(M)
	if err != nil {
		return err
	}
	M.ResetVisited()
	return nil // only for first module
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
		if sy.Kind == GK.Proc && !sy.External {
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
			err := checkSymbol(M, mod.FromSymbol(sy))
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func CheckMain(M *mod.Module) *Error {
	p, ok := M.Globals["main"]
	if !ok {
		return msg.ProgramWithoutEntry(M)
	}
	if p.Proc == nil || !T.IsProc(p.Proc.Type) || !T.T_MainProc.Equals(p.Proc.Type) {
		return msg.InvalidMain(M, p)
	}
	return nil
}

type modSy struct {
	Mod string
	Sy  string
}

func createStructTypes(M *mod.Module) *Error {
	M.ResetVisited()
	strmap := map[modSy]*T.Type{}
	return createInternalStructs(M, strmap)
}

func createInternalStructs(M *mod.Module, strmap map[modSy]*T.Type) *Error {
	if M.Visited {
		return nil
	}
	M.Visited = true
	for _, dep := range M.Dependencies {
		createInternalStructs(dep.M, strmap)
	}
	for _, sy := range M.Globals {
		if sy.Kind == GK.Struct && !sy.External {
			ms := modSy{
				Mod: sy.ModuleName,
				Sy:  sy.Name,
			}
			t := T.Type{
				Basic: T.Ptr,
				Proc:  nil,
				Struct: &T.Struct{
					Module:   ms.Mod,
					Name:     ms.Sy,
					Fields:   []T.Field{},
					FieldMap: map[string]int{},
					Size:     nil,
				},
			}
			strmap[ms] = &t
			sy.Struct.Type = &t
		}
	}

	for _, sy := range M.Globals {
		if sy.Kind == GK.Struct && !sy.External {
			err := createStructFields(M, strmap, sy)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func createStructFields(M *mod.Module, strmap map[modSy]*T.Type, sy *mod.Global) *Error {
	fields := sy.N.Leaves[2]
	fieldIndex := 0
	stType := sy.Struct.Type
	for _, field := range fields.Leaves {
		idlist := field.Leaves[0]
		ann := field.Leaves[1]
		t, err := getType(M, ann.Leaves[0])
		if err != nil {
			return err
		}
		for _, id := range idlist.Leaves {
			stType.Struct.Fields[fieldIndex] = T.Field{
				Name:   id.Text,
				Type:   t,
				Offset: nil,
			}
			stType.Struct.FieldMap[id.Text] = fieldIndex
			fieldIndex++
		}
	}
	return nil
}

func checkSymbol(M *mod.Module, sf mod.SyField) *Error {
	if sf.IsVisited() {
		return nil
	}
	sf.SetVisited(true) // this can come before or after checkSyDeps, since there are no cycles
	err := checkSyDeps(M, sf)
	if err != nil {
		return err
	}
	if sf.IsField() {
		return checkField(M, sf)
	}
	switch sf.Sy.Kind {
	case GK.Proc:
		return checkProc(M, sf.Sy.Proc)
	case GK.Data:
		return checkData(M, sf.Sy)
	case GK.Const:
		sy := sf.Sy
		var t *T.Type
		annot := sy.N.Leaves[1]
		if annot != nil {
			t, err = getType(M, annot.Leaves[0])
		}
		expr := sy.N.Leaves[2]
		err := checkExpr(M, nil, expr)
		if err != nil {
			return err
		}
		if t != nil && !t.Equals(expr.T) {
			return msg.ErrorMismatchedAssignment(M, sy.N)
		}
		sy.Const.Type = expr.T
		if !T.IsBasic(sy.Const.Type) {
			return msg.InvalidTypeForConst(M, sy.N)
		}
	case GK.Struct:
		// check the size and offset expressions
		return checkStruct(M, sf.Sy)
	}
	return nil
}

func checkStruct(M *mod.Module, sy *mod.Global) *Error {
	size := sy.N.Leaves[1]
	err := checkExpr(M, nil, size)
	if err != nil {
		return err
	}
	fields := sy.N.Leaves[2]
	for _, field := range fields.Leaves {
		offset := field.Leaves[2]
		err := checkExpr(M, nil, offset)
		if err != nil {
			return err
		}
	}
	return nil
}

func checkField(M *mod.Module, sf mod.SyField) *Error {
	field := sf.GetField()
	if field.Offset != nil {
		err := checkExpr(M, nil, field.Offset)
		if err != nil {
			return err
		}
		if !T.IsInteger(field.Offset.T) {
			return msg.ExpectedInteger(M, field.Offset, field.Offset.T)
		}
	}
	return nil
}

func checkSyDeps(M *mod.Module, sy mod.SyField) *Error {
	var refs mod.Refs
	if sy.IsField() {
		refs = sy.Sy.Struct.Fields[sy.Field].Refs
	} else {
		refs = sy.Sy.Refs
	}
	for _, ref := range refs.Symbols {
		err := checkSymbol(M, ref)
		if err != nil {
			return err
		}
	}
	return nil
}

func checkData(M *mod.Module, sy *mod.Global) *Error {
	dt := sy.Data
	annot := sy.N.Leaves[1]
	var err *Error
	dt.Type = T.T_Ptr
	if annot != nil {
		dt.Type, err = getType(M, annot)
		if err != nil {
			return err
		}
	}
	switch dt.Init.Lex {
	case LxK.STRING_LIT:
	case LxK.BLOB:
		err = checkBlob(M, sy, dt.Init, sy.Data.Type)
		if err != nil {
			return err
		}
	default:
		expr := sy.N.Leaves[2]
		err = checkExpr(M, nil, expr)
		if err != nil {
			return err
		}
	}
	return nil
}

// TODO: checkBlob
func checkBlob(M *mod.Module, sy *mod.Global, contents *mod.Node, t *T.Type) *Error {
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
	proc.Type = t
	proc.N.T = t
	return nil
}

func getProcRets(M *mod.Module, n *mod.Node) ([]*T.Type, *Error) {
	types := []*T.Type{}
	for _, tNode := range n.Leaves {
		t, err := getType(M, tNode)
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
		tp, err := getType(M, decl.Leaves[1])
		if err != nil {
			return nil, err
		}
		idlist := decl.Leaves[0]
		for _, id := range idlist.Leaves {
			d := &mod.Local{
				Name: id.Text,
				N:    id,
				Kind: LcK.Argument,
				T:    tp,
			}
			err := verifyIfDefined(M, proc, d)
			if err != nil {
				return nil, err
			}
			decl.T = d.T
			tps = append(tps, d.T)
			proc.ArgMap[d.Name] = mod.PositionalLocal{
				Position: position, Local: d,
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
		tp, err := getType(M, decl.Leaves[1])
		if err != nil {
			return err
		}
		for _, id := range idlist.Leaves {
			d := &mod.Local{
				Name: id.Text,
				N:    id,
				Kind: LcK.Variable,
				T:    tp,
			}
			err := verifyIfDefined(M, proc, d)
			if err != nil {
				return err
			}
			decl.T = d.T
			proc.Vars[d.Name] = mod.PositionalLocal{
				Position: position, Local: d,
			}
			position++
		}
	}
	return nil
}

func verifyIfDefined(M *mod.Module, proc *mod.Proc, d *mod.Local) *Error {
	l := getLocal(proc, d.Name)
	if l != nil {
		return msg.ErrorNameAlreadyDefined(M, d.N, d.Name)
	}
	return nil
}

func getType(M *mod.Module, n *mod.Node) (*T.Type, *Error) {
	switch n.Lex {
	case LxK.I8:
		return T.T_I8, nil
	case LxK.I16:
		return T.T_I16, nil
	case LxK.I32:
		return T.T_I32, nil
	case LxK.I64:
		return T.T_I64, nil
	case LxK.U8:
		return T.T_U8, nil
	case LxK.U16:
		return T.T_U16, nil
	case LxK.U32:
		return T.T_U32, nil
	case LxK.U64:
		return T.T_U64, nil
	case LxK.PTR:
		return T.T_Ptr, nil
	case LxK.BOOL:
		return T.T_Bool, nil
	case LxK.PROC:
		return getProcType(M, n)
	case LxK.IDENTIFIER, LxK.DOUBLECOLON:
		return getIDType(M, n)
	}
	panic("getType: what: " + n.String())
}

func getProcType(M *mod.Module, n *mod.Node) (*T.Type, *Error) {
	argTypes := make([]*T.Type, 0)
	if n.Leaves[0] != nil {
		args := n.Leaves[0].Leaves
		argTypes = make([]*T.Type, len(args))
		for i, arg := range args {
			t, err := getType(M, arg)
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
			t, err := getType(M, ret)
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

func getIDType(M *mod.Module, n *mod.Node) (*T.Type, *Error) {
	var found *mod.Global
	switch n.Lex {
	case LxK.IDENTIFIER:
		found = M.GetSymbol(n.Text)
	case LxK.DOUBLECOLON:
		mod := n.Leaves[0].Text
		id := n.Leaves[1].Text
		found = M.GetExternalSymbol(mod, id)
	}
	if found != nil {
		return nil, msg.ErrorNameNotDefined(M, n)
	}
	if found.Kind != GK.Struct {
		return nil, msg.ErrorExpectedStruct(M, n)
	}
	return found.Struct.Type, nil
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
	case LxK.EOF:
		return nil
	case LxK.IF:
		return checkIf(M, proc, n)
	case LxK.WHILE:
		return checkWhile(M, proc, n)
	case LxK.DO:
		return checkDoWhile(M, proc, n)
	case LxK.RETURN:
		return checkReturn(M, proc, n)
	case LxK.SET:
		return checkSet(M, proc, n)
	case LxK.EXIT:
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
	cond := n.Leaves[0]
	bl := n.Leaves[1]
	err := checkExpr(M, proc, cond)
	if err != nil {
		return err
	}

	err = checkExprType(M, cond)
	if err != nil {
		return err
	}

	if !cond.T.Equals(T.T_Bool) {
		return msg.ExpectedBool(M, cond)
	}

	err = checkBlock(M, proc, bl)
	if err != nil {
		return err
	}
	return nil
}

func checkDoWhile(M *mod.Module, proc *mod.Proc, n *mod.Node) *Error {
	cond := n.Leaves[1]
	bl := n.Leaves[0]

	err := checkExpr(M, proc, cond)
	if err != nil {
		return err
	}

	err = checkExprType(M, cond)
	if err != nil {
		return err
	}

	if !cond.T.Equals(T.T_Bool) {
		return msg.ExpectedBool(M, cond)
	}

	err = checkBlock(M, proc, bl)
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
	exp := n.Leaves[1]
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

func checkSet(M *mod.Module, proc *mod.Proc, n *mod.Node) *Error {
	left := n.Leaves[0]
	op := n.Leaves[1]
	right := n.Leaves[2]

	err := checkAssignees(M, proc, left)
	if err != nil {
		return err
	}

	if op.Lex == LxK.PLUS_PLUS ||
		op.Lex == LxK.MINUS_MINUS {
		if len(left.Leaves) != 1 {
			return msg.ErrorInvalidNumberOfAssignees(M, n)
		}
		assignee := left.Leaves[0]
		if assignee.MultiRet {
			return msg.ErrorCannotUseMultipleValuesInExpr(M, n)
		}
		if !(T.IsInteger(assignee.T) ||
			T.IsPtr(assignee.T) ||
			T.IsStruct(assignee.T)) {
			return msg.ErrorInvalidTypeForExpr(M, op, assignee, "numerical, pointer or struct")
		}
		return nil
	}

	err = checkExpr(M, proc, right)
	if err != nil {
		return err
	}

	if (right.MultiRet || len(left.Leaves) > 1) &&
		op.Lex != LxK.ASSIGNMENT {
		return msg.ErrorCanOnlyUseNormalAssignment(M, op)
	}

	if !right.MultiRet && len(left.Leaves) > 1 ||
		right.MultiRet && len(left.Leaves) == 1 {
		return msg.ErrorMismatchedAssignment(M, n)
	}

	if right.MultiRet {
		err := checkMultiAssignment(M, left, right)
		if err != nil {
			return err
		}
	} else {
		if T.IsVoid(right.T) {
			return msg.ErrorCannotUseVoid(M, right)
		}
		leftside := left.Leaves[0]
		if T.IsPtr(leftside.T) {
			if op.Lex != LxK.ASSIGNMENT && !T.IsInteger(right.T) {
				return msg.ExpectedInteger(M, op, right.T)
			}
			if op.Lex == LxK.ASSIGNMENT && !T.IsPtr(right.T) {
				return msg.ErrorMismatchedTypesInAssignment(M, leftside, right)
			}
		} else {
			if !leftside.T.Equals(right.T) {
				return msg.ErrorMismatchedTypesInAssignment(M, leftside, right)
			}
			if op.Lex != LxK.ASSIGNMENT && !T.IsInteger(leftside.T) {
				return msg.ExpectedInteger(M, op, left.T)
			}
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
		case LxK.IDENTIFIER:
			err := checkIdAssignee(M, proc, assignee)
			if err != nil {
				return err
			}
		case LxK.AT:
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
	d := getLocal(proc, assignee.Text)
	if d != nil {
		assignee.T = d.T
		return nil
	}
	_, ok := M.Globals[assignee.Text]
	if ok {
		return msg.ErrorCannotAssignGlobal(M, assignee)
	}
	return msg.ErrorNameNotDefined(M, assignee)
}

func getLocal(proc *mod.Proc, name string) *mod.Local {
	posSy, ok := proc.ArgMap[name]
	if ok {
		return posSy.Local
	}
	def, ok := proc.Vars[name]
	if ok {
		return def.Local
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
	case LxK.IDENTIFIER:
		return checkID(M, proc, n, true)
	case LxK.SIZEOF:
		return checkSizeof(M, proc, n)
	case LxK.DOUBLECOLON:
		return checkExternalID(M, n)
	case LxK.I64_LIT, LxK.I32_LIT, LxK.I16_LIT, LxK.I8_LIT,
		LxK.U64_LIT, LxK.U32_LIT, LxK.U16_LIT, LxK.U8_LIT,
		LxK.FALSE, LxK.TRUE, LxK.PTR_LIT, LxK.STRING_LIT,
		LxK.CHAR_LIT:
		n.T = termToType(n.Lex)
		return nil
	case LxK.NEG, LxK.BITWISENOT:
		return unaryOp(M, proc, n, integer, outSame)
	case LxK.PLUS:
		return checkAdd(M, proc, n)
	case LxK.MINUS:
		return checkSub(M, proc, n)
	case LxK.MULTIPLICATION,
		LxK.DIVISION, LxK.REMAINDER, LxK.BITWISEAND,
		LxK.BITWISEXOR, LxK.BITWISEOR, LxK.SHIFTLEFT,
		LxK.SHIFTRIGHT:
		return binaryOp(M, proc, n, integer, outSame)
	case LxK.EQUALS, LxK.DIFFERENT:
		return binaryOp(M, proc, n, comparable, outBool)
	case LxK.MORE, LxK.MOREEQ, LxK.LESS, LxK.LESSEQ:
		return binaryOp(M, proc, n, basic, outBool)
	case LxK.AND, LxK.OR:
		return binaryOp(M, proc, n, _bool, outBool)
	case LxK.COLON:
		return conversion(M, proc, n)
	case LxK.CALL:
		return checkCall(M, proc, n)
	case LxK.AT:
		return checkDeref(M, proc, n)
	case LxK.NOT:
		return unaryOp(M, proc, n, _bool, outBool)
	case LxK.DOT:
		return dotAccess(M, proc, n)
	case LxK.ARROW:
		return arrowAccess(M, proc, n)
	}
	return nil
}

func checkSizeof(M *mod.Module, proc *mod.Proc, n *mod.Node) *Error {
	typeOp := n.Leaves[0]
	dot := n.Leaves[1]
	// special cases of sizeof: data declarations, struct fields
	if dot != nil {
		var sy *mod.Global
		switch typeOp.Lex {
		case LxK.IDENTIFIER: // struct
			sy = M.GetSymbol(typeOp.Text)
		case LxK.DOUBLECOLON: // external struct
			mod := typeOp.Leaves[0].Text
			id := typeOp.Leaves[1].Text
			sy = M.GetExternalSymbol(mod, id)
		}
		if sy == nil {
			return msg.ErrorNameNotDefined(M, n)
		}
		if sy.Kind != GK.Struct {
			return msg.ErrorExpectedStruct(M, n)
		}
		field := dot.Leaves[0]
		_, ok := sy.Struct.FieldMap[field.Text]
		if ok {
			return msg.FieldNotDefined(M, n)
		}
		n.T = T.T_I32
		return nil
	}
	switch typeOp.Lex {
	case LxK.IDENTIFIER: // data declarations and structs
		sy := M.GetSymbol(typeOp.Text)
		if sy == nil {
			return msg.ErrorNameNotDefined(M, n)
		}
		if sy.Kind != GK.Data && sy.Kind != GK.Struct {
			return msg.ErrorInvalidSizeof(M, n)
		}
	case LxK.DOUBLECOLON: // external data declarations
		moduleName := typeOp.Leaves[0].Text
		symbolName := typeOp.Leaves[1].Text
		sy := M.GetExternalSymbol(moduleName, symbolName)
		if sy == nil {
			return msg.ErrorNameNotDefined(M, n)
		}
		if sy.Kind != GK.Data && sy.Kind != GK.Struct {
			return msg.ErrorInvalidSizeof(M, n)
		}
	default:
		t, err := getType(M, typeOp)
		if err != nil {
			return err
		}
		typeOp.T = t
	}
	n.T = T.T_I32
	return nil
}

func conversion(M *mod.Module, proc *mod.Proc, n *mod.Node) *Error {
	err := checkExpr(M, proc, n.Leaves[1])
	if err != nil {
		return err
	}
	n.T, err = getType(M, n.Leaves[0])
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
		n.MultiRet = true
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

	if sy.Kind == GK.Struct {
		return msg.ErrorInvalidUseForStruct(M, dcolon)
	}
	t := getSymbolType(sy)
	dcolon.Leaves[1].T = t
	dcolon.T = t
	return nil
}

func checkID(M *mod.Module, proc *mod.Proc, id *mod.Node, disallow bool) *Error {
	if proc != nil { // means we're inside a procedure
		local := getLocal(proc, id.Text)
		if local != nil {
			id.T = local.T
			return nil
		}
	}
	global, ok := M.Globals[id.Text]
	if ok {
		if global.Kind == GK.Struct && disallow {
			return msg.ErrorInvalidUseForStruct(M, id)
		}
		id.T = getSymbolType(global)
		if global.External {
			id.T = global.N.T
		}
		return nil
	}
	return msg.ErrorNameNotDefined(M, id)
}

// as if the symbol is inside an expression
func getSymbolType(sy *mod.Global) *T.Type {
	switch sy.Kind {
	case GK.Data:
		return sy.Data.Type
	case GK.Proc:
		return sy.Proc.Type
	case GK.Const:
		return sy.Const.Type
	case GK.Struct:
		return nil
	default:
		panic("unreachable 820")
	}
}

func termToType(tp LxK.LexKind) *T.Type {
	switch tp {
	case LxK.I64_LIT:
		return T.T_I64
	case LxK.I32_LIT:
		return T.T_I32
	case LxK.I16_LIT:
		return T.T_I16
	case LxK.I8_LIT:
		return T.T_I8
	case LxK.U64_LIT:
		return T.T_U64
	case LxK.U32_LIT:
		return T.T_U32
	case LxK.U16_LIT:
		return T.T_U16
	case LxK.U8_LIT:
		return T.T_U8
	case LxK.CHAR_LIT:
		return T.T_I8
	case LxK.STRING_LIT:
		return T.T_Ptr
	case LxK.TRUE:
		return T.T_Bool
	case LxK.FALSE:
		return T.T_Bool
	case LxK.PTR_LIT:
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
	Description: "integer, ptr or bool",
	Checker:     T.IsBasic,
}

var _bool = class{
	Description: "bool",
	Checker:     T.IsBool,
}

var integer = class{
	Description: "integer",
	Checker:     T.IsInteger,
}

var comparable = class{
	Description: "all types",
	Checker:     T.IsBasicOrProc,
}

var ptr = class{
	Description: "ptr",
	Checker:     T.IsPtr,
}

func checkAdd(M *mod.Module, proc *mod.Proc, op *mod.Node) *Error {
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

	if T.IsPtr(left.T) {
		if !T.IsInteger(right.T) {
			return msg.ErrorInvalidTypeForExpr(M, op, right, "integer")
		}
		op.T = T.T_Ptr
		return nil
	} else if T.IsPtr(right.T) {
		if !T.IsInteger(left.T) {
			return msg.ErrorInvalidTypeForExpr(M, op, left, "integer")
		}
		op.T = T.T_Ptr
		return nil
	} else {
		if !integer.Checker(left.T) {
			return msg.ErrorInvalidTypeForExpr(M, op, left, integer.Description)
		}
		if !integer.Checker(right.T) {
			return msg.ErrorInvalidTypeForExpr(M, op, right, integer.Description)
		}
		if !left.T.Equals(right.T) {
			return msg.ErrorOperationBetweenUnequalTypes(M, op)
		}
		op.T = left.T
		return nil
	}
}

func checkSub(M *mod.Module, proc *mod.Proc, op *mod.Node) *Error {
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

	if T.IsPtr(right.T) {
		return msg.ErrorInvalidTypeForExpr(M, op, right, "integer")
	}
	if T.IsPtr(left.T) {
		if !T.IsInteger(right.T) {
			return msg.ErrorInvalidTypeForExpr(M, op, right, "integer")
		}
		op.T = T.T_Ptr
		return nil
	} else {
		if !integer.Checker(left.T) {
			return msg.ErrorInvalidTypeForExpr(M, op, left, integer.Description)
		}
		if !integer.Checker(right.T) {
			return msg.ErrorInvalidTypeForExpr(M, op, right, integer.Description)
		}
		if !left.T.Equals(right.T) {
			return msg.ErrorOperationBetweenUnequalTypes(M, op)
		}
		op.T = left.T
		return nil
	}
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
		return msg.ErrorInvalidTypeForExpr(M, op, left, c.Description)
	}

	if !c.Checker(right.T) {
		return msg.ErrorInvalidTypeForExpr(M, op, right, c.Description)
	}

	if !left.T.Equals(right.T) {
		return msg.ErrorOperationBetweenUnequalTypes(M, op)
	}

	op.T = der(left.T, right.T)
	return nil
}

func checkExprType(M *mod.Module, n *mod.Node) *Error {
	if n.MultiRet {
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
		return msg.ErrorInvalidTypeForExpr(M, op, operand, c.Description)
	}

	op.T = der(operand.T)
	return nil
}

func checkDeref(M *mod.Module, proc *mod.Proc, n *mod.Node) *Error {
	exp := n.Leaves[1]
	t := n.Leaves[0]
	var err *Error
	t.T, err = getType(M, t)
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
func dotAccess(M *mod.Module, proc *mod.Proc, n *mod.Node) *Error {
	leftExpr := n.Leaves[1]
	field := n.Leaves[0]

	switch leftExpr.Lex {
	case LxK.IDENTIFIER:
		err := checkID(M, proc, leftExpr, false)
		if err != nil {
			return err
		}
		sy := M.GetSymbol(leftExpr.Text)
		if sy != nil && sy.Kind == GK.Struct {
			return checkStructField(M, sy, n, field)
		}
	case LxK.DOUBLECOLON:
		mod := n.Leaves[0].Text
		id := n.Leaves[1].Text
		global := M.GetExternalSymbol(mod, id)
		if global == nil {
			return msg.ErrorNameNotDefined(M, n)
		}
		if global.Kind == GK.Struct {
			return checkStructField(M, global, n, field)
		} else {
			leftExpr.T = getSymbolType(global)
		}
	default:
		err := checkExpr(M, proc, leftExpr)
		if err != nil {
			return err
		}
	}

	if leftExpr.MultiRet {
		return msg.ErrorCannotUseMultipleValuesInExpr(M, n)
	} else if leftExpr.T == nil {
		return msg.ErrorBadType(M, n)
	} else if !T.IsStruct(leftExpr.T) {
		return msg.ErrorExpectedStruct(M, n)
	}

	_, ok := leftExpr.T.Struct.Field(field.Text)
	if ok {
		return msg.FieldNotDefined(M, field)
	}
	n.T = T.T_Ptr
	return nil
}

func checkStructField(M *mod.Module, sy *mod.Global, n, field *mod.Node) *Error {
	_, ok := sy.Struct.FieldMap[field.Text]
	if !ok {
		return msg.FieldNotDefined(M, n)
	}
	n.T = T.T_I32
	return nil
}

// p->field where p is an expression of struct type and field is a
// valid field in the struct, yields (p + STRUCT.field)@fieldtype,
// where fieldtype is the type specified at the struct declaration
func arrowAccess(M *mod.Module, proc *mod.Proc, n *mod.Node) *Error {
	leftExpr := n.Leaves[1]
	field := n.Leaves[0]
	err := checkExpr(M, proc, leftExpr)
	if err != nil {
		return err
	}
	if leftExpr.MultiRet {
		return msg.ErrorCannotUseMultipleValuesInExpr(M, n)
	} else if leftExpr.T == nil {
		return msg.ErrorBadType(M, n)
	} else if !T.IsStruct(leftExpr.T) {
		return msg.ErrorExpectedStruct(M, n)
	}

	t := leftExpr.T.Struct.Typeof(field.Text)
	if t == nil {
		return msg.FieldNotDefined(M, field)
	}
	n.T = t
	return nil
}
