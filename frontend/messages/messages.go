package messages

import (
	T "mpc/frontend/Type"
	et "mpc/frontend/enums/errType"
	"mpc/frontend/errors"
	"mpc/frontend/ir"
	. "mpc/frontend/util/errors"
	"strconv"
	"strings"
)

func ErrorNameAlreadyDefined(M *ir.Module, newName *ir.Node, oldName *ir.Node) *errors.CompilerError {
	exc1 := NewNodeInfo(newName, "name already exists")
	exc2 := NewNodeInfo(oldName, "defined here")
	err := NewSemanticError(M, et.NameAlreadyDefined, exc1, exc2)
	return err
}

func ErrorDuplicatedExport(M *ir.Module, n *ir.Node, other *ir.Node) *errors.CompilerError {
	ni1 := NewNodeInfo(n, "previously exported here")
	ni2 := NewNodeInfo(n, "named already exported")
	return NewSemanticError(M, et.DuplicatedExport, ni1, ni2)
}

func ErrorExportingUndefName(M *ir.Module, n *ir.Node) *errors.CompilerError {
	ni := NewNodeInfo(n, "name not defined in this module")
	return NewSemanticError(M, et.ExportingUndefName, ni)
}

func NameNotExported(M *ir.Module, n *ir.Node) *errors.CompilerError {
	ni := NewNodeInfo(n, "name not defined in module")
	return NewSemanticError(M, et.NameNotExported, ni)
}

func ErrorOperationBetweenUnequalTypes(M *ir.Module, op *ir.Node) *errors.CompilerError {
	left := op.Leaves[0]
	right := op.Leaves[1]

	excOp := NewNodeInfo(op, "Operation between unequal types")
	excLeft := NewNodeInfo(left, left.T.String())
	excRight := NewNodeInfo(right, right.T.String())
	return NewSemanticError(M, et.OperationBetweenUnequalTypes, excOp, excLeft, excRight)
}

func ErrorTypeCheckerExpectedName(M *ir.Module, n *ir.Node) *errors.CompilerError {
	info := NewNodeInfo(n, "name is not declared")
	return NewSemanticError(M, et.ExpectedName, info)
}

func ErrorNameResExpectedName(M *ir.Module, n *ir.Node) *errors.CompilerError {
	info := NewNodeInfo(n, "name is not declared")
	return NewSemanticError(M, et.ExpectedName, info)
}

func ErrorInvalidInitForMemType(M *ir.Module, sy *ir.Symbol, n *ir.Node) *errors.CompilerError {
	info := NewNodeInfo(n, "expected to be of type: "+sy.Mem.Type.String())
	return NewSemanticError(M, et.InvalidInitForMemType, info)
}

func ErrorNameNotDefined(M *ir.Module, n *ir.Node) *errors.CompilerError {
	info := NewNodeInfo(n, "name is not defined")
	return NewSemanticError(M, et.NameNotDefined, info)
}

func ErrorBadDeref(M *ir.Module, n *ir.Node, t *T.Type) *errors.CompilerError {
	info := NewNodeInfo(n, "can only index pointers (type: "+t.String()+")")
	return NewSemanticError(M, et.CanOnlyDerefPointers, info)
}

func ErrorCannotAssignGlobal(M *ir.Module, global *ir.Symbol, n *ir.Node) *errors.CompilerError {
	info := NewNodeInfo(n, "cannot assign global")
	source := NewNodeInfo(global.N, "defined here")
	return NewSemanticError(M, et.CanOnlyAssignLocal, info, source)
}

func ErrorNotAssignable(M *ir.Module, assignee *ir.Node) *errors.CompilerError {
	info := NewNodeInfo(assignee, "not assignable")
	return NewSemanticError(M, et.NotAssignable, info)
}

func ErrorInvalidType(M *ir.Module, n *ir.Node) *errors.CompilerError {
	info := NewNodeInfo(n, "invalid type in expression")
	return NewSemanticError(M, et.InvalidType, info)
}

func ErrorCannotUseMultipleValuesInExpr(M *ir.Module, n *ir.Node) *errors.CompilerError {
	info := NewNodeInfo(n, "cannot use multi returns in expressions")
	return NewSemanticError(M, et.InvalidType, info)
}

func ErrorExpectedConst(M *ir.Module, global *ir.Symbol, n *ir.Node) *errors.CompilerError {
	info := NewNodeInfo(n, "id is not a constant")
	source := NewNodeInfo(global.N, "is a: "+global.T.String())
	return NewSemanticError(M, et.ExpectedConst, info, source)
}

func ErrorMemResAllowsOnlyIntAndChar(M *ir.Module, n *ir.Node) *errors.CompilerError {
	info := NewNodeInfo(n, "must be int or char")
	return NewSemanticError(M, et.InvalidMemResTerm, info)
}

func ErrorMismatchedTypeForArgument(M *ir.Module, param *ir.Node, arg *T.Type) *errors.CompilerError {
	info := NewNodeInfo(param, "mismatched type in Call, has type: "+param.T.String()+", expected: "+arg.String())
	return NewSemanticError(M, et.MismatchedTypeForArgument, info)
}

func ErrorInvalidNumberOfArgs(M *ir.Module, callee *T.ProcType, n *ir.Node) *errors.CompilerError {
	expected := strconv.Itoa(len(callee.Args))
	info := NewNodeInfo(n, "invalid number of arguments, expected: "+expected)
	return NewSemanticError(M, et.InvalidNumberOfArgs, info)
}

func ErrorExpectedProcedure(M *ir.Module, n *ir.Node) *errors.CompilerError {
	info := NewNodeInfo(n, "is not a procedure (type: "+n.String()+")")
	return NewSemanticError(M, et.ExpectedProcedure, info)
}

func ErrorExpectedBasicType(M *ir.Module, n *ir.Node) *errors.CompilerError {
	info := NewNodeInfo(n, "is not of a basic type (type: "+n.String()+")")
	return NewSemanticError(M, et.ExpectedBasicType, info)
}

func ErrorInvalidNumberOfReturns(M *ir.Module, proc *ir.Proc, n *ir.Node) *errors.CompilerError {
	expected := strconv.Itoa(len(proc.Rets))
	info := NewNodeInfo(n, "invalid number of returns")
	source := NewNodeInfo(proc.N, "expected: "+expected)
	return NewSemanticError(M, et.InvalidNumberOfReturns, info, source)
}

func ErrorUnmatchingReturns(M *ir.Module, proc *ir.Proc, retN *ir.Node, i int) *errors.CompilerError {
	ret := proc.Rets[i]
	info := NewNodeInfo(retN, "mismatched type in return, has type: "+retN.T.String())
	source := NewNodeInfo(proc.N, "expected type: "+ret.String())
	return NewSemanticError(M, et.MismatchedReturnType, info, source)
}

func ErrorExpectedMem(M *ir.Module, n *ir.Node) *errors.CompilerError {
	info := NewNodeInfo(n, "is not a memory region")
	return NewSemanticError(M, et.ExpectedMem, info)
}

func ErrorExpectedMemGotLocal(M *ir.Module, local *ir.Symbol, n *ir.Node) *errors.CompilerError {
	info := NewNodeInfo(n, "is not a memory region")
	source := NewNodeInfo(local.N, "defined here")
	return NewSemanticError(M, et.ExpectedMem, info, source)
}

func ErrorCantUseStringInExpr(M *ir.Module, n *ir.Node) *errors.CompilerError {
	info := NewNodeInfo(n, "cannot use string in expressions")
	return NewSemanticError(M, et.InvalidUseOfString, info)
}

func ErrorMismatchedMultiRetAssignment(M *ir.Module, proc *ir.Symbol, n *ir.Node, left *ir.Node) *errors.CompilerError {
	has := strconv.Itoa(len(left.Leaves))
	expected := strconv.Itoa(len(proc.Proc.Rets))
	info := NewNodeInfo(n, "invalid number of assignments: "+has)
	source := NewNodeInfo(proc.N, "expected: "+expected)
	return NewSemanticError(M, et.MismatchedMultiRetAssignment, info, source)
}

func ErrorMismatchedTypesInMultiAssignment(M *ir.Module, proc *ir.Symbol, assignee *ir.Node, i int) *errors.CompilerError {
	ret := proc.Proc.Rets[i]
	info := NewNodeInfo(assignee, "mismatched type in assignment, has type: "+assignee.T.String())
	source := NewNodeInfo(proc.N, "expected type: "+ret.String())
	return NewSemanticError(M, et.MismatchedTypeInMultiRetAssign, info, source)
}

func ErrorMismatchedTypesInAssignment(M *ir.Module, assignee *ir.Node, value *ir.Node) *errors.CompilerError {
	info := NewNodeInfo(assignee, "mismatched type in assignment, has type: "+assignee.T.String())
	source := NewNodeInfo(value, "expected type: "+value.T.String())
	return NewSemanticError(M, et.MismatchedTypeInAssign, info, source)
}

func ErrorMismatchedAssignment(M *ir.Module, assignee *ir.Node) *errors.CompilerError {
	info := NewNodeInfo(assignee, "mismatched number of expressions in assignment")
	return NewSemanticError(M, et.MismatchedTypeInAssign, info)
}

func ErrorInvalidClassForExpr(M *ir.Module, n *ir.Node, descr string) *errors.CompilerError {
	info := NewNodeInfo(n, "invalid type "+n.T.String()+", expected "+descr)
	return NewSemanticError(M, et.InvalidClassforExpr, info)
}

func ErrorCannotUseVoid(M *ir.Module, n *ir.Node) *errors.CompilerError {
	info := NewNodeInfo(n, "cannot use void as value")
	return NewSemanticError(M, et.CannotUseVoid, info)
}
func ErrorCanOnlyUseNormalAssignment(M *ir.Module, n *ir.Node) *errors.CompilerError {
	info := NewNodeInfo(n, "can only use normal assignment when assigning multiple values")
	return NewSemanticError(M, et.CanOnlyUseNormalAssignment, info)
}

func ExpectedNumber(M *ir.Module, op *ir.Node, t *T.Type) *errors.CompilerError {
	info := NewNodeInfo(op, "operation can only be done on numbers (type: "+t.String()+")")
	return NewSemanticError(M, et.ExpectedNumber, info)
}

func ExitMustBeI8(M *ir.Module, exp *ir.Node) *errors.CompilerError {
	info := NewNodeInfo(exp, "exit must be type i8 (type: "+exp.T.String()+")")
	return NewSemanticError(M, et.ExitMustBeI8, info)
}

func ErrorPtrCantBeUsedAsMemSize(M *ir.Module, init *ir.Node) *errors.CompilerError {
	info := NewNodeInfo(init, "can't use pointer type as size of memory")
	return NewSemanticError(M, et.PtrCantBeUsedAsMemSize, info)
}

func ErrorInvalidProp(M *ir.Module, n *ir.Node) *errors.CompilerError {
	info := NewNodeInfo(n, "is not a valid property (only 'size' allowed)")
	return NewSemanticError(M, et.ExpectedMem, info)
}

func NotAllCodePathsReturnAValue(M *ir.Module, p *ir.Proc) *errors.CompilerError {
	info := NewNodeInfo(p.N, "not all code paths return a value")
	return NewSemanticError(M, et.NotAllCodePathsReturnAValue, info)
}

func InvalidMain(M *ir.Module, sy *ir.Symbol) *errors.CompilerError {
	main := NewNodeInfo(sy.N, "invalid type for main function: must be proc[][]")
	return NewSemanticError(M, et.InvalidMain, main)
}

func ProgramWithoutEntry(M *ir.Module) *errors.CompilerError {
	start := NewNodeInfo(M.Root, "program has no entry point")
	return NewSemanticError(M, et.NoEntryPoint, start)
}

func AmbiguousFilesInFolder(M *ir.Module, n *ir.Node, found []string, modID string) *errors.CompilerError {
	msg := "Multiple modules possible for " + modID +
		": " + strings.Join(found, ", ")
	if M != nil && n != nil {
		info := NewNodeInfo(n, msg)
		return NewSemanticError(M, et.AmbiguousModuleName, info)
	}
	return &errors.CompilerError{
		Stage: errors.Resolver,
		Type:  et.AmbiguousModuleName,
		Debug: msg,
	}
}

func ModuleNotFound(M *ir.Module, n *ir.Node, baseFolder string, modID string) *errors.CompilerError {
	msg := "module " + modID + " not found in folder " + baseFolder
	if M != nil && n != nil {
		info := NewNodeInfo(n, msg)
		return NewSemanticError(M, et.ModuleNotFound, info)
	}
	return &errors.CompilerError{
		Stage: errors.Resolver,
		Type:  et.ModuleNotFound,
		Debug: msg,
	}
}

func ErrorInvalidDependencyCycle(M *ir.Module, prev []*ir.Dependency, dep *ir.Dependency) *errors.CompilerError {
	ninfoList := []*NodeInfo{}
	for _, item := range prev {
		ni := NewNodeInfo(item.Source, "references")
		ninfoList = append(ninfoList, ni)
	}
	ni := NewNodeInfo(dep.Source, "forms a invalid cycle")
	ninfoList = append(ninfoList, ni)
	return NewSemanticError(M, et.InvalidDependencyCycle, ninfoList...)
}
