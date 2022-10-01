package messages

import (
	"mpc/frontend/ir"
	"mpc/frontend/errors"
	. "mpc/frontend/util/errors"
	et "mpc/frontend/enums/errType"
	"strconv"
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

/*
func ErrorInvalidDependencyCycle(M *ast.Module, prev []*ast.Dependency, dep *ast.Dependency) *errors.CompilerError {
	ninfoList := []*NodeInfo{}
	for _, item := range prev {
		ni := NewNodeInfo(item.Source, "references")
		ninfoList = append(ninfoList, ni)
	}
	ni := NewNodeInfo(dep.Source, "forms a invalid cycle")
	ninfoList = append(ninfoList, ni)
	return NewSemanticError(M, et.InvalidDependencyCycle, ninfoList...)
}
*/

func ErrorOperationBetweenUnequalTypes(M *ir.Module, op *ir.Node) *errors.CompilerError {
	left := op.Leaves[0]
	right := op.Leaves[1]

	excOp    := NewNodeInfo(op, "Operation between unequal types")
	excLeft  := NewNodeInfo(left, left.T.String())
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
	info := NewNodeInfo(n, "expected to be of type: " + sy.Mem.Type.String())
	return NewSemanticError(M, et.InvalidInitForMemType, info)
}

func ErrorNameNotDefined(M *ir.Module, n *ir.Node) *errors.CompilerError {
	info := NewNodeInfo(n, "name is not defined")
	return NewSemanticError(M, et.NameNotDefined, info)
}

func ErrorCanOnlyIndexMemory(M *ir.Module, global *ir.Symbol, n *ir.Node) *errors.CompilerError {
	info := NewNodeInfo(n, "can only index memory")
	source := NewNodeInfo(global.N, "name is: " + global.T.String())
	return NewSemanticError(M, et.CanOnlyIndexMemory, info, source)
}

func ErrorCannotIndexLocal(M *ir.Module, local *ir.Symbol, n *ir.Node) *errors.CompilerError {
	info := NewNodeInfo(n, "can only index memory")
	source := NewNodeInfo(local.N, "name is local " + local.Type.String())
	return NewSemanticError(M, et.CanOnlyIndexMemory, info, source)
}

func ErrorBadIndex(M *ir.Module, n *ir.Node) *errors.CompilerError {
	info := NewNodeInfo(n, "can only index memory")
	return NewSemanticError(M, et.CanOnlyIndexMemory, info)
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
	source := NewNodeInfo(global.N, "is a: " + global.T.String())
	return NewSemanticError(M, et.ExpectedConst, info, source)
}

func ErrorMemResAllowsOnlyIntAndChar(M *ir.Module, n *ir.Node) *errors.CompilerError {
	info := NewNodeInfo(n, "must be int or char")
	return NewSemanticError(M, et.InvalidMemResTerm, info)
}

func ErrorMismatchedTypeForArgument(M *ir.Module, param *ir.Node, arg *ir.Symbol) *errors.CompilerError {
	info := NewNodeInfo(param, "mismatched type in Call, has type: " + param.T.String())
	source := NewNodeInfo(arg.N, "expected type: " + arg.Type.String())
	return NewSemanticError(M, et.MismatchedTypeForArgument, info, source)
}

func ErrorInvalidNumberOfArgs(M *ir.Module, callee *ir.Proc, n *ir.Node) *errors.CompilerError {
	expected := strconv.Itoa(len(callee.Args))
	info := NewNodeInfo(n, "invalid number of arguments, expected: " + expected)
	return NewSemanticError(M, et.InvalidNumberOfArgs, info)
}

func ErrorExpectedProcedure(M *ir.Module, global *ir.Symbol, n *ir.Node) *errors.CompilerError {
	info := NewNodeInfo(n, "is not a procedure")
	source := NewNodeInfo(global.N, "defined here")
	return NewSemanticError(M, et.ExpectedProcedure, info, source)
}

func ErrorExpectedProcedureGotLocal(M *ir.Module, local *ir.Symbol, n *ir.Node) *errors.CompilerError {
	info := NewNodeInfo(n, "is not a procedure")
	source := NewNodeInfo(local.N, "defined here")
	return NewSemanticError(M, et.ExpectedProcedure, info, source)
}

func ErrorInvalidNumberOfReturns(M *ir.Module, proc *ir.Proc, n *ir.Node) *errors.CompilerError {
	expected := strconv.Itoa(len(proc.Rets))
	info := NewNodeInfo(n, "invalid number of returns")
	source := NewNodeInfo(proc.N, "expected: " + expected)
	return NewSemanticError(M, et.InvalidNumberOfReturns, info, source)
}

func ErrorUnmatchingReturns(M *ir.Module, proc *ir.Proc, retN *ir.Node, i int) *errors.CompilerError {
	ret := proc.Rets[i]
	info := NewNodeInfo(retN, "mismatched type in return, has type: " + retN.T.String())
	source := NewNodeInfo(proc.N, "expected type: " + ret.String())
	return NewSemanticError(M, et.MismatchedReturnType, info, source)
}

func ErrorExpectedMem(M *ir.Module, global *ir.Symbol, n *ir.Node) *errors.CompilerError {
	info := NewNodeInfo(n, "is not a memory region")
	source := NewNodeInfo(global.N, "defined here")
	return NewSemanticError(M, et.ExpectedMem, info, source)
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
	info := NewNodeInfo(n, "invalid number of assignments: "+ has)
	source := NewNodeInfo(proc.N, "expected: " + expected)
	return NewSemanticError(M, et.MismatchedMultiRetAssignment, info, source)
}

func ErrorMismatchedTypesInMultiAssignment(M *ir.Module, proc *ir.Symbol, assignee *ir.Node, i int) *errors.CompilerError {
	ret := proc.Proc.Rets[i]
	info := NewNodeInfo(assignee, "mismatched type in assignment, has type: " + assignee.T.String())
	source := NewNodeInfo(proc.N, "expected type: " + ret.String())
	return NewSemanticError(M, et.MismatchedTypeInMultiRetAssign, info, source)
}

func ErrorMismatchedTypesInAssignment(M *ir.Module, assignee *ir.Node, value *ir.Node) *errors.CompilerError {
	info := NewNodeInfo(assignee, "mismatched type in assignment, has type: " + assignee.T.String())
	source := NewNodeInfo(value, "expected type: " + value.T.String())
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

func ErrorCannotUseSyscallAsValue(M *ir.Module, n *ir.Node) *errors.CompilerError {
	info := NewNodeInfo(n, "invalid use of syscall")
	return NewSemanticError(M, et.CannotUseSyscallInExpr, info)
}

func ErrorCannotUseVoid(M *ir.Module, n *ir.Node) *errors.CompilerError {
	info := NewNodeInfo(n, "cannot use void as value")
	return NewSemanticError(M, et.CannotUseVoid, info)
}
