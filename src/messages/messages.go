package messages

import (
	T "github.com/padeir0/pir/types"
	. "mpc/core"
	et "mpc/core/errorkind"
	ir "mpc/core/module"
	. "mpc/core/util"
	"strconv"
	"strings"
)

func ErrorNameAlreadyDefined(M *ir.Module, newName *ir.Node) *Error {
	return NewSemanticError(M, et.NameAlreadyDefined, newName, "name already exists")
}

func ErrorDuplicatedExport(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.DuplicatedExport, n, "named already exported")
}

func ErrorExportingUndefName(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.ExportingUndefName, n, "name not defined in this module")
}

func NameNotExported(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.NameNotExported, n, "name not defined in module")
}

func ErrorOperationBetweenUnequalTypes(M *ir.Module, op *ir.Node) *Error {
	left := op.Leaves[0]
	right := op.Leaves[1]
	msg := "Operation between unequal types: " + left.T.String() + " and " + right.T.String()
	return NewSemanticError(M, et.OperationBetweenUnequalTypes, op, msg)
}

func ErrorNameNotDefined(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.NameNotDefined, n, "name is not defined")
}

func ErrorBadDeref(M *ir.Module, n *ir.Node, t *T.Type) *Error {
	return NewSemanticError(M, et.CanOnlyDerefPointers, n, "can only index pointers (type: "+t.String()+")")
}

func ErrorCannotAssignGlobal(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.CanOnlyAssignLocal, n, "cannot assign global")
}

func ErrorNotAssignable(M *ir.Module, assignee *ir.Node) *Error {
	return NewSemanticError(M, et.NotAssignable, assignee, "not assignable")
}

func ErrorInvalidType(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.InvalidType, n, "invalid type in expression")
}

func ErrorCannotUseMultipleValuesInExpr(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.InvalidType, n, "cannot use multi returns in expressions")
}

func ErrorMismatchedTypeForArgument(M *ir.Module, param *ir.Node, arg *T.Type) *Error {
	return NewSemanticError(M, et.MismatchedTypeForArgument, param, "mismatched type in Call, has type: "+param.T.String()+", expected: "+arg.String())
}

func ErrorInvalidNumberOfArgs(M *ir.Module, callee *T.ProcType, n *ir.Node) *Error {
	expected := strconv.Itoa(len(callee.Args))
	return NewSemanticError(M, et.InvalidNumberOfArgs, n, "invalid number of arguments, expected: "+expected)
}

func ErrorExpectedProcedure(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.ExpectedProcedure, n, "is not a procedure (type: "+n.T.String()+")")
}

func ErrorExpectedBasicType(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.ExpectedBasicType, n, "is not of a basic type (type: "+n.T.String()+")")
}

func ErrorInvalidNumberOfReturns(M *ir.Module, proc *ir.Proc, n *ir.Node) *Error {
	return NewSemanticError(M, et.InvalidNumberOfReturns, n, "invalid number of returns for procedure: "+proc.T.String())
}

func ErrorUnmatchingReturns(M *ir.Module, proc *ir.Proc, retN *ir.Node, i int) *Error {
	ret := proc.Rets[i]
	return NewSemanticError(M, et.MismatchedReturnType, retN, "mismatched type in return, has type: "+retN.T.String()+"expected type: "+ret.String())
}

func ErrorExpectedMem(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.ExpectedMem, n, "is not a memory region")
}

func ErrorMismatchedMultiRetAssignment(M *ir.Module, proc *ir.Symbol, n *ir.Node, left *ir.Node) *Error {
	has := strconv.Itoa(len(left.Leaves))
	expected := strconv.Itoa(len(proc.Proc.Rets))
	return NewSemanticError(M, et.MismatchedMultiRetAssignment, n, "invalid number of assignments: "+has+", expected: "+expected)
}

func ErrorMismatchedTypesInMultiAssignment(M *ir.Module, proc *ir.Symbol, assignee *ir.Node, i int) *Error {
	ret := proc.Proc.Rets[i]
	return NewSemanticError(M, et.MismatchedTypeInMultiRetAssign, assignee, "mismatched type in assignment, has type: "+assignee.T.String()+", expected type: "+ret.String())
}

func ErrorMismatchedTypesInAssignment(M *ir.Module, assignee *ir.Node, value *ir.Node) *Error {
	return NewSemanticError(M, et.MismatchedTypeInAssign, assignee, "mismatched type in assignment, has type: "+assignee.T.String()+", expected type: "+value.T.String())
}

func ErrorMismatchedAssignment(M *ir.Module, assignee *ir.Node) *Error {
	return NewSemanticError(M, et.MismatchedTypeInAssign, assignee, "mismatched number of expressions in assignment")
}

func ErrorInvalidClassForExpr(M *ir.Module, op, operand *ir.Node, descr string) *Error {
	return NewSemanticError(M, et.InvalidClassforExpr, op, "invalid type for operator "+op.Text+" has "+operand.T.String()+", expected "+descr)
}

func ErrorCannotUseVoid(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.CannotUseVoid, n, "can't use empty return in expression")
}

func ErrorCanOnlyUseNormalAssignment(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.CanOnlyUseNormalAssignment, n, "can only use normal assignment '=' when assigning multiple values")
}

func ExpectedNumber(M *ir.Module, op *ir.Node, t *T.Type) *Error {
	return NewSemanticError(M, et.ExpectedNumber, op, "operation can only be done on numbers (type: "+t.String()+")")
}

func ExitMustBeI8(M *ir.Module, exp *ir.Node) *Error {
	return NewSemanticError(M, et.ExitMustBeI8, exp, "exit must be type i8 (type: "+exp.T.String()+")")
}

func ErrorPtrCantBeUsedAsMemSize(M *ir.Module, init *ir.Node) *Error {
	return NewSemanticError(M, et.PtrCantBeUsedAsMemSize, init, "can't use pointer type as size of memory")
}

func ErrorInvalidProp(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.InvalidProp, n, "is not a valid property (only 'size' allowed)")
}

func NotAllCodePathsReturnAValue(M *ir.Module, p *ir.Proc) *Error {
	return NewSemanticError(M, et.NotAllCodePathsReturnAValue, p.N, "not all code paths return a value")
}

func InvalidMain(M *ir.Module, sy *ir.Symbol) *Error {
	return NewSemanticError(M, et.InvalidMain, sy.N, "invalid type for main function: must be proc[][]")
}

func ProgramWithoutEntry(M *ir.Module) *Error {
	return NewSemanticError(M, et.NoEntryPoint, M.Root, "program has no entry point")
}

func AmbiguousFilesInFolder(M *ir.Module, n *ir.Node, found []string, modID string) *Error {
	msg := "Multiple modules possible for " + modID +
		": " + strings.Join(found, ", ")
	if M != nil && n != nil {
		return NewSemanticError(M, et.AmbiguousModuleName, n, msg)
	}
	return &Error{
		Code:    et.AmbiguousModuleName,
		Message: msg,
	}
}

func ModuleNotFound(M *ir.Module, n *ir.Node, baseFolder string, modID string) *Error {
	msg := "module " + modID + " not found in folder " + baseFolder
	if M != nil && n != nil {
		return NewSemanticError(M, et.ModuleNotFound, n, msg)
	}
	return &Error{
		Code:    et.ModuleNotFound,
		Message: msg,
	}
}

func ErrorInvalidDependencyCycle(M *ir.Module, prev []*ir.Dependency, dep *ir.Dependency) *Error {
	msg := dep.M.Name + ": forms a invalid cycle"
	for _, item := range prev {
		msg += item.M.Name + ": is imported by ^\n"
	}
	return NewSemanticError(M, et.InvalidDependencyCycle, dep.Source, msg)
}

func ExpectedBool(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.ExpectedBool, n, "expected expression of bool type, instead got: "+n.T.String())
}
