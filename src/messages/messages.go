package messages

import (
	"math/big"
	. "mpc/core"
	et "mpc/core/errorkind"
	ir "mpc/core/module"
	sv "mpc/core/severity"
	T "mpc/core/types"
	. "mpc/core/util"
	"strconv"
	"strings"
)

func ErrorNameAlreadyDefined(M *ir.Module, newName *ir.Node, text string) *Error {
	return NewSemanticError(M, et.NameAlreadyDefined, newName, "name '"+text+"' already exists")
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
	msg := "Operation between unequal types: " + left.Type.String() + " and " + right.Type.String()
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

func ErrorMismatchedTypeForArgument(M *ir.Module, param *ir.Node, arg string) *Error {
	return NewSemanticError(M, et.MismatchedTypeForArgument, param, "mismatched type in Call, has type: "+param.Type.String()+", expected: "+arg)
}

func ErrorInvalidNumberOfArgs(M *ir.Module, expected int, n *ir.Node) *Error {
	exp := strconv.Itoa(expected)
	return NewSemanticError(M, et.InvalidNumberOfArgs, n, "invalid number of arguments, expected: "+exp)
}

func ErrorNotCallable(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.NotCallable, n, "is not callable (type: "+n.Type.String()+")")
}

func ErrorExpectedStruct(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.ExpectedStruct, n, "is not a struct")
}

func ErrorExpectedBasicOrProc(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.ExpectedBasicOrProcType, n, "is not of a basic or proc type (type: "+n.Type.String()+")")
}

func ErrorInvalidNumberOfReturns(M *ir.Module, proc *ir.Proc, n *ir.Node) *Error {
	return NewSemanticError(M, et.InvalidNumberOfReturns, n, "invalid number of returns for procedure: "+proc.Type.String())
}

func ErrorUnmatchingReturns(M *ir.Module, proc *ir.Proc, retN *ir.Node, i int) *Error {
	ret := proc.Rets[i]
	return NewSemanticError(M, et.MismatchedReturnType, retN, "mismatched type in return, has type: "+retN.Type.String()+"expected type: "+ret.String())
}

func ErrorExpectedData(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.ExpectedData, n, "is not a data region")
}

func ErrorMismatchedMultiRetAssignment(M *ir.Module, proc *T.Type, n *ir.Node, left *ir.Node) *Error {
	has := strconv.Itoa(len(left.Leaves))
	expected := strconv.Itoa(len(proc.Proc.Rets))
	return NewSemanticError(M, et.MismatchedMultiRetAssignment, n, "invalid number of assignments: "+has+", expected: "+expected)
}

func ErrorMismatchedTypesInMultiAssignment(M *ir.Module, proc *T.Type, assignee *ir.Node, i int) *Error {
	ret := proc.Proc.Rets[i]
	return NewSemanticError(M, et.MismatchedTypeInMultiRetAssign, assignee, "mismatched type in assignment, has type: "+assignee.Type.String()+", expected type: "+ret.String())
}

func ErrorMismatchedTypesInAssignment(M *ir.Module, assignee *ir.Node, value *ir.Node) *Error {
	return NewSemanticError(M, et.MismatchedTypeInAssign, assignee, "mismatched type in assignment, has type: "+assignee.Type.String()+", expected type: "+value.Type.String())
}

func ErrorMismatchedAssignment(M *ir.Module, assignee *ir.Node) *Error {
	return NewSemanticError(M, et.MismatchedTypeInAssign, assignee, "mismatched number of expressions in assignment")
}

func ErrorInvalidTypeForExpr(M *ir.Module, op, operand *ir.Node, descr string) *Error {
	return NewSemanticError(M, et.InvalidTypeForExpr, op, "invalid type for operator "+op.Text+" has "+operand.Type.String()+", expected "+descr)
}

func ErrorCannotUseVoid(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.CannotUseVoid, n, "can't use empty return in expression")
}

func ErrorCanOnlyUseNormalAssignment(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.CanOnlyUseNormalAssignment, n, "can only use normal assignment '=' when assigning multiple values")
}

func ExpectedInteger(M *ir.Module, n *ir.Node, t *T.Type) *Error {
	return NewSemanticError(M, et.ExpectedIntegers, n, "expected integer (instead got: "+t.String()+")")
}

func ExitMustBeI8(M *ir.Module, exp *ir.Node) *Error {
	return NewSemanticError(M, et.ExitMustBeI8, exp, "exit must be type i8 (type: "+exp.Type.String()+")")
}

func ErrorPtrCantBeUsedAsDataSize(M *ir.Module, init *ir.Node) *Error {
	return NewSemanticError(M, et.PtrCantBeUsedAsDataSize, init, "can't use pointer type as size of data")
}

func ErrorInvalidProp(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.InvalidProp, n, "is not a valid property (only 'size' allowed)")
}

func NotAllCodePathsReturnAValue(M *ir.Module, p *ir.Proc) *Error {
	return NewSemanticError(M, et.NotAllCodePathsReturnAValue, p.N, "not all code paths return a value")
}

func InvalidMain(M *ir.Module, sy *ir.Global) *Error {
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
		Code:     et.AmbiguousModuleName,
		Severity: sv.Error,
		Message:  msg,
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
	msg := "'" + dep.M.Name + "' forms a invalid cycle: ("
	for _, item := range prev {
		msg += item.M.Name + ", "
	}
	msg += dep.M.Name + ")"
	return NewSemanticError(M, et.InvalidDependencyCycle, dep.Source, msg)
}

func ErrorInvalidSymbolCycle(M *ir.Module, prev []ir.SyField, sf ir.SyField) *Error {
	msg := "'" + sf.Name() + "' forms a invalid cycle ("
	for _, item := range prev {
		msg += item.Name() + ", "
	}
	msg += sf.Name() + ")"
	return NewSemanticError(M, et.InvalidSymbolCycle, sf.Sy.N, msg)
}

func ExpectedBool(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.ExpectedBool, n, "expected expression of bool type, instead got: "+n.Type.String())
}

func NonConstExpr(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.NonConstExpr, n, "expression is not compile-time constant")
}

func CannotUseStringInExpr(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.CannotUseStringInExpr, n, "string literals can't be used inside expressions")
}

func InvalidTypeForConst(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.InvalidTypeForConst, n, "invalid type for constant, must be of a basic type")
}

func ValueOutOfBounds(M *ir.Module, n *ir.Node, res *big.Int) *Error {
	msg := "value '" + res.Text(10) + "' is too big for constant type"
	return NewSemanticError(M, et.ValueOutOfBounds, n, msg)
}

func DoesntMatchBlobAnnot(M *ir.Module, assignee *ir.Node, t *T.Type) *Error {
	msg := "doesn't match blob annotation, has: " + assignee.Type.String() + ", expected: " + t.String()
	return NewSemanticError(M, et.DoesntMatchBlobAnnot, assignee, msg)
}

func ErrorBadType(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.BadType, n, "not a type")
}

func CantImportAll(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.CantImportAll, n, "you can't import all modules in existence")
}

func ErrorOffsetInMultipleFields(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.OffsetInMultipleFields, n, "explicit offset being applied to multiple fields")
}

func ErrorInvalidUseForStruct(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.InvalidUseForStruct, n, "invalid use for struct in expression")
}

func FieldNotDefined(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.FieldNotDefined, n, "field not defined in struct")
}

func ErrorInvalidSizeof(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.InvalidSizeof, n, "invalid sizeof")
}

func ErrorInvalidNumberOfAssignees(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.InvalidNumOfAssignees, n, "invalid number of assignees")
}

func InvalidDataDecl(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.InvalidDataDecl, n, "invalid data declaration")
}

func InvalidStructDecl(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.InvalidStructDecl, n, "invalid struct declaration (either fully explicit or fully implicit)")
}

func UnsizeableType(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.UnsizeableType, n, "type is not sizeable")
}

func InvalidFlag(M *ir.Module, n *ir.Node) *Error {
	return NewSemanticError(M, et.InvalidFlag, n, "invalid flag")
}

func InvalidCC(M *ir.Module, cc *ir.Node) *Error {
	return NewSemanticError(M, et.InvalidCC, cc, "invalid calling convention")
}

func DuplicatedLabel(M *ir.Module, lbl *ir.Node) *Error {
	return NewSemanticError(M, et.DupLabel, lbl, "duplicated label")
}

func InvalidNestedAddr(M *ir.Module, op *ir.Node) *Error {
	return NewSemanticError(M, et.NestedAddress, op, "invalid nested address")
}

func InvalidInstr(M *ir.Module, op *ir.Node) *Error {
	return NewSemanticError(M, et.InvalidInstr, op, "invalid instruction")
}

func InvalidTypeSize(M *ir.Module, op *ir.Node) *Error {
	return NewSemanticError(M, et.InvalidTypeSize, op, "invalid type size")
}

func InvalidOperand(M *ir.Module, op *ir.Node) *Error {
	return NewSemanticError(M, et.InvalidOperand, op, "invalid operand")
}

func ErrorExpectedProc(M *ir.Module, op *ir.Node) *Error {
	return NewSemanticError(M, et.ExpectedProc, op, "expected procedure")
}

func ErrorExportingExternalName(M *ir.Module, op *ir.Node) *Error {
	return NewSemanticError(M, et.ExportExternal, op, "exported external symbol")
}
