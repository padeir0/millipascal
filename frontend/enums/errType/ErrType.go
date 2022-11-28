package errType

import (
	"fmt"
)

type ErrType int

const (
	/* internal (bad) errors */
	InvalidErrType ErrType = iota
	InternalCompilerError
	UnmatchingTypesInInstr

	/* lexer errors */
	InvalidUTF8Rune
	InvalidSymbol

	/* parser errors */
	ExpectedSymbol
	ExpectedProd
	ExpectedEOF

	/* resolver error */
	FileError
	ImportError
	InvalidFileName
	ImportNameColision
	FromImportingUnexportedName
	InvalidDependencyCycle

	/* semantic errors */
	ExpectedName
	NameAlreadyDefined
	ExportingUndefName
	DuplicatedExport
	OperationBetweenUnequalTypes
	InvalidInitForMemType
	NameNotDefined
	CanOnlyDerefPointers
	CanOnlyAssignLocal
	NotAssignable
	InvalidType
	InvalidMemResTerm
	ExpectedConst
	MismatchedTypeForArgument
	InvalidNumberOfArgs
	ExpectedProcedure
	InvalidNumberOfReturns
	MismatchedReturnType
	ExpectedMem
	InvalidUseOfString
	MismatchedMultiRetAssignment
	MismatchedTypeInMultiRetAssign
	CopyTooBig
	InvalidCopy
	MismatchedTypeInAssign
	BadConst
	InvalidClassforExpr
	CannotUseSyscallInExpr
	CannotUseVoid
)

func (et ErrType) String() string {
	v, ok := ErrorCodeMap[et]
	if !ok {
		panic(fmt.Sprintf("%d is not stringified", et))
	}
	return v
}

func (et ErrType) Debug() string {
	if et == InvalidErrType {
		panic("invalid ErrType")
	}
	v, ok := DebugMap[et]
	if !ok {
		panic("ErrType not mapped to string")
	}
	return v
}

var DebugMap = map[ErrType]string{
	InvalidErrType:         "InvalidErrType",
	InternalCompilerError:  "InternalCompilerError",
	UnmatchingTypesInInstr: "UnmatchingTypesInInstr",

	/* resolver errors */
	FileError:                   "FileError",
	ImportError:                 "ImportError",
	InvalidFileName:             "InvalidFileName",
	ImportNameColision:          "ImportNameColision",
	FromImportingUnexportedName: "FromImportingUnexportedName",

	InvalidUTF8Rune: "InvalidUTF8Rune",
	InvalidSymbol:   "InvalidSymbol",

	ExpectedSymbol: "ExpectedSymbol",
	ExpectedProd:   "ExpectedProd",
	ExpectedEOF:    "ExpectedEOF",

	ExpectedName: "ExpectedName",

	NameAlreadyDefined:             "NameAlreadyDefined",
	OperationBetweenUnequalTypes:   "OperationBetweenUnequalTypes",
	DuplicatedExport:               "DuplicatedExport",
	ExportingUndefName:             "ExportingUndefName",
	InvalidDependencyCycle:         "InvalidDependencyCycle",
	InvalidInitForMemType:          "InvalidInitForMemType",
	NameNotDefined:                 "NameNotDefined",
	CanOnlyDerefPointers:             "CanOnlyIndexMemory",
	CanOnlyAssignLocal:             "CanOnlyAssignLocal",
	NotAssignable:                  "NotAssignable",
	InvalidType:                    "InvalidType",
	InvalidMemResTerm:              "InvalidMemResTerm",
	ExpectedConst:                  "ExpectedConst",
	MismatchedTypeForArgument:      "MismatchedTypeForArgument",
	InvalidNumberOfArgs:            "InvalidNumberOfArgs",
	ExpectedProcedure:              "ExpectedProcedure",
	InvalidNumberOfReturns:         "InvalidNumberOfReturns",
	MismatchedReturnType:           "MismatchedReturnType",
	ExpectedMem:                    "ExpectedMem",
	InvalidUseOfString:             "InvalidUseOfString",
	MismatchedMultiRetAssignment:   "MismatchedMultiRetAssignment",
	MismatchedTypeInMultiRetAssign: "MismatchedTypeInMultiRetAssign",
	CopyTooBig:                     "CopyTooBig",
	InvalidCopy:                    "InvalidCopy",
	MismatchedTypeInAssign:         "MismatchedTypeInAssign",
	BadConst:                       "BadConst",
	InvalidClassforExpr:            "InvalidClassforExpr",
	CannotUseSyscallInExpr:         "CannotUseSyscallInExpr",
	CannotUseVoid:                  "CannotUseVoid",
}

var ErrorCodeMap = map[ErrType]string{
	/* internal (bad) errors */
	InvalidErrType:         "I001",
	InternalCompilerError:  "I002",
	UnmatchingTypesInInstr: "I003",

	/* resolver errors */
	/* lexer errors */
	InvalidUTF8Rune: "L001",
	InvalidSymbol:   "L002",

	/* parser errors */
	ExpectedSymbol: "P001",
	ExpectedProd:   "P002",
	ExpectedEOF:    "P003",

	/* semantic errors */
	ExpectedName:                   "S001",
	NameAlreadyDefined:             "S002",
	OperationBetweenUnequalTypes:   "S003",
	DuplicatedExport:               "S004",
	ExportingUndefName:             "S005",
	InvalidDependencyCycle:         "S006",
	FileError:                      "S007",
	ImportError:                    "S008",
	InvalidFileName:                "S009",
	ImportNameColision:             "S010",
	FromImportingUnexportedName:    "S011",
	InvalidInitForMemType:          "S012",
	NameNotDefined:                 "S013",
	CanOnlyDerefPointers:             "S014",
	CanOnlyAssignLocal:             "S015",
	NotAssignable:                  "S016",
	InvalidType:                    "S017",
	InvalidMemResTerm:              "S018",
	ExpectedConst:                  "S019",
	MismatchedTypeForArgument:      "S020",
	InvalidNumberOfArgs:            "S021",
	ExpectedProcedure:              "S022",
	InvalidNumberOfReturns:         "S023",
	MismatchedReturnType:           "S024",
	ExpectedMem:                    "S025",
	InvalidUseOfString:             "S026",
	MismatchedMultiRetAssignment:   "S027",
	MismatchedTypeInMultiRetAssign: "S028",
	CopyTooBig:                     "S029",
	InvalidCopy:                    "S030",
	MismatchedTypeInAssign:         "S031",
	BadConst:                       "S032",
	InvalidClassforExpr:            "S033",
	CannotUseSyscallInExpr:         "S034",
	CannotUseVoid:                  "S035",
}
