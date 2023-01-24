package errorkind

import (
	"fmt"
)

type ErrorKind int

const (
	/* internal (bad) errors */
	InvalidErrType ErrorKind = iota
	InternalCompilerError

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
	ExpectedBasicType
	CanOnlyUseNormalAssignment
	ExpectedNumber
	ExitMustBeI8
	PtrCantBeUsedAsMemSize
	InvalidProp
	NotAllCodePathsReturnAValue
	InvalidMain
	NoEntryPoint
	AmbiguousModuleName
	ModuleNotFound
	NameNotExported
)

func (et ErrorKind) String() string {
	v, ok := ErrorCodeMap[et]
	if !ok {
		panic(fmt.Sprintf("%d is not stringified", et))
	}
	return v
}

func (et ErrorKind) Debug() string {
	if et == InvalidErrType {
		panic("invalid ErrType")
	}
	v, ok := DebugMap[et]
	if !ok {
		panic("ErrType not mapped to string")
	}
	return v
}

var DebugMap = map[ErrorKind]string{
	InvalidErrType:        "InvalidErrType",
	InternalCompilerError: "InternalCompilerError",

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
	CanOnlyDerefPointers:           "CanOnlyIndexMemory",
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
	ExpectedBasicType:              "ExpectedBasicType",
	CanOnlyUseNormalAssignment:     "CanOnlyUseNormalAssignment",
	ExpectedNumber:                 "ExpectedNumber",
	ExitMustBeI8:                   "ExitMustBeI8",
	PtrCantBeUsedAsMemSize:         "PtrCantBeUsedAsMemSize",
	InvalidProp:                    "InvalidProp",
	NotAllCodePathsReturnAValue:    "NotAllCodePathsReturnAValue",
	InvalidMain:                    "InvalidMain",
	NoEntryPoint:                   "NoEntryPoint",
	AmbiguousModuleName:            "AmbiguousModuleName",
	ModuleNotFound:                 "ModuleNotFound",
	NameNotExported:                "NameNotExported",
}

var ErrorCodeMap = map[ErrorKind]string{
	/* internal (bad) errors */
	InvalidErrType:        "E101",
	InternalCompilerError: "E102",

	/* resolver errors */
	/* lexer errors */
	InvalidUTF8Rune: "E103",
	InvalidSymbol:   "E104",

	/* parser errors */
	ExpectedSymbol: "E105",
	ExpectedProd:   "E106",
	ExpectedEOF:    "E107",

	/* semantic errors */
	ExpectedName:                   "E001",
	NameAlreadyDefined:             "E002",
	OperationBetweenUnequalTypes:   "E003",
	DuplicatedExport:               "E004",
	ExportingUndefName:             "E005",
	InvalidDependencyCycle:         "E006",
	FileError:                      "E007",
	ImportError:                    "E008",
	InvalidFileName:                "E009",
	ImportNameColision:             "E010",
	FromImportingUnexportedName:    "E011",
	InvalidInitForMemType:          "E012",
	NameNotDefined:                 "E013",
	CanOnlyDerefPointers:           "E014",
	CanOnlyAssignLocal:             "E015",
	NotAssignable:                  "E016",
	InvalidType:                    "E017",
	InvalidMemResTerm:              "E018",
	ExpectedConst:                  "E019",
	MismatchedTypeForArgument:      "E020",
	InvalidNumberOfArgs:            "E021",
	ExpectedProcedure:              "E022",
	InvalidNumberOfReturns:         "E023",
	MismatchedReturnType:           "E024",
	ExpectedMem:                    "E025",
	InvalidUseOfString:             "E026",
	MismatchedMultiRetAssignment:   "E027",
	MismatchedTypeInMultiRetAssign: "E028",
	CopyTooBig:                     "E029",
	InvalidCopy:                    "E030",
	MismatchedTypeInAssign:         "E031",
	BadConst:                       "E032",
	InvalidClassforExpr:            "E033",
	CannotUseSyscallInExpr:         "E034",
	CannotUseVoid:                  "E035",
	ExpectedBasicType:              "E036",
	CanOnlyUseNormalAssignment:     "E037",
	ExpectedNumber:                 "E038",
	ExitMustBeI8:                   "E039",
	PtrCantBeUsedAsMemSize:         "E040",
	InvalidProp:                    "E041",
	NotAllCodePathsReturnAValue:    "E042",
	InvalidMain:                    "E043",
	NoEntryPoint:                   "E044",
	AmbiguousModuleName:            "E045",
	ModuleNotFound:                 "E046",
	NameNotExported:                "E047",
}
