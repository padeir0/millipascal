package errorkind

import (
	"fmt"
)

type ErrorKind int

const (
	InvalidErrType ErrorKind = iota
	InternalCompilerError

	InvalidSymbol

	ExpectedSymbol
	ExpectedProd
	ExpectedEOF

	FileError
	InvalidFileName
	InvalidDependencyCycle

	NameAlreadyDefined
	ExportingUndefName
	DuplicatedExport
	OperationBetweenUnequalTypes
	NameNotDefined
	CanOnlyDerefPointers
	CanOnlyAssignLocal
	NotAssignable
	InvalidType
	MismatchedTypeForArgument
	InvalidNumberOfArgs
	ExpectedProcedure
	InvalidNumberOfReturns
	MismatchedReturnType
	ExpectedMem
	MismatchedMultiRetAssignment
	MismatchedTypeInMultiRetAssign
	MismatchedTypeInAssign
	InvalidClassforExpr
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
	ExpectedBool
)

func (et ErrorKind) String() string {
	v, ok := ErrorCodeMap[et]
	if !ok {
		panic(fmt.Sprintf("%d is not stringified", et))
	}
	return v
}

var ErrorCodeMap = map[ErrorKind]string{
	InvalidErrType:        "E101",
	InternalCompilerError: "E102",

	InvalidSymbol: "E104",

	ExpectedSymbol: "E105",
	ExpectedProd:   "E106",
	ExpectedEOF:    "E107",

	NameAlreadyDefined:             "E002",
	OperationBetweenUnequalTypes:   "E003",
	DuplicatedExport:               "E004",
	ExportingUndefName:             "E005",
	InvalidDependencyCycle:         "E006",
	FileError:                      "E007",
	InvalidFileName:                "E009",
	NameNotDefined:                 "E013",
	CanOnlyDerefPointers:           "E014",
	CanOnlyAssignLocal:             "E015",
	NotAssignable:                  "E016",
	InvalidType:                    "E017",
	MismatchedTypeForArgument:      "E020",
	InvalidNumberOfArgs:            "E021",
	ExpectedProcedure:              "E022",
	InvalidNumberOfReturns:         "E023",
	MismatchedReturnType:           "E024",
	ExpectedMem:                    "E025",
	MismatchedMultiRetAssignment:   "E027",
	MismatchedTypeInMultiRetAssign: "E028",
	MismatchedTypeInAssign:         "E031",
	InvalidClassforExpr:            "E033",
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
	ExpectedBool:                   "E048",
}
