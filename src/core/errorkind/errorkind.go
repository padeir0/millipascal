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
	InvalidSymbolCycle

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
	NotCallable
	InvalidNumberOfReturns
	MismatchedReturnType
	ExpectedData
	MismatchedMultiRetAssignment
	MismatchedTypeInMultiRetAssign
	MismatchedTypeInAssign
	InvalidTypeForExpr
	CannotUseVoid
	ExpectedBasicOrProcType
	CanOnlyUseNormalAssignment
	ExpectedIntegers
	ExitMustBeI8
	PtrCantBeUsedAsDataSize
	InvalidProp
	NotAllCodePathsReturnAValue
	InvalidMain
	NoEntryPoint
	AmbiguousModuleName
	ModuleNotFound
	NameNotExported
	ExpectedBool
	NonConstExpr
	CannotUseStringInExpr
	InvalidTypeForConst
	ValueOutOfBounds
	DoesntMatchBlobAnnot
	BadType
	CantImportAll
	ExpectedStruct
	OffsetInMultipleFields
	InvalidUseForStruct
	FieldNotDefined
	InvalidSizeof
	InvalidNumOfAssignees
	InvalidDataDecl
	InvalidStructDecl
	UnsizeableType
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
	NotCallable:                    "E022",
	InvalidNumberOfReturns:         "E023",
	MismatchedReturnType:           "E024",
	ExpectedData:                   "E025",
	MismatchedMultiRetAssignment:   "E027",
	MismatchedTypeInMultiRetAssign: "E028",
	MismatchedTypeInAssign:         "E031",
	InvalidTypeForExpr:             "E033",
	CannotUseVoid:                  "E035",
	ExpectedBasicOrProcType:        "E036",
	CanOnlyUseNormalAssignment:     "E037",
	ExpectedIntegers:               "E038",
	ExitMustBeI8:                   "E039",
	PtrCantBeUsedAsDataSize:        "E040",
	InvalidProp:                    "E041",
	NotAllCodePathsReturnAValue:    "E042",
	InvalidMain:                    "E043",
	NoEntryPoint:                   "E044",
	AmbiguousModuleName:            "E045",
	ModuleNotFound:                 "E046",
	NameNotExported:                "E047",
	ExpectedBool:                   "E048",
	NonConstExpr:                   "E049",
	CannotUseStringInExpr:          "E050",
	InvalidSymbolCycle:             "E051",
	InvalidTypeForConst:            "E052",
	ValueOutOfBounds:               "E053",
	DoesntMatchBlobAnnot:           "E054",
	BadType:                        "E055",
	CantImportAll:                  "E056",
	ExpectedStruct:                 "E057",
	OffsetInMultipleFields:         "E058",
	InvalidUseForStruct:            "E059",
	FieldNotDefined:                "E060",
	InvalidSizeof:                  "E061",
	InvalidNumOfAssignees:          "E062",
	InvalidDataDecl:                "E063",
	InvalidStructDecl:              "E064",
	UnsizeableType:                 "E065",
}
