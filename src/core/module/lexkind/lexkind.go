package lexkind

import "strconv"

type LexKind int

const (
	UNDEFINED LexKind = iota

	IDENTIFIER
	I64_LIT
	I32_LIT
	I16_LIT
	I8_LIT
	U64_LIT
	U32_LIT
	U16_LIT
	U8_LIT
	PTR_LIT
	STRING_LIT
	CHAR_LIT

	// symbols
	PLUS
	PLUS_PLUS
	MINUS
	MINUS_MINUS
	NEG
	DIVISION
	MULTIPLICATION
	REMAINDER
	PLUS_ASSIGN
	MINUS_ASSIGN
	DIVISION_ASSIGN
	MULTIPLICATION_ASSIGN
	REMAINDER_ASSIGN
	AT
	LESS
	LESSEQ
	EQUALS
	MOREEQ
	MORE
	DIFFERENT
	QUESTION
	SWAP

	LEFTPAREN
	RIGHTPAREN
	LEFTBRACE
	RIGHTBRACE
	LEFTBRACKET
	RIGHTBRACKET

	COLON
	DOUBLECOLON
	ASSIGNMENT
	COMMA
	SEMICOLON

	DOT
	ARROW

	BITWISEAND
	BITWISEOR
	BITWISENOT
	BITWISEXOR
	SHIFTLEFT
	SHIFTRIGHT

	// keywords
	VAR
	TRUE
	FALSE
	OR
	AND
	NOT
	IF
	ELSE
	ELSEIF
	WHILE
	DO
	RETURN
	PROC
	DATA
	BEGIN
	END
	SET
	EXIT
	IMPORT
	FROM
	EXPORT
	SIZEOF
	CONST
	ATTR
	AS
	ALL
	STRUCT
	ASM

	I8
	I16
	I32
	I64
	U8
	U16
	U32
	U64
	BOOL
	PTR
	VOID

	// special
	TYPELIST
	IDLIST
	ALIASLIST
	FIELDLIST
	EXPRLIST
	OPLIST

	BLOCK
	SYMBOLS
	COUPLINGS
	PROCDECLS
	TERMLIST
	ARRAYACCESS
	CALL
	ELSEIFCHAIN
	SINGLE
	BLOB
	FIELD
	ASMLINES
	INSTR

	EOF
)

func FmtLexKind(t LexKind) string {
	v, ok := Tktosrc[t]
	if ok {
		return v
	}
	panic("unspecified lexKind" + strconv.Itoa(int(t)))
}

func FmtTypes(t ...LexKind) string {
	out := Tktosrc[t[0]]
	for _, t := range t[1:] {
		out += "," + Tktosrc[t]
	}
	return out
}

var Tktosrc = map[LexKind]string{
	UNDEFINED:  "\033[0;31m?\033[0m",
	I64_LIT:    "i64 literal",
	I32_LIT:    "i32 literal",
	I16_LIT:    "i16 literal",
	I8_LIT:     "i8 literal",
	U64_LIT:    "u64 literal",
	U32_LIT:    "u32 literal",
	U16_LIT:    "u16 literal",
	U8_LIT:     "u8 literal",
	PTR_LIT:    "pointer literal",
	STRING_LIT: "string literal",
	CHAR_LIT:   "char literal",
	IDENTIFIER: "identifier",

	PLUS:                  "+",
	PLUS_PLUS:             "++",
	MINUS:                 "-",
	MINUS_MINUS:           "--",
	NEG:                   "~",
	AT:                    "@",
	DIVISION:              "/",
	MULTIPLICATION:        "*",
	REMAINDER:             "%",
	PLUS_ASSIGN:           "+=",
	MINUS_ASSIGN:          "-=",
	DIVISION_ASSIGN:       "/=",
	MULTIPLICATION_ASSIGN: "*=",
	REMAINDER_ASSIGN:      "%=",
	LEFTPAREN:             "(",
	RIGHTPAREN:            ")",
	LEFTBRACE:             "{",
	RIGHTBRACE:            "}",
	LEFTBRACKET:           "[",
	RIGHTBRACKET:          "]",
	COLON:                 ":",
	DOUBLECOLON:           "::",
	COMMA:                 ",",
	SEMICOLON:             ";",
	LESS:                  "<",
	LESSEQ:                "<=",
	EQUALS:                "==",
	MOREEQ:                ">=",
	MORE:                  ">",
	DIFFERENT:             "!=",
	ASSIGNMENT:            "=",
	DOT:                   ".",
	ARROW:                 "->",
	BITWISEAND:            "&",
	BITWISEOR:             "|",
	BITWISENOT:            "!",
	BITWISEXOR:            "^",
	SHIFTLEFT:             "<<",
	SHIFTRIGHT:            ">>",
	QUESTION:              "?",
	SWAP:                  "<>",

	VAR:    "vars",
	TRUE:   "true",
	FALSE:  "false",
	AND:    "and",
	OR:     "or",
	NOT:    "not",
	IF:     "if",
	ELSE:   "else",
	WHILE:  "while",
	DO:     "do",
	RETURN: "return",
	ELSEIF: "elseif",
	PROC:   "proc",
	DATA:   "data",
	BEGIN:  "begin",
	END:    "end",
	SET:    "set",
	I8:     "i8",
	I16:    "i16",
	I32:    "i32",
	I64:    "i64",
	U8:     "u8",
	U16:    "u16",
	U32:    "u32",
	U64:    "u64",
	PTR:    "ptr",
	BOOL:   "bool",
	VOID:   "void",
	EXIT:   "exit",
	IMPORT: "import",
	FROM:   "from",
	EXPORT: "export",
	SIZEOF: "sizeof",
	CONST:  "const",
	ATTR:   "attr",
	AS:     "as",
	ALL:    "all",
	ASM:    "asm",
	STRUCT: "struct",

	IDLIST:    "id list",
	ALIASLIST: "alias list",
	FIELDLIST: "field list",
	TYPELIST:  "type list",
	EXPRLIST:  "expression list",
	OPLIST:    "operand list",

	BLOCK:       "block",
	SYMBOLS:     "symbols",
	PROCDECLS:   "parameters",
	ARRAYACCESS: "array access",
	COUPLINGS:   "module coupling",
	CALL:        "procedure call",
	ELSEIFCHAIN: "else if chain",
	SINGLE:      "single",
	BLOB:        "blob",
	FIELD:       "field",
	ASMLINES:    "asm lines",
	INSTR:       "instruction",

	EOF: "EOF",
}

func fmtToUser(t LexKind) string {
	v, ok := Tktosrc[t]
	if ok {
		return v
	}
	panic("unspecified nodeType")
}

func FmtToUser(t ...LexKind) string {
	out := "'" + fmtToUser(t[0]) + "'"
	for _, t := range t[1:] {
		out += ", '" + fmtToUser(t) + "'"
	}
	return out
}
