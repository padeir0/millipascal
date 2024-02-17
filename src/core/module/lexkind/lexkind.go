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
	MINUS
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
	BITWISEAND
	BITWISEOR
	BITWISEXOR
	BITWISENOT
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

	// special
	BLOCK
	SYMBOLS
	COUPLINGS
	PROCDECLS
	TYPELIST
	IDLIST
	TERMLIST
	ARRAYACCESS
	CALL
	EXPRLIST
	ELSEIFCHAIN
	SINGLE

	EOF
)

var tktostr = map[LexKind]string{
	UNDEFINED: "\033[0;31m?\033[0m",

	IDENTIFIER: "IDENTIFIER",
	I64_LIT:    "I64_LIT",
	I32_LIT:    "I32_LIT",
	I16_LIT:    "I16_LIT",
	I8_LIT:     "I8_LIT",
	U64_LIT:    "U64_LIT",
	U32_LIT:    "U32_LIT",
	U16_LIT:    "U16_LIT",
	U8_LIT:     "U8_LIT",
	PTR_LIT:    "PTR_LIT",
	STRING_LIT: "STRING_LIT",
	CHAR_LIT:   "CHAR_LIT",

	// symbols:
	PLUS:                  "PLUS",
	MINUS:                 "MINUS",
	NEG:                   "NEG",
	AT:                    "AT",
	DIVISION:              "DIVISION",
	MULTIPLICATION:        "MULTIPLICATION",
	REMAINDER:             "REMAINDER",
	PLUS_ASSIGN:           "PLUS_ASSIGN",
	MINUS_ASSIGN:          "MINUS_ASSIGN",
	DIVISION_ASSIGN:       "DIVISION_ASSIGN",
	MULTIPLICATION_ASSIGN: "MULTIPLICATION_ASSIGN",
	REMAINDER_ASSIGN:      "REMAINDER_ASSIGN",
	LESS:                  "LESS",
	LESSEQ:                "LESSEQ",
	EQUALS:                "EQUALS",
	MOREEQ:                "MOREEQ",
	MORE:                  "MORE",
	DIFFERENT:             "DIFFERENT",
	LEFTPAREN:             "LEFTPAREN",
	RIGHTPAREN:            "RIGHTPAREN",
	LEFTBRACE:             "LEFTBRACES",
	RIGHTBRACE:            "RIGHTBRACES",
	LEFTBRACKET:           "LEFTBRACKETS",
	RIGHTBRACKET:          "RIGHTBRACKETS",
	COLON:                 "COLON",
	DOUBLECOLON:           "DOUBLECOLON",
	ASSIGNMENT:            "ASSIGNMENT",
	COMMA:                 "COMMA",
	DOT:                   "DOT",
	BITWISEAND:            "BITWISEAND",
	BITWISEOR:             "BITWISEOR",
	BITWISEXOR:            "BITWISEXOR",
	BITWISENOT:            "BITWISENOT",
	SHIFTLEFT:             "SHIFTLEFT",
	SHIFTRIGHT:            "SHIFTRIGHT",

	// keyword: "keyword"
	VAR:    "VARS",
	TRUE:   "TRUE",
	FALSE:  "FALSE",
	AND:    "AND",
	OR:     "OR",
	NOT:    "NOT",
	IF:     "IF",
	ELSE:   "ELSE",
	WHILE:  "WHILE",
	RETURN: "RETURN",
	ELSEIF: "ELSEIF",
	PROC:   "PROC",
	DATA:   "DATA",
	BEGIN:  "BEGIN",
	END:    "END",
	SET:    "SET",
	EXIT:   "EXIT",
	IMPORT: "IMPORT",
	FROM:   "FROM",
	EXPORT: "EXPORT",
	SIZEOF: "SIZEOF",
	CONST:  "CONST",

	I8:   "I8",
	I16:  "I16",
	I32:  "I32",
	I64:  "I64",
	U8:   "I8",
	U16:  "I16",
	U32:  "I32",
	U64:  "I64",
	BOOL: "BOOL",
	PTR:  "PTR",

	// special
	BLOCK:       "BLOCK",
	SYMBOLS:     "SYMBOLS",
	PROCDECLS:   "PARAMS",
	TYPELIST:    "TYPELIST",
	IDLIST:      "IDLIST",
	ARRAYACCESS: "ARRAYACCESS",
	COUPLINGS:   "COUPLINGS",
	CALL:        "CALL",
	EXPRLIST:    "EXPRLIST",
	ELSEIFCHAIN: "ELSEIFCHAIN",
	SINGLE:      "SINGLE",

	EOF: "EOF",
}

func FmtLexKind(t LexKind) string {
	v, ok := tktostr[t]
	if ok {
		return v
	}
	panic("unspecified lexKind" + strconv.Itoa(int(t)))
}

func FmtTypes(t ...LexKind) string {
	out := tktostr[t[0]]
	for _, t := range t[1:] {
		out += "," + tktostr[t]
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
	MINUS:                 "-",
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
	LESS:                  "<",
	LESSEQ:                "<=",
	EQUALS:                "==",
	MOREEQ:                ">=",
	MORE:                  ">",
	DIFFERENT:             "!=",
	ASSIGNMENT:            "=",
	DOT:                   ".",
	BITWISEAND:            "&&",
	BITWISEOR:             "||",
	BITWISEXOR:            "|^",
	BITWISENOT:            "!",
	SHIFTLEFT:             "<<",
	SHIFTRIGHT:            ">>",

	VAR:    "vars",
	TRUE:   "true",
	FALSE:  "false",
	AND:    "and",
	OR:     "or",
	NOT:    "not",
	IF:     "if",
	ELSE:   "else",
	WHILE:  "while",
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
	EXIT:   "exit",
	IMPORT: "import",
	FROM:   "from",
	EXPORT: "export",
	SIZEOF: "sizeof",
	CONST:  "const",

	BLOCK:       "block",
	SYMBOLS:     "symbols",
	PROCDECLS:   "parameters",
	IDLIST:      "id list",
	TYPELIST:    "type list",
	ARRAYACCESS: "array access",
	COUPLINGS:   "module coupling",
	CALL:        "procedure call",
	EXPRLIST:    "expression list",
	ELSEIFCHAIN: "else if chain",
	SINGLE:      "single",

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
