package T

import "strconv"

type TkType int

const (
	UNDEFINED TkType = iota

	IDENTIFIER
	I64_LIT
	I32_LIT
	I16_LIT
	I8_LIT
	PTR_LIT
	STRING_LIT
	CHAR_LIT

	// symbols
	PLUS
	MINUS
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
	ASSIGNMENT
	COMMA
	SEMICOLON
	DOT

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
	MEMORY
	BEGIN
	END
	SET
	EXIT

	I8
	I16
	I32
	I64
	BOOL
	PTR

	// special
	BLOCK
	SYMBOLS
	PROCDECLS
	TYPELIST
	IDLIST
	TERMLIST
	ARRAYACCESS
	CALL
	EXPRLIST
	ELSEIFCHAIN

	EOF
)

var tktostr = map[TkType]string{
	UNDEFINED: "\033[0;31m?\033[0m",

	IDENTIFIER: "IDENTIFIER",
	I64_LIT:    "INT_LIT",
	I32_LIT:    "I32_LIT",
	I16_LIT:    "I16_LIT",
	I8_LIT:     "I8_LIT",
	PTR_LIT:    "PTR_LIT",
	STRING_LIT: "STRING_LIT",
	CHAR_LIT:   "CHAR_LIT",

	// symbols:
	PLUS:                  "PLUS",
	MINUS:                 "MINUS",
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
	ASSIGNMENT:            "ASSIGNMENT",
	COMMA:                 "COMMA",
	DOT:                   "DOT",

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
	MEMORY: "MEMORY",
	BEGIN:  "BEGIN",
	END:    "END",
	SET:    "SET",

	I8:   "I8",
	I16:  "I16",
	I32:  "I32",
	I64:  "I64",
	BOOL: "BOOL",
	PTR:  "PTR",

	// special
	BLOCK:       "BLOCK",
	SYMBOLS:     "SYMBOLS",
	PROCDECLS:   "PARAMS",
	TYPELIST:    "TYPELIST",
	IDLIST:      "IDLIST",
	ARRAYACCESS: "ARRAYACCESS",
	CALL:        "CALL",
	EXPRLIST:    "EXPRLIST",
	ELSEIFCHAIN: "ELSEIFCHAIN",

	EOF: "EOF",
}

func FmtNodeType(t TkType) string {
	v, ok := tktostr[t]
	if ok {
		return v
	}
	panic("unspecified nodeType" + strconv.Itoa(int(t)))
}

func FmtTypes(t ...TkType) string {
	out := tktostr[t[0]]
	for _, t := range t[1:] {
		out += "," + tktostr[t]
	}
	return out
}

var tktosrc = map[TkType]string{
	UNDEFINED:  "\033[0;31m?\033[0m",
	I64_LIT:    "i64 literal",
	I32_LIT:    "i32 literal",
	I16_LIT:    "i16 literal",
	I8_LIT:     "i8 literal",
	PTR_LIT:    "pointer literal",
	STRING_LIT: "string literal",
	CHAR_LIT:   "char literal",
	IDENTIFIER: "identifier",

	PLUS:                  "+",
	MINUS:                 "-",
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
	COMMA:                 ",",
	LESS:                  "<",
	LESSEQ:                "<=",
	EQUALS:                "==",
	MOREEQ:                ">=",
	MORE:                  ">",
	DIFFERENT:             "!=",
	ASSIGNMENT:            "=",
	DOT:                   ".",

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
	MEMORY: "memory",
	BEGIN:  "begin",
	END:    "end",
	SET:    "set",
	I8:     "i8",
	I16:    "i16",
	I32:    "i32",
	I64:    "i64",
	PTR:    "ptr",
	BOOL:   "bool",

	BLOCK:       "block",
	SYMBOLS:     "symbols",
	PROCDECLS:   "parameters",
	IDLIST:      "id list",
	TYPELIST:    "type list",
	ARRAYACCESS: "array access",
	CALL:        "procedure call",
	EXPRLIST:    "expression list",
	ELSEIFCHAIN: "else if chain",

	EOF: "EOF",
}

func fmtToUser(t TkType) string {
	v, ok := tktosrc[t]
	if ok {
		return v
	}
	panic("unspecified nodeType")
}

func FmtToUser(t ...TkType) string {
	out := "'" + fmtToUser(t[0]) + "'"
	for _, t := range t[1:] {
		out += ", '" + fmtToUser(t) + "'"
	}
	return out
}
