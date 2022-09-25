package T

import "strconv"

type TkType int

const (
	UNDEFINED TkType = iota

	IDENTIFIER
	INT

	// symbols
	PLUS
	MINUS
	DIVISION
	MULTIPLICATION
	REMAINDER
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
	SYSCALL
	SET

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
	IDLIST
	TERMLIST
	ARRAYACCESS
	CALL
	EXPRLIST
	ASSIGNEES
	ELSEIFCHAIN

	EOF
)

var tktostr = map[TkType]string{
	UNDEFINED: "\033[0;31m?\033[0m",

	IDENTIFIER: "IDENTIFIER",
	INT:        "INT",

	// symbols:
	PLUS:           "PLUS",
	MINUS:          "MINUS",
	DIVISION:       "DIVISION",
	MULTIPLICATION: "MULTIPLICATION",
	REMAINDER:      "REMAINDER",
	LESS:           "LESS",
	LESSEQ:         "LESSEQ",
	EQUALS:         "EQUALS",
	MOREEQ:         "MOREEQ",
	MORE:           "MORE",
	DIFFERENT:      "DIFFERENT",
	LEFTPAREN:      "LEFTPAREN",
	RIGHTPAREN:     "RIGHTPAREN",
	LEFTBRACE:      "LEFTBRACES",
	RIGHTBRACE:     "RIGHTBRACES",
	LEFTBRACKET:    "LEFTBRACKETS",
	RIGHTBRACKET:   "RIGHTBRACKETS",
	COLON:          "COLON",
	ASSIGNMENT:     "ASSIGNMENT",
	COMMA:          "COMMA",

	// keyword: "keyword"
	VAR:     "VARS",
	TRUE:    "TRUE",
	FALSE:   "FALSE",
	AND:     "AND",
	OR:      "OR",
	NOT:     "NOT",
	IF:      "IF",
	ELSE:    "ELSE",
	WHILE:   "WHILE",
	RETURN:  "RETURN",
	ELSEIF:  "ELSEIF",
	PROC:    "PROC",
	MEMORY:  "MEMORY",
	BEGIN:   "BEGIN",
	END:     "END",
	SYSCALL: "SYSCALL",
	SET:     "SET",

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
	IDLIST:      "IDLIST",
	ARRAYACCESS: "ARRAYACCESS",
	CALL:        "CALL",
	EXPRLIST:    "EXPRLIST",
	ASSIGNEES:   "ASSIGNEES",
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
	INT:        "integer literal",
	IDENTIFIER: "identifier",

	PLUS:           "+",
	MINUS:          "-",
	DIVISION:       "/",
	MULTIPLICATION: "*",
	REMAINDER:      "%",
	LEFTPAREN:      "(",
	RIGHTPAREN:     ")",
	LEFTBRACE:      "{",
	RIGHTBRACE:     "}",
	LEFTBRACKET:    "[",
	RIGHTBRACKET:   "]",
	COLON:          ":",
	COMMA:          ",",
	LESS:           "<",
	LESSEQ:         "<=",
	EQUALS:         "==",
	MOREEQ:         ">=",
	MORE:           ">",
	DIFFERENT:      "!=",
	ASSIGNMENT:     "=",

	VAR:     "vars",
	TRUE:    "true",
	FALSE:   "false",
	AND:     "and",
	OR:      "or",
	NOT:     "not",
	IF:      "if",
	ELSE:    "else",
	WHILE:   "while",
	RETURN:  "return",
	ELSEIF:  "elseif",
	PROC:    "proc",
	MEMORY:  "memory",
	BEGIN:   "begin",
	END:     "end",
	SET:     "set",
	SYSCALL: "syscall",
	I8:      "i8",
	I16:     "i16",
	I32:     "i32",
	I64:     "i64",
	PTR:     "ptr",
	BOOL:    "bool",

	BLOCK:       "block",
	SYMBOLS:     "symbols",
	PROCDECLS:   "parameters",
	IDLIST:      "id list",
	ARRAYACCESS: "array access",
	CALL:        "procedure call",
	EXPRLIST:    "expression list",
	ASSIGNEES:   "assignees",
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
