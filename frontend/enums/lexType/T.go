package T

import "strconv"

type TkType int

const (
	UNDEFINED TkType = iota

	IDENTIFIER
	INT
	STRING
	CHAR

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

	// keywords
	VARS
	TRUE
	FALSE
	OR
	AND
	NOT
	IF
	ELSE
	WHILE
	RETURN
	ELSEIF
	PROC
	MEM
	CONST
	RES
	SET
	DEF
	BEGIN
	END
	COPY
	TO

	BYTE
	WORD
	DWORD
	QWORD

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

	EOF
)

var tktostr = map[TkType]string{
	UNDEFINED: "\033[0;31m?\033[0m",

	IDENTIFIER: "IDENTIFIER",
	INT:        "INT",
	STRING:     "STRING",
	CHAR:       "CHAR",

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
	VARS:   "VARS",
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
	MEM:    "MEM",
	CONST:  "CONST",
	RES:    "RES",
	SET:    "SET",
	DEF:    "DET",
	BEGIN:  "BEGIN",
	END:    "END",
	COPY:   "COPY",
	TO:     "TO",
	BYTE:   "BYTE",
	WORD:   "WORD",
	DWORD:  "DWORD",
	QWORD:  "QWORD",

	// special
	BLOCK:       "BLOCK",
	SYMBOLS:     "SYMBOLS",
	PROCDECLS:      "PARAMS",
	IDLIST:      "IDLIST",
	ARRAYACCESS: "ARRAYACCESS",
	CALL:        "CALL",
	EXPRLIST:    "EXPRLIST",
	ASSIGNEES:   "ASSIGNEES",


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
	STRING:     "string literal",
	CHAR:       "rune literal",

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

	VARS:   "vars",
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
	MEM:    "mem",
	CONST:  "const",
	RES:    "res",
	SET:    "set",
	DEF:    "det",
	BEGIN:  "begin",
	END:    "end",
	COPY:   "copy",
	TO:     "to",
	BYTE:   "byte",
	WORD:   "word",
	DWORD:  "dword",
	QWORD:  "qword",

	BLOCK:       "block",
	SYMBOLS:     "symbols",
	PROCDECLS:      "parameters",
	IDLIST:      "id list",
	ARRAYACCESS: "array access",
	CALL:        "procedure call",
	EXPRLIST:    "expression list",
	ASSIGNEES:   "assignees",

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
