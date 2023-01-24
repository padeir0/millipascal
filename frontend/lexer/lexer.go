package lexer

import (
	ir "mpc/core/module"
	T "mpc/core/module/lexkind"

	. "mpc/core"
	et "mpc/core/errorkind"

	"fmt"
	"strings"
	"unicode/utf8"
)

const IsTracking bool = false

func NewLexerError(st *Lexer, t et.ErrorKind, message string) *Error {
	loc := GetSourceLocation(st)
	return &Error{
		Type: t,
		Info: []Excerpt{
			{
				Location: &loc,
				Message:  message,
			},
		},
	}
}

type Lexer struct {
	Word *ir.Node

	File      string
	Line, Col int

	Start, End   int
	LastRuneSize int
	Input        string

	Peeked *ir.Node
}

func NewLexer(s string) *Lexer {
	st := &Lexer{
		Input: s,
	}
	return st
}

func GetSourceLocation(s *Lexer) SourceLocation {
	return SourceLocation{
		File:  s.File,
		Line:  s.Line,
		Col:   s.Col,
		Input: &s.Input,
	}
}

func Next(l *Lexer) *Error {
	if l.Peeked != nil {
		p := l.Peeked
		l.Peeked = nil
		l.Word = p
		return nil
	}
	symbol, err := any(l)
	if err != nil {
		return err
	}
	l.Start = l.End // this shouldn't be here
	l.Word = symbol
	return nil
}

func Peek(s *Lexer) (*ir.Node, *Error) {
	symbol, err := any(s)
	if err != nil {
		return nil, err
	}
	s.Start = s.End
	s.Peeked = symbol
	return symbol, nil
}

func ReadAll(s *Lexer) ([]*ir.Node, *Error) {
	e := Next(s)
	if e != nil {
		return nil, e
	}
	output := []*ir.Node{}
	for s.Word.Lex != T.EOF {
		output = append(output, s.Word)
		e = Next(s)
		if e != nil {
			return nil, e
		}
	}
	return output, nil
}

func GenNumNode(l *Lexer, tp T.LexKind, value int64) *ir.Node {
	text := Selected(l)
	n := &ir.Node{
		Lex:    tp,
		Text:   text,
		Line:   l.Line,
		Value:  value,
		Col:    l.Col - len(text),
		Length: len(text),
	}
	return n
}

func GenNode(l *Lexer, tp T.LexKind) *ir.Node {
	text := Selected(l)
	n := &ir.Node{
		Lex:    tp,
		Text:   text,
		Line:   l.Line,
		Col:    l.Col - len(text),
		Length: len(text),
	}
	return n
}

func nextRune(l *Lexer) (rune, *Error) {
	l.Col++
	r, size := utf8.DecodeRuneInString(l.Input[l.End:])
	if r == utf8.RuneError && size == 1 {
		err := NewLexerError(l, et.InvalidUTF8Rune, "Invalid UTF8 rune in string")
		return -1, err
	}
	l.End += size
	l.LastRuneSize = size

	return r, nil
}

/*ignore ignores the text previously read*/
func ignore(l *Lexer) {
	l.Start = l.End
	l.LastRuneSize = 0
}

/*unread decrements the end index by the size of the last rune read,
can only be used once after a Next()*/
func unread(l *Lexer) {
	if l.End > 0 {
		l.End -= l.LastRuneSize
		l.LastRuneSize = 0
		l.Col--
	}
}

func acceptRun(l *Lexer, s string) *Error {
	r, err := nextRune(l)
	if err != nil {
		return err
	}
	for strings.ContainsRune(s, r) {
		r, err = nextRune(l)
		if err != nil {
			return err
		}
	}
	unread(l)
	return nil
}

func acceptUntil(l *Lexer, s string) *Error {
	r, err := nextRune(l)
	if err != nil {
		return err
	}
	for !strings.ContainsRune(s, r) {
		r, err = nextRune(l)
		if err != nil {
			return err
		}
	}
	unread(l)
	return nil
}

const (
	/*eof is equivalent to RuneError, but in this package it only shows up in EoFs
	If the rune is invalid, it panics instead*/
	eof rune = utf8.RuneError
)

func Selected(l *Lexer) string {
	return l.Input[l.Start:l.End]
}

const (
	insideStr  = `\"`
	insideChar = `\'`
	digits     = "0123456789"
	hex_digits = "0123456789ABCDEFabcdef"
	bin_digits = "01"
	letters    = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_" // yes _ is a letter, fuck you
)

func isNumber(r rune) bool {
	return strings.ContainsRune(digits, r)
}

func isLetter(r rune) bool {
	return strings.ContainsRune(letters, r)
}

func ignoreWhitespace(st *Lexer) *Error {
	r, err := nextRune(st)
loop:
	for {
		switch r {
		case ' ', '\t':
		case '\n':
			st.Line++
			st.Col = 0
		case '#':
			comment(st)
		default:
			break loop
		}
		ignore(st)
		r, err = nextRune(st)
		if err != nil {
			return err
		}
	}
	unread(st)
	return nil
}

// refactor this
func any(st *Lexer) (*ir.Node, *Error) {
	var err *Error
	var r rune
	var tp T.LexKind

	err = ignoreWhitespace(st)

	r, err = nextRune(st)
	if err != nil {
		return nil, err
	}

	if isNumber(r) {
		unread(st)
		return number(st)
	}
	if isLetter(r) {
		return identifier(st)
	}
	if r == '\'' {
		return charLit(st)
	}
	if r == '"' {
		return strLit(st)
	}

	switch r {
	case '+':
		r, err := nextRune(st)
		if err != nil {
			return nil, err
		}
		switch r {
		case '=':
			tp = T.PLUS_ASSIGN
		default:
			unread(st)
			tp = T.PLUS
		}
	case '-':
		r, err := nextRune(st)
		if err != nil {
			return nil, err
		}
		switch r {
		case '=':
			tp = T.MINUS_ASSIGN
		default:
			unread(st)
			tp = T.MINUS
		}
	case '/':
		r, err := nextRune(st)
		if err != nil {
			return nil, err
		}
		switch r {
		case '=':
			tp = T.DIVISION_ASSIGN
		default:
			unread(st)
			tp = T.DIVISION
		}
	case '*':
		r, err := nextRune(st)
		if err != nil {
			return nil, err
		}
		switch r {
		case '=':
			tp = T.MULTIPLICATION_ASSIGN
		default:
			unread(st)
			tp = T.MULTIPLICATION
		}
	case '%':
		r, err := nextRune(st)
		if err != nil {
			return nil, err
		}
		switch r {
		case '=':
			tp = T.REMAINDER_ASSIGN
		default:
			unread(st)
			tp = T.REMAINDER
		}
	case '@':
		tp = T.AT
	case '~':
		tp = T.NEG
	case '(':
		tp = T.LEFTPAREN
	case ')':
		tp = T.RIGHTPAREN
	case '{':
		tp = T.LEFTBRACE
	case '}':
		tp = T.RIGHTBRACE
	case '[':
		tp = T.LEFTBRACKET
	case ']':
		tp = T.RIGHTBRACKET
	case ',':
		tp = T.COMMA
	case ':':
		r, err := nextRune(st)
		if err != nil {
			return nil, err
		}
		switch r {
		case ':':
			tp = T.DOUBLECOLON
		default:
			unread(st)
			tp = T.COLON
		}
	case ';':
		tp = T.SEMICOLON
	case '.':
		tp = T.DOT
	case '>': // >  >=
		r, err := nextRune(st)
		if err != nil {
			return nil, err
		}
		switch r {
		case '=':
			tp = T.MOREEQ
		default:
			unread(st)
			tp = T.MORE
		}
	case '<': // <  <-  <=
		r, err := nextRune(st)
		if err != nil {
			return nil, err
		}
		switch r {
		case '=':
			tp = T.LESSEQ
		default:
			unread(st)
			tp = T.LESS
		}
	case '!':
		r, err := nextRune(st)
		if err != nil {
			return nil, err
		}
		switch r {
		case '=':
			tp = T.DIFFERENT
		default:
			unread(st)
			message := fmt.Sprintf("Invalid symbol: %v", string(r))
			err := NewLexerError(st, et.InvalidSymbol, message)
			return nil, err
		}
	case '=':
		r, err := nextRune(st)
		if err != nil {
			return nil, err
		}
		if r == '=' {
			tp = T.EQUALS
		} else {
			unread(st)
			tp = T.ASSIGNMENT
		}
	case eof:
		return &ir.Node{Lex: T.EOF}, nil
	default:
		message := fmt.Sprintf("Invalid symbol: %v", string(r))
		err := NewLexerError(st, et.InvalidSymbol, message)
		return nil, err
	}
	return GenNode(st, tp), nil
}

// sorry
func number(st *Lexer) (*ir.Node, *Error) {
	r, err := nextRune(st)
	if err != nil {
		return nil, err
	}
	var value int64
	if r == '0' {
		r, err = nextRune(st)
		switch r {
		case 'x': // he x
			err = acceptRun(st, hex_digits)
			if err != nil {
				return nil, err
			}
			value = parseHex(Selected(st))
		case 'b': // b inary
			err = acceptRun(st, bin_digits)
			if err != nil {
				return nil, err
			}
			value = parseBin(Selected(st))
		default:
			unread(st)
			err = acceptRun(st, digits)
			if err != nil {
				return nil, err
			}
			value = parseNormal(Selected(st))
		}
	} else {
		unread(st)
		err = acceptRun(st, digits)
		if err != nil {
			return nil, err
		}
		value = parseNormal(Selected(st))
	}
	r, err = nextRune(st)
	if err != nil {
		return nil, err
	}
	switch r {
	case 'p': // p ointer
		return GenNumNode(st, T.PTR_LIT, value), nil
	case 'r': // cha r
		return GenNumNode(st, T.I8_LIT, value), nil
	case 't': // shor t
		return GenNumNode(st, T.I16_LIT, value), nil
	case 'g': // lon g
		return GenNumNode(st, T.I32_LIT, value), nil
	}
	unread(st)
	return GenNumNode(st, T.I64_LIT, value), nil
}

func identifier(st *Lexer) (*ir.Node, *Error) {
	err := acceptRun(st, digits+letters)
	if err != nil {
		return nil, err
	}
	selected := Selected(st)
	tp := T.IDENTIFIER
	switch selected {
	case "var":
		tp = T.VAR
	case "true":
		tp = T.TRUE
	case "false":
		tp = T.FALSE
	case "and":
		tp = T.AND
	case "or":
		tp = T.OR
	case "not":
		tp = T.NOT
	case "if":
		tp = T.IF
	case "else":
		tp = T.ELSE
	case "while":
		tp = T.WHILE
	case "return":
		tp = T.RETURN
	case "elseif":
		tp = T.ELSEIF
	case "proc":
		tp = T.PROC
	case "memory":
		tp = T.MEMORY
	case "begin":
		tp = T.BEGIN
	case "end":
		tp = T.END
	case "set":
		tp = T.SET
	case "exit":
		tp = T.EXIT
	case "import":
		tp = T.IMPORT
	case "from":
		tp = T.FROM
	case "export":
		tp = T.EXPORT
	case "i8":
		tp = T.I8
	case "i16":
		tp = T.I16
	case "i32":
		tp = T.I32
	case "i64":
		tp = T.I64
	case "bool":
		tp = T.BOOL
	case "ptr":
		tp = T.PTR
	}
	return GenNode(st, tp), nil
}

func comment(st *Lexer) *Error {
	r, err := nextRune(st)
	if err != nil {
		return err
	}
	for !strings.ContainsRune("\n"+string(eof), r) {
		r, err = nextRune(st)
		if err != nil {
			return err
		}
	}
	if r == '\n' {
		unread(st)
	}
	return nil
}

func strLit(st *Lexer) (*ir.Node, *Error) {
	for {
		err := acceptUntil(st, insideStr)
		if err != nil {
			return nil, err
		}
		r, err := nextRune(st)
		if err != nil {
			return nil, err
		}
		if r == '"' {
			return &ir.Node{
				Text: Selected(st),
				Lex:  T.STRING_LIT,
				Line: st.Line,
				Col:  st.Col,
			}, nil
		}
		if r == '\\' {
			_, err = nextRune(st) // escaped rune
			if err != nil {
				return nil, err
			}
		}
		unread(st)
	}
}

func charLit(st *Lexer) (*ir.Node, *Error) {
	for {
		acceptUntil(st, insideChar)
		r, err := nextRune(st)
		if err != nil {
			return nil, err
		}
		if r == '\'' {
			text := Selected(st)
			return &ir.Node{
				Text:  text,
				Lex:   T.CHAR_LIT,
				Line:  st.Line,
				Value: parseCharLit(text[1 : len(text)-1]),
				Col:   st.Col,
			}, nil
		}
		if r == '\\' {
			_, err = nextRune(st) // escaped
			if err != nil {
				return nil, err
			}
		}
		unread(st)
	}
}

func IsValidIdentifier(s string) bool {
	st := NewLexer(s)
	tks, err := ReadAll(st)
	if err != nil {
		return false
	}
	if len(tks) != 1 { // we want only ID
		return false
	}
	return tks[0].Lex == T.IDENTIFIER
}

func parseNormal(text string) int64 {
	var output int64 = 0
	for i := range text {
		output *= 10
		char := text[i]
		if char >= '0' || char <= '9' {
			output += int64(char - '0')
		} else {
			panic(text)
		}
	}
	return output
}

func parseHex(oldText string) int64 {
	text := oldText[2:]
	var output int64 = 0
	for i := range text {
		output *= 16
		char := text[i]
		if char >= '0' && char <= '9' {
			output += int64(char - '0')
		} else if char >= 'a' && char <= 'f' {
			output += int64(char-'a') + 10
		} else if char >= 'A' && char <= 'F' {
			output += int64(char-'A') + 10
		} else {
			panic(text)
		}
	}
	return output
}

func parseBin(oldText string) int64 {
	text := oldText[2:]
	var output int64 = 0
	for i := range text {
		output *= 2
		char := text[i]
		if char == '0' || char == '1' {
			output += int64(char - '0')
		} else {
			panic(text)
		}
	}
	return output
}

func parseCharLit(text string) int64 {
	value := int64(text[0])
	if len(text) > 1 {
		switch text {
		case "\\n":
			value = '\n'
		case "\\t":
			value = '\t'
		case "\\r":
			value = '\r'
		case "\\'":
			value = '\''
		case "\\\"":
			value = '"'
		case "\\\\":
			value = '\\'
		default:
			fmt.Println(text)
			panic("too many chars in char :C")
		}
	}
	return value
}
