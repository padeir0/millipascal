package lexer

import (
	"mpc/frontend/ast"
	T "mpc/frontend/enums/lexType"

	"mpc/frontend/errors"
	et "mpc/frontend/enums/errType"

	"fmt"
	"strings"
	"unicode/utf8"
)

const IsTracking bool = false

func NewLexerError(st *Lexer, t et.ErrType, message string) *errors.CompilerError {
	loc := GetSourceLocation(st)
	return &errors.CompilerError{
		Stage: errors.Lexer,
		Type:  t,
		Info: []errors.Excerpt{
			{
				Location: &loc,
				Message:  message,
			},
		},
	}
}

type Lexer struct {
	Word *ast.Node

	File      string
	Line, Col int

	Start, End   int
	LastRuneSize int
	Input        string

	Peeked *ast.Node
}

func NewLexer(s string) *Lexer {
	st := &Lexer{
		Input: s,
	}
	return st
}

func GetSourceLocation(s *Lexer) errors.SourceLocation {
	return errors.SourceLocation{
		File:  s.File,
		Line:  s.Line,
		Col:   s.Col,
		Input: &s.Input,
	}
}

func Next(l *Lexer) *errors.CompilerError {
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
	l.Start = l.End
	l.Word = symbol
	return nil
}

func Peek(s *Lexer) (*ast.Node, *errors.CompilerError) {
	symbol, err := any(s)
	if err != nil {
		return nil, err
	}
	s.Start = s.End
	s.Peeked = symbol
	return symbol, nil
}

func ReadAll(s *Lexer) ([]*ast.Node, *errors.CompilerError) {
	e := Next(s)
	if e != nil {
		return nil, e
	}
	output := []*ast.Node{}
	for s.Word.Lex != T.EOF {
		output = append(output, s.Word)
		e = Next(s)
		if e != nil {
			return nil, e
		}
	}
	return output, nil
}

func GenNode(l *Lexer, tp T.TkType) *ast.Node {
	text := Selected(l)
	n := &ast.Node{
		Lex:    tp,
		Text:   text,
		Line:   l.Line,
		Col:    l.Col - len(text),
		Length: len(text),
	}
	return n
}

func nextRune(l *Lexer) (rune, *errors.CompilerError) {
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

func acceptRun(l *Lexer, s string) *errors.CompilerError {
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

func acceptUntil(l *Lexer, s string) *errors.CompilerError {
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
	insideRune = `\'`
	digits     = "0123456789"
	letters    = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_" // yes _ is a letter, fuck you
)

func isNumber(r rune) bool {
	return strings.ContainsRune(digits, r)
}

func isLetter(r rune) bool {
	return strings.ContainsRune(letters, r)
}

func ignoreWhitespace(st *Lexer) *errors.CompilerError {
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
func any(st *Lexer) (*ast.Node, *errors.CompilerError) {
	var err *errors.CompilerError
	var r rune
	var tp T.TkType

	err = ignoreWhitespace(st)

	r, err = nextRune(st)
	if err != nil {
		return nil, err
	}

	if err != nil {
		return nil, err
	}

	if isNumber(r) {
		return number(st)
	}
	if isLetter(r) {
		return identifier(st)
	}

	switch r {
	case '+':
		tp = T.PLUS
	case '-':
		tp = T.MINUS
	case '/':
		tp = T.DIVISION
	case '*':
		tp = T.MULTIPLICATION
	case '%':
		tp = T.REMAINDER
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
	case '"':
		return strLit(st)
	case '\'':
		return runeLit(st)
	case ':':
		tp = T.COLON
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
		return &ast.Node{Lex: T.EOF}, nil
	default:
		message := fmt.Sprintf("Invalid symbol: %v", string(r))
		err := NewLexerError(st, et.InvalidSymbol, message)
		return nil, err
	}
	return GenNode(st, tp), nil
}

func number(st *Lexer) (*ast.Node, *errors.CompilerError) {
	err := acceptRun(st, digits)
	if err != nil {
		return nil, err
	}
	return GenNode(st, T.INT), nil
}

func strLit(st *Lexer) (*ast.Node, *errors.CompilerError) {
	for {
		acceptUntil(st, insideStr)
		r, err := nextRune(st)
		if err != nil {
			return nil, err
		}
		if r == '"' {
			return &ast.Node{
				Text: Selected(st),
				Lex:  T.STRING,
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

func runeLit(st *Lexer) (*ast.Node, *errors.CompilerError) {
	for {
		acceptUntil(st, insideRune)
		r, err := nextRune(st)
		if err != nil {
			return nil, err
		}
		if r == '"' {
			return &ast.Node{
				Text: Selected(st),
				Lex:  T.CHAR,
				Line: st.Line,
				Col:  st.Col,
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

func identifier(st *Lexer) (*ast.Node, *errors.CompilerError) {
	err := acceptRun(st, digits+letters)
	if err != nil {
		return nil, err
	}
	selected := Selected(st)
	tp := T.IDENTIFIER
	switch selected {
	case "vars":
		tp = T.VARS
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
	case "mem":
		tp = T.MEM
	case "const":
		tp = T.CONST
	case "res":
		tp = T.RES
	case "set":
		tp = T.SET
	case "def":
		tp = T.DEF
	case "begin":
		tp = T.BEGIN
	case "end":
		tp = T.END
	case "byte":
		tp = T.BYTE
	case "word":
		tp = T.WORD
	case "dword":
		tp = T.DWORD
	case "qword":
		tp = T.QWORD
	}
	return GenNode(st, tp), nil
}

func comment(st *Lexer) *errors.CompilerError {
	r, err := nextRune(st)
	if err != nil {
		return err
	}
	for !strings.ContainsRune("#\n"+string(eof), r) {
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
