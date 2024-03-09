package lexer

import (
	ir "mpc/core/module"
	T "mpc/core/module/lexkind"

	. "mpc/core"
	et "mpc/core/errorkind"
	sv "mpc/core/severity"

	"fmt"
	"math/big"
	"strings"
	"unicode/utf8"
)

const IsTracking bool = false

func Track(st *Lexer, s string) {
	if IsTracking {
		fmt.Printf("%v: %v\n", s, st.Word.String())
	}
}

func NewLexerError(st *Lexer, t et.ErrorKind, message string) *Error {
	loc := st.GetSourceLocation()
	return &Error{
		Code:     t,
		Severity: sv.Error,
		Location: loc,
		Message:  message,
	}
}

func AllTokens(s *Lexer) []*ir.Node {
	output := []*ir.Node{}
	for s.Word.Lex != T.EOF {
		output = append(output, s.Word)
		s.Next()
	}
	return output
}

type Lexer struct {
	Word *ir.Node

	File                string
	BeginLine, BeginCol int
	EndLine, EndCol     int

	Start, End   int
	LastRuneSize int
	Input        string

	Peeked *ir.Node
}

func NewLexer(filename string, s string) *Lexer {
	st := &Lexer{
		File:  filename,
		Input: s,
	}
	return st
}

func (this *Lexer) GetSourceLocation() *Location {
	return &Location{
		File:  this.File,
		Range: this.Range(),
	}
}

func (this *Lexer) Next() *Error {
	if this.Peeked != nil {
		p := this.Peeked
		this.Peeked = nil
		this.Word = p
		return nil
	}
	symbol, err := any(this)
	if err != nil {
		return err
	}
	this.Start = this.End // this shouldn't be here
	this.BeginLine = this.EndLine
	this.BeginCol = this.EndCol
	this.Word = symbol
	return nil
}

func (this *Lexer) Peek() (*ir.Node, *Error) {
	symbol, err := any(this)
	if err != nil {
		return nil, err
	}
	this.Start = this.End
	this.Peeked = symbol
	return symbol, nil
}

func (this *Lexer) ReadAll() ([]*ir.Node, *Error) {
	e := this.Next()
	if e != nil {
		return nil, e
	}
	output := []*ir.Node{}
	for this.Word.Lex != T.EOF {
		output = append(output, this.Word)
		e = this.Next()
		if e != nil {
			return nil, e
		}
	}
	return output, nil
}

func (this *Lexer) Selected() string {
	return this.Input[this.Start:this.End]
}

func (this *Lexer) Range() *Range {
	return &Range{
		Begin: Position{
			Line:   this.BeginLine,
			Column: this.BeginCol,
		},
		End: Position{
			Line:   this.EndLine,
			Column: this.EndCol,
		},
	}
}

func genNumNode(l *Lexer, tp T.LexKind, value *big.Int) *ir.Node {
	text := l.Selected()
	n := &ir.Node{
		Lex:   tp,
		Text:  text,
		Value: value,
		Range: l.Range(),
	}
	return n
}

func genNode(l *Lexer, tp T.LexKind) *ir.Node {
	text := l.Selected()
	n := &ir.Node{
		Lex:   tp,
		Text:  text,
		Range: l.Range(),
	}
	return n
}

func nextRune(l *Lexer) rune {
	r, size := utf8.DecodeRuneInString(l.Input[l.End:])
	if r == utf8.RuneError && size == 1 {
		panic("Invalid UTF8 rune in string")
	}
	l.End += size
	l.LastRuneSize = size

	if r == '\n' {
		l.EndLine++
		l.EndCol = 0
	} else {
		l.EndCol++
	}

	return r
}

func peekRune(l *Lexer) rune {
	r, size := utf8.DecodeRuneInString(l.Input[l.End:])
	if r == utf8.RuneError && size == 1 {
		panic("Invalid UTF8 rune in string")
	}

	return r
}

/*ignore ignores the text previously read*/
func ignore(l *Lexer) {
	l.Start = l.End
	l.BeginLine = l.EndLine
	l.BeginCol = l.EndCol
	l.LastRuneSize = 0
}

func acceptRun(l *Lexer, s string) {
	r := peekRune(l)
	for strings.ContainsRune(s, r) {
		nextRune(l)
		r = peekRune(l)
	}
}

func acceptUntil(l *Lexer, s string) {
	r := peekRune(l)
	for !strings.ContainsRune(s, r) {
		nextRune(l)
		r = peekRune(l)
	}
}

const (
	/*eof is equivalent to RuneError, but in this package it only shows up in EoFs
	If the rune is invalid, it panics instead*/
	eof rune = utf8.RuneError
)

const (
	insideStr  = `\"`
	insideChar = `\'`
	digits     = "0123456789"
	dec_digits = digits + "_"
	hex_digits = digits + "ABCDEFabcdef_"
	bin_digits = "01_"
	letters    = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_" // yes _ is a letter, fuck you
)

func isNumber(r rune) bool {
	return strings.ContainsRune(digits, r)
}

func isLetter(r rune) bool {
	return strings.ContainsRune(letters, r)
}

func ignoreWhitespace(st *Lexer) {
	r := peekRune(st)
loop:
	for {
		switch r {
		case ' ', '\t', '\n':
			nextRune(st)
		case '#':
			comment(st)
		default:
			break loop
		}
		r = peekRune(st)
	}
	ignore(st)
}

// refactor this
func any(st *Lexer) (*ir.Node, *Error) {
	var r rune
	var tp T.LexKind

	ignoreWhitespace(st)

	r = peekRune(st)

	if isNumber(r) {
		return number(st), nil
	}
	if isLetter(r) {
		return identifier(st), nil
	}
	if r == '\'' {
		return charLit(st), nil
	}
	if r == '"' {
		return strLit(st), nil
	}

	switch r {
	case '+': // + +=
		nextRune(st)
		r = peekRune(st)
		switch r {
		case '=':
			nextRune(st)
			tp = T.PLUS_ASSIGN
		default:
			tp = T.PLUS
		}
	case '-': // - -= ->
		nextRune(st)
		r = peekRune(st)
		switch r {
		case '=':
			nextRune(st)
			tp = T.MINUS_ASSIGN
		case '>':
			nextRune(st)
			tp = T.ARROW
		default:
			tp = T.MINUS
		}
	case '/': // / /=
		nextRune(st)
		r = peekRune(st)
		switch r {
		case '=':
			nextRune(st)
			tp = T.DIVISION_ASSIGN
		default:
			tp = T.DIVISION
		}
	case '*': // * *=
		nextRune(st)
		r = peekRune(st)
		switch r {
		case '=':
			nextRune(st)
			tp = T.MULTIPLICATION_ASSIGN
		default:
			tp = T.MULTIPLICATION
		}
	case '%': // % %=
		nextRune(st)
		r = peekRune(st)
		switch r {
		case '=':
			nextRune(st)
			tp = T.REMAINDER_ASSIGN
		default:
			tp = T.REMAINDER
		}
	case '|': // || |^
		nextRune(st)
		r = peekRune(st)
		switch r {
		case '|':
			nextRune(st)
			tp = T.BITWISEOR
		case '^':
			nextRune(st)
			tp = T.BITWISEXOR
		default:
			return nil, InvalidSymbol(st, r)
		}
	case '&': // &&
		nextRune(st)
		r = peekRune(st)
		switch r {
		case '&':
			nextRune(st)
			tp = T.BITWISEAND
		default:
			return nil, InvalidSymbol(st, r)
		}
	case '@':
		nextRune(st)
		tp = T.AT
	case '^':
		nextRune(st)
		tp = T.CARET
	case '~':
		nextRune(st)
		tp = T.NEG
	case '(':
		nextRune(st)
		tp = T.LEFTPAREN
	case ')':
		nextRune(st)
		tp = T.RIGHTPAREN
	case '{':
		nextRune(st)
		tp = T.LEFTBRACE
	case '}':
		nextRune(st)
		tp = T.RIGHTBRACE
	case '[':
		nextRune(st)
		tp = T.LEFTBRACKET
	case ']':
		nextRune(st)
		tp = T.RIGHTBRACKET
	case ',':
		nextRune(st)
		tp = T.COMMA
	case ';':
		nextRune(st)
		tp = T.SEMICOLON
	case '.':
		nextRune(st)
		tp = T.DOT
	case ':': // : ::
		nextRune(st)
		r = peekRune(st)
		switch r {
		case ':':
			nextRune(st)
			tp = T.DOUBLECOLON
		default:
			tp = T.COLON
		}
	case '>': // >  >= >>
		nextRune(st)
		r = peekRune(st)
		switch r {
		case '=':
			nextRune(st)
			tp = T.MOREEQ
		case '>':
			nextRune(st)
			tp = T.SHIFTRIGHT
		default:
			tp = T.MORE
		}
	case '<': // <  <=  <<
		nextRune(st)
		r = peekRune(st)
		switch r {
		case '=':
			nextRune(st)
			tp = T.LESSEQ
		case '<':
			nextRune(st)
			tp = T.SHIFTLEFT
		default:
			tp = T.LESS
		}
	case '!': // ! !=
		nextRune(st)
		r = peekRune(st)
		switch r {
		case '=':
			nextRune(st)
			tp = T.DIFFERENT
		default:
			tp = T.BITWISENOT
		}
	case '=': // = ==
		nextRune(st)
		r = peekRune(st)
		if r == '=' {
			nextRune(st)
			tp = T.EQUALS
		} else {
			tp = T.ASSIGNMENT
		}
	case eof:
		nextRune(st)
		return &ir.Node{Lex: T.EOF}, nil
	default:
		return nil, InvalidSymbol(st, r)
	}
	return genNode(st, tp), nil
}

// sorry
func number(st *Lexer) *ir.Node {
	r := peekRune(st)
	var value *big.Int
	if r == '0' {
		nextRune(st)
		r = peekRune(st)
		switch r {
		case 'x': // he x
			nextRune(st)
			acceptRun(st, hex_digits)
			value = parseHex(st.Selected())
		case 'b': // b inary
			nextRune(st)
			acceptRun(st, bin_digits)
			value = parseBin(st.Selected())
		default:
			acceptRun(st, dec_digits)
			value = parseNormal(st.Selected())
		}
	} else {
		acceptRun(st, dec_digits)
		value = parseNormal(st.Selected())
	}
	r = peekRune(st)
	switch r {
	case 'p': // p ointer
		nextRune(st)
		return genNumNode(st, T.PTR_LIT, value)
	case 'u':
		nextRune(st)
		r = peekRune(st)
		switch r {
		case 's': // short
			nextRune(st)
			r = peekRune(st)
			if r == 's' { // shorter short
				nextRune(st)
				return genNumNode(st, T.U8_LIT, value)
			}
			return genNumNode(st, T.U16_LIT, value)
		case 'l': // long
			nextRune(st)
			r = peekRune(st)
			if r == 'l' { // longer long
				nextRune(st)
				return genNumNode(st, T.U64_LIT, value)
			}
			return genNumNode(st, T.U32_LIT, value)
		default:
			return genNumNode(st, T.U64_LIT, value)
		}
	case 's': // short
		nextRune(st)
		r = peekRune(st)
		if r == 's' { // shorter short
			nextRune(st)
			return genNumNode(st, T.I8_LIT, value)
		}
		return genNumNode(st, T.I16_LIT, value)
	case 'l': // long
		nextRune(st)
		r = peekRune(st)
		if r == 'l' { // longer long
			nextRune(st)
			return genNumNode(st, T.I64_LIT, value)
		}
		return genNumNode(st, T.I32_LIT, value)
	}
	return genNumNode(st, T.I64_LIT, value)
}

func identifier(st *Lexer) *ir.Node {
	r := peekRune(st)
	if !isLetter(r) {
		panic("identifier not beginning with letter")
	}
	acceptRun(st, digits+letters)
	selected := st.Selected()
	tp := T.IDENTIFIER
	switch selected {
	case "proc":
		tp = T.PROC
	case "data":
		tp = T.DATA
	case "const":
		tp = T.CONST
	case "var":
		tp = T.VAR
	case "true":
		return genNumNode(st, T.TRUE, big.NewInt(1))
	case "false":
		return genNumNode(st, T.FALSE, big.NewInt(0))
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
	case "as":
		tp = T.AS
	case "is":
		tp = T.IS
	case "all":
		tp = T.ALL
	case "type":
		tp = T.TYPE
	case "sizeof":
		tp = T.SIZEOF
	case "i8":
		tp = T.I8
	case "i16":
		tp = T.I16
	case "i32":
		tp = T.I32
	case "i64":
		tp = T.I64
	case "u8":
		tp = T.U8
	case "u16":
		tp = T.U16
	case "u32":
		tp = T.U32
	case "u64":
		tp = T.U64
	case "bool":
		tp = T.BOOL
	case "ptr":
		tp = T.PTR
	case "void":
		tp = T.VOID
	}
	return genNode(st, tp)
}

func comment(st *Lexer) *Error {
	r := nextRune(st)
	if r != '#' {
		panic("internal error: comment without '#'")
	}
	for !strings.ContainsRune("\n"+string(eof), r) {
		nextRune(st)
		r = peekRune(st)
	}
	nextRune(st)
	return nil
}

func strLit(st *Lexer) *ir.Node {
	r := nextRune(st)
	if r != '"' {
		panic("wong")
	}
	for {
		acceptUntil(st, insideStr)
		r := peekRune(st)
		if r == '"' {
			nextRune(st)
			return &ir.Node{
				Text:  st.Selected(),
				Lex:   T.STRING_LIT,
				Range: st.Range(),
			}
		}
		if r == '\\' {
			nextRune(st) // \
			nextRune(st) // escaped rune
		}
	}
}

func charLit(st *Lexer) *ir.Node {
	r := nextRune(st)
	if r != '\'' {
		panic("wong")
	}
	for {
		acceptUntil(st, insideChar)
		r := peekRune(st)
		if r == '\'' {
			nextRune(st)
			text := st.Selected()
			return &ir.Node{
				Text:  text,
				Lex:   T.CHAR_LIT,
				Range: st.Range(),
				Value: parseCharLit(text[1 : len(text)-1]),
			}
		}
		if r == '\\' {
			nextRune(st) // \
			nextRune(st) // escaped rune
		}
	}
}

func IsValidIdentifier(s string) bool {
	st := NewLexer("oh no", s)
	tks, err := st.ReadAll()
	if err != nil {
		return false
	}
	if len(tks) != 1 { // we want only ID
		return false
	}
	return tks[0].Lex == T.IDENTIFIER
}

var big10 = big.NewInt(10)

func parseNormal(text string) *big.Int {
	output := big.NewInt(0)
	scratch := big.NewInt(0)
	for i := range text {
		if text[i] == '_' {
			continue
		}
		output.Mul(output, big10)
		char := text[i]
		if char >= '0' || char <= '9' {
			scratch.SetInt64(int64(char - '0'))
			output.Add(output, scratch)
		} else {
			panic(text)
		}
	}
	return output
}

var big16 = big.NewInt(16)

func parseHex(oldText string) *big.Int {
	text := oldText[2:]
	output := big.NewInt(0)
	scratch := big.NewInt(0)
	for i := range text {
		if text[i] == '_' {
			continue
		}
		output.Mul(output, big16)
		char := text[i]
		if char >= '0' && char <= '9' {
			scratch.SetInt64(int64(char - '0'))
			output.Add(output, scratch)
		} else if char >= 'a' && char <= 'f' {
			scratch.SetInt64(int64(char-'a') + 10)
			output.Add(output, scratch)
		} else if char >= 'A' && char <= 'F' {
			scratch.SetInt64(int64(char-'A') + 10)
			output.Add(output, scratch)
		} else {
			panic(text)
		}
	}
	return output
}

var big2 = big.NewInt(2)

func parseBin(oldText string) *big.Int {
	text := oldText[2:]
	output := big.NewInt(0)
	scratch := big.NewInt(0)
	for i := range text {
		if text[i] == '_' {
			continue
		}
		output.Mul(output, big2)
		char := text[i]
		if char == '0' || char == '1' {
			scratch.SetInt64(int64(char - '0'))
			output.Add(output, scratch)
		} else {
			panic(text)
		}
	}
	return output
}

func parseCharLit(text string) *big.Int {
	value := uint64(text[0])
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
	output := big.NewInt(0)
	return output.SetInt64(int64(value))
}

func InvalidSymbol(st *Lexer, r rune) *Error {
	message := fmt.Sprintf("Invalid symbol: %v", string(r))
	err := NewLexerError(st, et.InvalidSymbol, message)
	return err
}
