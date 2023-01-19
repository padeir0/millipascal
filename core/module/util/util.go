package util

import (
	"fmt"
	. "mpc/core"
	et "mpc/core/errorkind"
	mod "mpc/core/module"
	T "mpc/core/module/lexkind"
	. "mpc/frontend/lexer"
)

func Track(st *Lexer, s string) {
	if IsTracking {
		fmt.Printf("%v: %v\n", s, st.Word.String())
	}
}

func NewCompilerError(st *Lexer, t et.ErrorKind, message string) *Error {
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

func AllTokens(s *Lexer) []*mod.Node {
	output := []*mod.Node{}
	for s.Word.Lex != T.EOF {
		output = append(output, s.Word)
		Next(s)
	}
	return output
}

func Consume(st *Lexer) (*mod.Node, *Error) {
	n := st.Word
	err := Next(st)
	return n, err
}

func AddLeaf(a *mod.Node, b *mod.Node) {
	a.Leaves = append(a.Leaves, b)
}

func Check(st *Lexer, tpList ...T.LexKind) *Error {
	for _, tp := range tpList {
		if st.Word.Lex == tp {
			return nil
		}
	}
	message := fmt.Sprintf("Expected one of %v: instead found '%v'",
		T.FmtToUser(tpList...),
		st.Word.String())

	err := NewCompilerError(st, et.ExpectedSymbol, message)

	err.Debug = fmt.Sprintf("found: %v, wanted: %v",
		T.FmtTypes(tpList...),
		T.FmtTypes(st.Word.Lex))
	return err
}

func Expect(st *Lexer, tpList ...T.LexKind) (*mod.Node, *Error) {
	for _, tp := range tpList {
		if st.Word.Lex == tp {
			return Consume(st)
		}
	}
	message := fmt.Sprintf("Expected one of %v: instead found '%v'",
		T.FmtToUser(tpList...),
		st.Word.String())

	err := NewCompilerError(st, et.ExpectedSymbol, message)

	err.Debug = fmt.Sprintf("found: %v, wanted: %v",
		T.FmtTypes(tpList...),
		T.FmtTypes(st.Word.Lex))
	return nil, err
}

func ExpectProd(st *Lexer, prod Production, name string) (*mod.Node, *Error) {
	n, err := prod(st)
	if err != nil {
		return nil, err
	}
	if n == nil {
		message := fmt.Sprintf("expected %v instead found %v", name, st.Word.String())
		err := NewCompilerError(st, et.ExpectedProd, message)
		return nil, err
	}
	return n, err
}

type Production func(st *Lexer) (*mod.Node, *Error)
type Validator func(*mod.Node) bool

/* RepeatBinary implements the following pattern
for a given Production and Terminal:

	RepeatBinary := Production {Terminal Production}

Validator checks for terminals.
Left to Right precedence
*/
func RepeatBinary(st *Lexer, prod Production, name string, v Validator) (*mod.Node, *Error) {
	last, err := prod(st)
	if err != nil {
		return nil, err
	}
	for v(st.Word) {
		parent, err := Consume(st)
		if err != nil {
			return nil, err
		}
		AddLeaf(parent, last)

		newLeaf, err := ExpectProd(st, prod, name)
		if err != nil {
			return nil, err
		}
		AddLeaf(parent, newLeaf)

		last = parent
	}
	return last, nil
}

/* Repeat implements the following pattern
for a given Production:

	Repeat := {Production}.
*/
func Repeat(st *Lexer, prod Production) ([]*mod.Node, *Error) {
	out := []*mod.Node{}
	n, err := prod(st)
	if err != nil {
		return nil, err
	}
	for n != nil {
		out = append(out, n)
		n, err = prod(st)
		if err != nil {
			return nil, err
		}
	}
	return out, nil
}

/*RepeatUnaryLeft implements the following pattern
for a given Production:

	RepeatUnaryLeft := {Production}.

But returns the first and last item in the tree.

It's Left associative: first<-second<-last
*/
func RepeatUnaryLeft(st *Lexer, prod Production) (*mod.Node, *mod.Node, *Error) {
	first, err := prod(st)
	if err != nil {
		return nil, nil, err
	}
	if first == nil {
		return nil, nil, nil
	}
	last := first
	for first != nil {
		n, err := prod(st)
		if err != nil {
			return nil, nil, err
		}
		if n == nil {
			break
		}
		AddLeaf(last, n)
		last = n
	}
	return first, last, nil
}

func RepeatUnaryRight(st *Lexer, prod Production) (*mod.Node, *mod.Node, *Error) {
	first, err := prod(st)
	if err != nil {
		return nil, nil, err
	}
	if first == nil {
		return nil, nil, nil
	}
	last := first
	for first != nil {
		n, err := prod(st)
		if err != nil {
			return nil, nil, err
		}
		if n == nil {
			break
		}
		AddLeaf(n, last)
		last = n
	}
	return first, last, nil
}

/* RepeatList implements the following pattern
for a given Production and Terminal:

	RepeatBinary := Production {Terminal Production}

Validator checks for terminals.

It differs from RepeatBinary in that it returns a slice
instead of a Tree with precedence
*/
func RepeatList(st *Lexer, prod Production, val Validator) ([]*mod.Node, *Error) {
	first, err := prod(st)
	if err != nil {
		return nil, err
	}
	if first == nil {
		return nil, nil
	}
	out := []*mod.Node{first}
	for val(st.Word) {
		Next(st)
		n, err := prod(st)
		if err != nil {
			return nil, err
		}
		out = append(out, n)
	}
	return out, nil
}

// Implements the pattern:
//    RepeatBinary := Production {',' Production} [','].
func RepeatCommaList(st *Lexer, prod Production) ([]*mod.Node, *Error) {
	first, err := prod(st)
	if err != nil {
		return nil, err
	}
	if first == nil {
		return nil, nil
	}
	out := []*mod.Node{first}
	for st.Word.Lex == T.COMMA {
		Next(st)
		n, err := prod(st)
		if err != nil {
			return nil, err
		}
		out = append(out, n)
	}
	if st.Word.Lex == T.COMMA {
		err := Next(st)
		if err != nil {
			return nil, err
		}
	}
	return out, nil
}

func CreateNode(nodes []*mod.Node, t T.LexKind) *mod.Node {
	return &mod.Node{
		Lex:    t,
		Leaves: nodes,
	}
}

func ExpectedEOF(s *Lexer) *Error {
	return NewCompilerError(s, et.ExpectedEOF, "unexpected symbol")
}
