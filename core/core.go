package core

import (
	"fmt"
	"io/ioutil"
	et "mpc/core/errorkind"
)

type SourceLocation struct {
	File      string
	Input     *string
	Line, Col int
}

func (sl *SourceLocation) String() string {
	return fmt.Sprintf("%v:%v:%v", sl.File, sl.Line, sl.Col)
}

type Error struct {
	Type  et.ErrorKind
	Debug string

	Info []Excerpt
}

// Example
// text.mp:1:9 E0001
//     a := ¬
//          ^ Invalid character
func (ce *Error) String() string {
	if ce == nil {
		return "<nil>"
	}
	first := ce.FirstInfo()
	if first == nil {
		return ce.Type.Debug() + "\t" + ce.Debug + "\n"
	}
	return first.Location.String() + " " +
		ce.ErrCode() + "\n" +
		ce.ExcerptsToStr()
}

func (ce *Error) ErrCode() string {
	return ce.Type.String()
}

func (ce *Error) FirstInfo() *Excerpt {
	if len(ce.Info) < 1 {
		return nil
	}
	return &ce.Info[0]
}

func (ce *Error) ExcerptsToStr() string {
	output := ""
	for _, v := range ce.Info {
		output += v.String() + "\n"
	}
	return output
}

type Excerpt struct {
	Location *SourceLocation
	Message  string
}

func (exc *Excerpt) String() string {
	text := ""
	if exc.Location == nil {
		return exc.Message
	}
	if exc.Location.Input == nil {
		f, err := ioutil.ReadFile(exc.Location.File)
		if err != nil { // shoudn't happen
			panic(err)
		}
		text = string(f)
	} else {
		text = *exc.Location.Input
	}
	line := getLine(text, exc.Location.Line)
	return pointColumn(line+"  ", exc.Location.Col, exc.Message)
}

func getLine(s string, sLine int) string {
	line := 0
	col := 0
	buff := []rune{}
	for _, r := range s {
		if r == '\n' {
			line++
			col = 0
		} else {
			col++
		}

		if line == sLine {
			buff = append(buff, r)
		}
	}
	return string(buff)
}

func pointColumn(s string, sCol int, message string) string {
	newS := "\033[0;32m" + s + "\033[0m" + "\n"
	for i, r := range s {
		if i == sCol {
			newS += "^ " + message
			break
		}
		if r == '\t' {
			newS += "\t"
		} else {
			newS += " "
		}
	}
	return newS
}
