package core

import (
	"fmt"
	"io/ioutil"
	et "mpc/core/errorkind"
	sv "mpc/core/severity"
	"strconv"
)

type Position struct {
	Line   int
	Column int
}

func (this Position) String() string {
	return strconv.FormatInt(int64(this.Line), 10) + ":" +
		strconv.FormatInt(int64(this.Column), 10)
}

func (this Position) LessThan(other Position) bool {
	if this.Line == other.Line {
		return this.Column < other.Column
	}
	return this.Line < other.Line
}

func (this Position) MoreThan(other Position) bool {
	if this.Line == other.Line {
		return this.Column > other.Column
	}
	return this.Line > other.Line
}

func (this Position) MoreOrEqualsThan(other Position) bool {
	if this.Line == other.Line {
		return this.Column >= other.Column
	}
	return this.Line > other.Line
}

type Range struct {
	Begin Position
	End   Position
}

func (this Range) String() string {
	if this.Begin.MoreOrEqualsThan(this.End) {
		return this.Begin.String()
	}
	return this.Begin.String() + " to " + this.End.String()
}

type Location struct {
	File  string
	Range *Range
}

func (this *Location) String() string {
	if this.Range != nil {
		return this.File + ":" +
			this.Range.String()
	}
	return this.File
}

func (this *Location) Source() string {
	if this == nil || this.Range == nil {
		return ""
	}
	contents, err := ioutil.ReadFile(this.File)
	if err != nil {
		fmt.Printf("%#v\n", this)
		panic(err) // internal error
	}
	currline := 0
	currcol := 0
	output := "    \u001b[36m"
	for _, r := range string(contents) {
		if currline >= this.Range.Begin.Line &&
			currline <= this.Range.End.Line {
			if currline == this.Range.Begin.Line &&
				currcol == this.Range.Begin.Column {
				output += "\u001b[31m"
			}
			if currline == this.Range.End.Line &&
				currcol == this.Range.End.Column {
				output += "\u001b[36m"
			}
			if r == '\n' {
				output += string(r) + "    "
			} else if r == '\t' {
				output += "    "
			} else {
				output += string(r)
			}
		}
		if r == '\n' {
			currline++
			currcol = 0
		} else {
			currcol++
		}
	}
	output += "\u001b[0m"
	return output
}

type Error struct {
	Code     et.ErrorKind
	Severity sv.Severity
	Message  string
	Location *Location
}

func (this *Error) String() string {
	source := this.Location.Source()
	message := this.Location.String() + " " +
		this.Severity.String() +
		": " + this.Message
	if source != "" {
		return message + "\n" + source
	}
	return message
}

func (this *Error) ErrCode() string {
	return this.Code.String()
}

func ProcessFileError(e error) *Error {
	return &Error{
		Code:    et.FileError,
		Message: e.Error(),
	}
}
