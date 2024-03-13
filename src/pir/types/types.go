package Type

import "strings"

type Type struct {
	Basic BasicType
	Proc  *ProcType
}

func (t *Type) String() string {
	if t == nil {
		return "nil"
	}
	switch t.Basic {
	case I8:
		return "i8"
	case I16:
		return "i16"
	case I32:
		return "i32"
	case I64:
		return "i64"
	case U8:
		return "u8"
	case U16:
		return "u16"
	case U32:
		return "u32"
	case U64:
		return "u64"
	case Bool:
		return "bool"
	case Ptr:
		return "ptr"
	case Void:
		return "void"
	}
	if t.Proc != nil {
		return t.Proc.String()
	}
	return "invalid type"
}

func (this *Type) Equals(other *Type) bool {
	if IsBasic(this) && IsBasic(other) {
		return this.Basic == other.Basic
	}
	// one is basic and the other is not
	if IsBasic(this) || IsBasic(other) {
		return false
	}
	if this.Proc != nil && other.Proc != nil {
		return this.Proc.Equals(other.Proc)
	}
	panic("cannot compare " + this.String() + " with " + other.String())
}

// returns the size of this type in bytes
// panics if the type is special
func (this *Type) Size() int {
	if IsBasic(this) {
		switch this.Basic {
		case Bool:
			return 1
		case I8, U8:
			return 1
		case I16, U16:
			return 2
		case I32, U32:
			return 4
		case I64, U64:
			return 8
		case Ptr:
			return 8
		}
	}
	if IsProc(this) {
		return 8
	}
	panic("unsizeable type")
}

type ProcType struct {
	Args []*Type
	Rets []*Type
}

func (this *ProcType) String() string {
	output := "proc["
	decls := []string{}
	for _, t := range this.Args {
		decls = append(decls, t.String())
	}
	output += strings.Join(decls, ", ") + "]["
	decls = []string{}
	for _, t := range this.Rets {
		decls = append(decls, t.String())
	}
	output += strings.Join(decls, ", ") + "]"
	return output
}

func (this *ProcType) Equals(other *ProcType) bool {
	if len(this.Args) != len(other.Args) ||
		len(this.Rets) != len(other.Rets) {
		return false
	}
	for i := range this.Args {
		if !this.Args[i].Equals(other.Args[i]) {
			return false
		}
	}
	for i := range this.Rets {
		if !this.Rets[i].Equals(other.Rets[i]) {
			return false
		}
	}
	return true
}

var T_I64 = &Type{Basic: I64}
var T_I32 = &Type{Basic: I32}
var T_I16 = &Type{Basic: I16}
var T_I8 = &Type{Basic: I8}
var T_U64 = &Type{Basic: U64}
var T_U32 = &Type{Basic: U32}
var T_U16 = &Type{Basic: U16}
var T_U8 = &Type{Basic: U8}
var T_Bool = &Type{Basic: Bool}
var T_Ptr = &Type{Basic: Ptr}
var T_Void = &Type{Basic: Void}
var T_MainProc = &Type{Proc: &ProcType{Args: []*Type{}, Rets: []*Type{}}}

type BasicType int

const (
	InvalidBasicType BasicType = iota

	Bool
	I8
	I16
	I32
	I64
	U8
	U16
	U32
	U64
	Ptr
	Void
)

func IsInvalid(t *Type) bool {
	if t == nil {
		return false
	}
	return t.Basic == InvalidBasicType &&
		t.Proc == nil
}

func IsVoid(tt *Type) bool {
	return tt.Basic == Void
}

func IsBasic(tt *Type) bool {
	return tt.Basic != InvalidBasicType
}

func IsProc(tt *Type) bool {
	return tt.Proc != nil
}

func IsBasicOrProc(tt *Type) bool {
	return IsBasic(tt) || IsProc(tt)
}

func IsBool(t *Type) bool {
	return IsBasic(t) && t.Basic == Bool
}

func IsInteger(t *Type) bool {
	if !IsBasic(t) {
		return false
	}
	b := t.Basic
	return b == I8 ||
		b == I16 ||
		b == I32 ||
		b == I64 ||
		b == U8 ||
		b == U16 ||
		b == U32 ||
		b == U64
}

func IsSigned(t *Type) bool {
	if !IsBasic(t) {
		return false
	}
	b := t.Basic
	return b == I8 ||
		b == I16 ||
		b == I32 ||
		b == I64
}

func IsUnsigned(t *Type) bool {
	if !IsBasic(t) {
		return false
	}
	b := t.Basic
	return b == U8 ||
		b == U16 ||
		b == U32 ||
		b == U64
}

func IsPtr(t *Type) bool {
	return IsBasic(t) && t.Basic == Ptr
}
