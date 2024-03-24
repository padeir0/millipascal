package Type

import (
	"fmt"
	"math/big"
	"strings"
)

// Struct can only be not nil if BasicType is ptr
// otherwise, it might as well be ignored.
type Type struct {
	Basic  BasicType
	Proc   *ProcType
	Struct *Struct
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
	return this._equals(other, true)
}

func (this *Type) _equals(other *Type, structs bool) bool {
	if IsBasic(this) && IsBasic(other) {
		if this.Basic == Ptr && other.Basic == Ptr && structs {
			if this.Struct != nil && other.Struct != nil {
				return this.Struct._equals(other.Struct)
			} else {
				return this.Struct == other.Struct
			}
		}
		return this.Basic == other.Basic
	}
	// one is basic and the other is not
	if IsBasic(this) || IsBasic(other) {
		return false
	}
	if this.Proc != nil && other.Proc != nil {
		return this.Proc._equals(other.Proc, structs)
	}
	panic("cannot compare " + this.String() + " with " + other.String())
}

// returns the size of this type in bytes
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

var one = big.NewInt(1)
var two = big.NewInt(2)
var four = big.NewInt(4)
var eight = big.NewInt(8)

// returns what sizeof[T] would return
func (this *Type) Sizeof() *big.Int {
	if IsBasic(this) && this.Struct == nil {
		switch this.Basic {
		case Bool:
			return one
		case I8, U8:
			return one
		case I16, U16:
			return two
		case I32, U32:
			return four
		case I64, U64:
			return eight
		case Ptr:
			return eight
		}
	}
	if IsBasic(this) && this.Struct != nil {
		return this.Struct.Size
	}
	if IsProc(this) {
		return eight
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

func (this *ProcType) _equals(other *ProcType, structs bool) bool {
	if len(this.Args) != len(other.Args) ||
		len(this.Rets) != len(other.Rets) {
		return false
	}
	for i := range this.Args {
		if !this.Args[i]._equals(other.Args[i], structs) {
			return false
		}
	}
	for i := range this.Rets {
		if !this.Rets[i]._equals(other.Rets[i], structs) {
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

func IsStruct(t *Type) bool {
	return IsBasic(t) && t.Basic == Ptr && t.Struct != nil
}

type Struct struct {
	Module   string
	Name     string
	Fields   []Field
	FieldMap map[string]int
	Size     *big.Int
}

// IMPROVBOOT: move all toggle constants to a separate module
const sanityCheck = true

func (this *Struct) _equals(other *Struct) bool {
	if sanityCheck {
		if this.Module == other.Module &&
			this.Name == other.Name {
			if len(this.Fields) != len(other.Fields) {
				fmt.Printf("%v =?= %v", this, other)
				panic("equal names, different underlying structure")
			}
			for i := range this.Fields {
				if !this.Fields[i]._equals(other.Fields[i]) {
					fmt.Printf("%v =?= %v", this, other)
					panic("equal names, different underlying structure")
				}
			}
		}
	}
	return this.Module == other.Module && this.Name == other.Name
}

func (this *Struct) Field(name string) (Field, bool) {
	i, ok := this.FieldMap[name]
	if !ok {
		return Field{}, false
	}
	return this.Fields[i], true
}

func (this *Struct) Offsetof(name string) *big.Int {
	i, ok := this.FieldMap[name]
	if !ok {
		return nil
	}
	return this.Fields[i].Offset
}

func (this *Struct) Typeof(name string) *Type {
	i, ok := this.FieldMap[name]
	if !ok {
		return nil
	}
	return this.Fields[i].Type
}

type Field struct {
	Name   string
	Type   *Type
	Offset *big.Int
}

func (this Field) _equals(other Field) bool {
	return this.Name == other.Name && this.Type._equals(other.Type, false)
}
