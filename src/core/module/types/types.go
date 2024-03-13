package types

import "fmt"

// index into TypeSpace.space
type TypeID int

func (this TypeID) String() string {
	return fmt.Sprintf("%d", int(this))
}

const (
	InvalidTypeID TypeID = iota
	Bool

	I8 // must be ordered like this so faster verification is possible
	I16
	I32
	I64
	U8
	U16
	U32
	U64

	Ptr
	Void
	_last
)

var basicTypes = []TypeID{Bool, I8, I16, I32, I64, U8, U16, U32, U64, Ptr, Void}

func NewProcType(args, rets []TypeID) *SourceType {
	return &SourceType{
		ID: InvalidTypeID,
		Base: BaseType{
			Simple: InvalidTypeID,
			Proc: &ProcType{
				Args: args,
				Rets: rets,
			},
		},
		Info:    nil,
		Builtin: false,
	}
}

/*
A type can be:
	- basic: of which SourceType.Builtin is true, and SourceType.Base.Simple == SourceType.ID
	- named with underlying basic, named or unnamed: then SourceType.Base.Simple is set to that basic/named/unnamed ID
	- unnamed: ptr with info: then SourceType.Base.Simple is set to ptr and Info is set to that info
	- unnamed: named with info: then SourceType.Base.Simple is set to that named ID, and Info is set to that info
	- unnamed: procedure: then SourceType.Base.Proc is set to the respective procedure type
*/
type SourceType struct {
	ID TypeID

	Base    BaseType
	Info    *Layout
	Builtin bool
}

func (this *SourceType) equals(other *SourceType) bool {
	if this.Info == nil && other.Info == nil {
		return this.Base.equals(other.Base)
	}
	if this.Info == nil || other.Info == nil {
		return false // at least one is not set
	}
	return this.Base.equals(other.Base) &&
		this.Info.equals(other.Info)
}

func (this *SourceType) hash() int {
	if this.Info == nil {
		return this.Base.hash()
	} else {
		return this.Base.hash() ^ this.Info.hash()
	}
}

type BaseType struct {
	Simple TypeID
	Proc   *ProcType
}

func (this BaseType) hash() int {
	if this.Simple == InvalidTypeID {
		return this.Proc.hash()
	}
	return int(this.Simple)
}

func (this BaseType) equals(other BaseType) bool {
	if this.Simple == InvalidTypeID && other.Simple == InvalidTypeID {
		return this.Proc.equals(other.Proc)
	}
	if this.Simple == InvalidTypeID || other.Simple == InvalidTypeID {
		return false
	}
	return this.Simple == other.Simple
}

type ProcType struct {
	Args []TypeID
	Rets []TypeID
}

func (this ProcType) hash() int {
	out := 0xCAFEBABE
	for _, id := range this.Args {
		out = out ^ 31*int(id)
		out = out >> 7
	}
	for _, id := range this.Rets {
		out = out ^ 31*int(id)
		out = out >> 7
	}
	return out
}

func (this *ProcType) equals(other *ProcType) bool {
	if len(this.Args) != len(other.Args) ||
		len(this.Rets) != len(other.Rets) {
		return false
	}
	for i := range this.Args {
		if this.Args[i] != other.Args[i] {
			return false
		}
	}
	for i := range this.Rets {
		if this.Rets[i] != other.Rets[i] {
			return false
		}
	}
	return true
}

type Layout struct {
	Simple TypeID
	Struct Struct
}

func (this *Layout) hash() int {
	if this.Simple == InvalidTypeID {
		return this.Struct.hash()
	}
	return int(this.Simple)
}

func (this *Layout) equals(other *Layout) bool {
	if this.Simple == InvalidTypeID && other.Simple == InvalidTypeID {
		return this.Struct.equals(other.Struct)
	}
	if this.Simple == InvalidTypeID || other.Simple == InvalidTypeID {
		return false
	}
	return this.Simple == other.Simple
}

func hashStr(s string) int {
	out := 0xDEADBABE
	for _, t := range s {
		out = (out ^ int(t)) * 0x5bd1e995
		out = out >> 15
	}
	return out
}

type Struct struct {
	Fields   []Field
	FieldMap map[string]int
}

func (this Struct) hash() int {
	out := 0xDEADCAFE
	for _, field := range this.Fields {
		out = (out ^ (int(field.Type) + int(field.Offset) + hashStr(field.Name))) * 0x5bd1e995
		out = out >> 11
	}
	return out
}

func (this Struct) equals(other Struct) bool {
	if len(this.Fields) != len(other.Fields) {
		return false
	}
	for i := range this.Fields {
		if !this.Fields[i].equals(other.Fields[i]) {
			return false
		}
	}
	return true
}

type Field struct {
	Name   string
	Offset int
	Type   TypeID
}

func (this Field) equals(other Field) bool {
	return this.Name == other.Name &&
		this.Offset == other.Offset &&
		this.Type == other.Type
}

type QualifiedName struct {
	Module string
	Name   string
}

func NewTypeSpace() *TypeSpace {
	tspace := &TypeSpace{
		named: map[QualifiedName]TypeID{},
		space: []*SourceType{},
		top:   0,
		tmap:  newtmap(),
	}
	for _, t := range basicTypes {
		st := &SourceType{
			ID: t,
			Base: BaseType{
				Simple: t,
				Proc:   nil,
			},
			Info:    nil,
			Builtin: true,
		}
		tspace.basic(st)
	}
	return tspace
}

type TypeSpace struct {
	named map[QualifiedName]TypeID
	space []*SourceType
	top   int

	tmap tmap
}

func (this *TypeSpace) basic(t *SourceType) {
	this.space[t.ID] = t
	if int(t.ID) > this.top {
		this.top = int(t.ID) + 1
	}
	this.tmap.Insert(t)
}

func (this *TypeSpace) Unnamed(t *SourceType) TypeID {
	id, ok := this.tmap.Lookup(t)
	if ok {
		return id
	}
	id = this.pushType(t)
	this.tmap.Insert(t)
	return id
}

func (this *TypeSpace) Named(Module, Name string) TypeID {
	qn := QualifiedName{
		Module: Module,
		Name:   Name,
	}
	id, ok := this.named[qn]
	if ok {
		return id
	}
	temp := &SourceType{
		ID:   InvalidTypeID,
		Base: BaseType{},
		Info: nil,
	}
	id = this.pushType(temp)
	return id
}

func (this *TypeSpace) UpdatedNamed(id, base TypeID) {
	this.space[id].Base.Simple = base
	this.tmap.Insert(this.space[id])
}

func (this *TypeSpace) Identical(a, b TypeID) bool {
	return a == b
}

func (this *TypeSpace) IsNumerical(a TypeID) bool {
	id := this.GetBase(a)
	return id >= I8 && id <= U64
}

func (this *TypeSpace) IsPointer(a TypeID) bool {
	id := this.GetBase(a)
	return id == Ptr
}

func (this *TypeSpace) IsBool(a TypeID) bool {
	id := this.GetBase(a)
	return id == Bool
}

func (this *TypeSpace) GetBase(a TypeID) TypeID {
	st := this.space[a]
	if st.Builtin {
		return a
	}
	if st.Base.Proc != nil {
		return a
	}
	if st.Base.Simple != InvalidTypeID {
		return this.GetBase(st.Base.Simple)
	}
	panic("unreachable")
}

func (this *TypeSpace) pushType(info *SourceType) TypeID {
	if this.top >= len(this.space) {
		this.space = append(this.space, make([]*SourceType, len(this.space))...)
	}
	id := TypeID(this.top)
	info.ID = id
	this.space[id] = info
	this.top++
	return id
}

func newtmap() tmap {
	return tmap{
		buckets: make([]*ll, 64),
	}
}

type tmap struct {
	buckets []*ll
}

func (this *tmap) Lookup(t *SourceType) (TypeID, bool) {
	index := t.hash() % len(this.buckets)
	list := this.buckets[index]
	current := list.Head
	for current != nil {
		if current.T.equals(t) {
			return current.T.ID, true
		}
		current = current.Next
	}
	return -1, false
}

func (this *tmap) Insert(t *SourceType) {
	_, ok := this.Lookup(t)
	if ok {
		panic("inserting type twice")
	}
	index := t.hash() % len(this.buckets)
	list := this.buckets[index]
	list.Push(t)
}

type ll struct {
	Head *llnode
	Tail *llnode
}

func (this *ll) Push(t *SourceType) {
	next := &llnode{
		T:    t,
		Next: nil,
	}
	if this.Head == nil {
		this.Head = next
	}
	if this.Tail != nil {
		this.Tail.Next = next
	}
	this.Tail = next
}

type llnode struct {
	T    *SourceType
	Next *llnode
}
