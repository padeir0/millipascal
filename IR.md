# IR

There are 3 Intermediate Representations:

	- HIR: high level representation of value flow between operations
	- MIR: representation of resource usage in an lower-level abstract machine
	- LIR: direct representation of a specific machine

HIR is transformed into MIR and MIR is transformed in
one of the many possible LIRs.

## HIR

HIR's primary concern is regarding value flow between operations.

### HIR Classes

While in HIR, operands can have the following classes:

	temp	lit	local	global

And are grouped as:
	Operable = temp|lit|local|global
	Result   = temp|local
 
#### TEMP

Temporaries are created implicitly, without need for declaration.
Their lifetime is bound to a basic block.
A TEMP cannot be assigned twice.
They are considered clean values.

#### LIT

Literals are just numbers, nothing fancy.

#### LOCAL

Locals represent values bound inside a procedure.
They represent both declared variables and arguments
and are considered dirty values.

#### GLOBAL

Globals are the symbols living in global scope.
They are procedures and memory declarations.

### HIR Types

HIR basic types are the following:

	i8	i16	i32	i64	ptr	bool

And are grouped as:

	Any    = i8|i16|i32|i64|ptr|bool
	NonPtr = i8|i16|i32|i64|bool
	Number = i8|i16|i32|i64
	
The names of groups start with a capital letter to be differentiable from
single types.

### HIR Instructions

```
	Add:T of Number [T'Operable, T'Operable] -> T'Result
	Sub
	Div
	Mult
	Rem

	Eq:T of Any [T'Operable, T'Operable] -> bool'Result
	Diff
	Less
	More
	LessEq
	MoreEq

	Or:bool [bool'Operable, bool'Operable] -> bool'Result
	And

	Not:bool [bool'Operable] -> bool'Result

	UnaryMinus:T of Number [T'Operable] -> T'Result
	UnaryPlus

	Convert:T of NonPtr [NonPtr'Operable] -> T'Result

	Offset:T of Number [ptr'Operable, T'Operable] -> ptr'Result
	LoadPtr:T of Any  [ptr'Operable] -> T'Result
	StorePtr:T of Any [T'Operable], ptr'Operable

	Copy:T of Any [T'Operable] -> T'Result

	Call [proc'Operable, Any'Operable...] -> Any'Result...
```

The `Call` instruction is the only instruction where you can have
multiple destinations.

## MIR

MIR's primary concern is with allocation of resources based on an
low level abstract machine. This machine has a limited number
of volatile registers that can be corrupted on procedure calls.
It needs explicit management of regions where comunication
between procedures occur.

### MIR Classes

There are 6 types of storage classes:

	register	spill		callee_interproc
	local		lit		static
	
They are grouped as:

	Immediate = register|lit|static
	Addressable = spill|callee_interproc|local

Interprocs and Registers are volatile, they can be corrupted on
procedure calls, so if a value that is currently residing in
the Interproc area or in a register is needed after a procedure call,
it's the job of the register allocator to make sure the value is preserved.

Spills and locals are always preserved between procedure calls.
Literals are literally just numbers, really.

#### REGISTER

Registers represent scratch space that need to be used to
perform most operations. They are volatile between procedure calls.

#### INTERPROC

Interproc areas are corruptible by the Callee procedure simply
because they are addressable by the Callee. The caller procedure
cannot bet on the Interproc areas being safe, since they may be
reused to return values.

Furthemore, Interproc size is variable, they may change to allocate
space for another procedure call.

#### LOCAL

Locals are values declared by the user that are private to a procedure,
and are preserved between procedure calls.

#### SPILL

Spills are regions of memory reserved by the register allocator to
save values of registers if the register is going to be corrupted
and the value is still needed.

#### LIT

nUmBErS

#### STATIC

Static refers to global procedures and memory declarations, they are like
global constants and are essentially address numbers.

### MIR Instructions

In MIR, the typing rules are the same as HIR, but with the added constraint
that the operands must be of specific storage classes.

The annotations bellow show the type and operand class as:

	type'class

On the left side of the arrow are the operands, surrounded by brackets,
while on the right side of the arrow, if present, is the single destination.
The instructions are typed, and the following notation is used to show
how operands must be typed and classed relative to the instruction type:

	instr:T of Constraint [T'class, T'class] -> T'class

Where all occurrences of `T` must be substituted
by one type inside the constraint. If a type constraint is used directly,
without being bounded by `T`, the type is not bounded by the instruction's
type.
	
As before, the names of groups start with a capital letter
to be differentiable.

```
	Add:T of Number [T'Immediate, T'Immediate] -> T'register
	Sub
	Div
	Mult
	Rem

	Eq:T of Any [T'Immediate, T'Immediate] -> bool'register
	Diff
	Less
	More
	LessEq
	MoreEq

	Or:bool [bool'Immediate, bool'Immediate] -> bool'register
	And

	Not:bool [bool'Immediate] -> bool'register

	UnaryMinus:T of Number [T'Immediate] -> T'register
	UnaryPlus

	Convert:T of NonPtr [NonPtr'Immediate] -> T'register

	Offset:T of Number [ptr'Immediate, T'Immediate] -> ptr'register
	LoadPtr:T of Any [ptr'Immediate] -> T'register
	StorePtr:T of Any [T'Immediate], ptr'Immediate

	Load:T of Any [T'Addressable] -> T'register
	Store:T of Any [T'Immediate] -> T'Addressable

	Copy:T of Any [T'Immediate] -> T'register

	Call [proc'proc]

	IncFrame [Number'lit]
	DecFrame [Number'lit]
```

TODO: LIR spec for x86
## LIR
