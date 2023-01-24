# Compiler structure 

The following is a folder tree together with the lines of code
in each directory, each package is a single file,
each file alone in it's folder.

```
.                      149
├── amd64              819
├── core               125
│   ├── errorkind      220
│   ├── hir            335
│   │   ├── checker    470
│   │   ├── class      43
│   │   ├── flowkind   26
│   │   ├── instrkind  91
│   │   └── util       59
│   ├── mir            298
│   │   ├── checker    740
│   │   ├── class      33
│   │   ├── flowkind   26
│   │   ├── instrkind  90
│   │   └── util       60
│   ├── module         199
│   │   ├── lexkind    295
│   │   └── symbolkind 30
│   ├── types          197
│   └── util           54
├── lexer              719
├── linearization      712
├── messages           263
├── parser             1131
├── pipelines          147
├── resalloc           1152
├── resolution         494
├── testing            141
└── typechecker        828
```

`core` is where all the main datastructures live, the intermediate
representations are each in a separate folder: `hir`, `mir` and `module`.
Additionally, `type` is a representation of types that is used in all
other representations.
The package `core` itself has the Error data structure, and this compiler
has very bad error messages, my advice is just to not write incorrect code.

`hir` and `mir` directories have the same structure, each
carries a `checker` module that does semantic validation
on the IR, the compiler uses this to validate transformations.

`pipelines` is the second most important package, it ties together
all passes into pipelines.

The `testing` is a simple testing framework to process files from the
`test_suite` folder and validate the correctness of the compiler.

`messages` is a package containing most messages related to user errors.

The rest of the folders outside of `core` are compiler passes, they
form the following compiler pipeline:

```
     | file
     v                                      
 --------------  module    --------------- 
 | resolution | ---------> | typechecker | 
 --------------            --------------- 
  | file     ^             typed module |  
  v      ast |                          v  
 ------------------      ----------------- 
 | lexer | parser |      | linearization | 
 ------------------      ----------------- 
                                    hir |  
                                        V  
 ---------              mir   ------------ 
 | amd64 | <----------------- | resalloc | 
 ---------                    ------------ 
  | fasm program                           
  v                                        
 --------             elf                  
 | fasm | -------------------------------->
 --------                                  
```

 - `resolution` resolves symbol names *and* modules
 - `parser` is a recursive descent parser.
 - `typechecker` is a (bad) typechecker that uses simple local inference.
 - `resalloc` is a linear scanning register allocator.
 - `amd64` is a (bad) code generator that transforms MIR into fasm code.
 - `fasm` refers to the assembler, it's an external dependency.

The rest of the file tries to document the semantic of each intermediate representation:

# IR

There are 3 Intermediate Representations:

	- HIR: high level representation of value flow between operations
	- MIR: representation of resource usage in an lower-level abstract machine
	- LIR: direct representation of a specific machine

HIR is transformed into MIR and MIR is transformed in
one of the many possible LIRs (there's only `amd64` for now)

## HIR

HIR's primary concern is regarding value flow between operations.

### HIR Classes

While in HIR, operands can have the following classes:

    temp    lit    local    global    arg

And are grouped as:

	Operable = temp|lit|local|global|arg
	Result   = temp|local|arg
 
#### TEMP

Temporaries are created implicitly, without need for declaration.
Their lifetime is bound to a basic block.
A TEMP cannot be assigned twice.
They are considered clean values.

#### LIT

Literals are just numbers, nothing fancy.

#### LOCAL

Locals represent values bound inside a procedure
and are considered dirty values.

#### ARG

ARGs represent values passed as parameters for a procedure
and are considered dirty values.

#### GLOBAL

Globals are the symbols living in global scope.
They are procedures and memory declarations.

### HIR Types

HIR basic types are the following:

    i8    i16    i32    i64    ptr    bool

And are grouped as:

	Any    = i8|i16|i32|i64|ptr|bool|proc
	NonPtr = i8|i16|i32|i64|bool
	Number = i8|i16|i32|i64|ptr
	
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

There are 7 types of storage classes:

	register    spill    static
	local       lit      callee_interproc
	caller_interproc
	
They are grouped as:

	Immediate = register|lit|static
	Addressable = spill|callee_interproc|local|caller_interproc

Interprocs and Registers are volatile, they can be corrupted on
procedure calls, so if a value that is currently residing in
the Interproc area or in a register is needed after a procedure call,
it's the job of the resource allocator to make sure the value is preserved.

Spills and locals are always preserved between procedure calls.

#### REGISTER

Registers represent scratch space that need to be used to
perform most operations. They are volatile between procedure calls.

#### INTERPROC

Interproc areas are corruptible by the Callee procedure simply
because they are addressable by both Caller and Callee. The caller procedure
cannot bet on the Interproc areas being safe, since they may be
reused to return values.

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
	
	LoadPtr:T of Any [ptr'Immediate] -> T'register
	StorePtr:T of Any [T'Immediate], ptr'Immediate

	Load:T of Any [T'Addressable] -> T'register
	Store:T of Any [T'Immediate] -> T'Addressable

	Copy:T of Any [T'Immediate] -> T'register

	Call [proc'proc]
```

## LIR

### Instructions

```
Add:
	add dest/a, b

	mov dest, a
	add dest, b
Sub:
	sub dest/a, b
	
	mov dest, a
	sub dest, b
Div:
	xor rdx, rdx
	mov rax, a
	idiv b
	mov dest, rax
Rem:
	xor rdx, rdx
	mov rax, a
	idiv b
	mov dest, rdx
Mult:
	imul dest/a, b

	mov dest, a
	imul dest, b
Eq:
	cmp a, b;
	sete dest
Diff:
	cmp a, b;
	setne dest
Less:
	cmp a, b;
	setl dest
More:
	cmp a, b;
	setg dest
LessEq:
	cmp a, b;
	setle dest
MoreEq:
	cmp a, b;
	setge dest

And:
	and dest/a, b

	mov dest, a
	and dest, b
Or:
	or  dest/a, b

	mov dest, a
	or dest, b
Not:
	cmp a, 0
	sete a

UnaryMinus:
	neg dest/a

	mov dest, a
	neg dest
UnaryPlus:
	does nothing?

LoadPtr:
	mov dest, type [a]
StorePtr:
	mov type [dest], a
Load:
	mov dest, type [a]
Store:
	mov type [dest], a
Copy:
	mov dest, a

Convert i8 -> i16:
	movsx ax, al
Convert i8 -> i32:
	movsx eax, al
Convert i8 -> i64:
	movsx rax, al
Convert i16 -> i32:
	movsx eax, ax
Convert i16 -> i64:
	movsx rax, ax
Convert i32 -> i64:
	movsx rax, eax
Convert t -> t
	mov
Convert bool -> numerical
	mov
Call:
	call a
```

### Built-in procedures

```
write:
	push 	rbp
	mov	rbp, rsp

	mov	rdx, [rbp+16]
	mov	rsi, [rbp+24]
	mov	rdi, 1 ; STDOUT
	mov	rax, 1 ; SYS_WRITE
	syscall

	mov 	rsp, rbp
	pop 	rbp
	ret

read:
error:
```

### Control Flow

```
exit:
	mov	rdi, a
	mov	rax, SYS_EXIT
	syscall
```

### Stack frame

The address always points to the end of the slot
so "ARP" points to the end of "Caller ARP" and
the beginning of the "Local#0"

`#local`  -> "number of locals"
`#spill`  -> "number of spills"
`#callerarg` -> "number of caller arguments"
`#calleearg` -> "number of callee arguments"

```
    Stack frame     |    Address
|-------------------|----------------------------
| CallerInterproc#N | <- ARP + 16 + (N*8)
| ...               |
| CallerInterproc#0 | <- ARP + 16
| Return Address    |
| Caller ARP        | <- ARP (rbp)
| Local#0           |
| ...               |
| Local#N           | <- ARP - (8 + N*8)
| Spill#0           |
| ...               |
| Spill#N           | <- ARP - (8 + #local*8 + N*8)
| CalleeInterproc#N | <- ARP - (8 + #local*8 + #spill*8 + (#calleearg-N)*8)
| ...               |
| CalleeInterproc#1 | <- ARP - (8 + #local*8 + #spill*8 + (#calleearg-2)*8)
| CalleeInterproc#0 | <- ARP - (8 + #local*8 + #spill*8 + (#calleearg-1)*8)
```

`Interproc` means interprocedural regions where both caller and callee can
access, they are corruptible by the callee.

This last line may seem strange: `ARP - (8 + #local*8 + #spill*8 + (#calleearg-1)*8)`
but it's because `#calleearg` is a quantity, and the `N` is an index, so they're off by 1.
