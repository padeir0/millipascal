# Millipascal Core

Initial subset that is able to bootstrap itself.

Constraints:
 - ASCII based
 - Numerical and symbolical consts and blobs
 - Only plain arithmetical constant evaluation, no complex procedure call stuff
 - Only static pointer-arithmetic syntax-sugar (structs)

```ebnf
id := letter {letter | digit}.
letter = 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|
         'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|
         'u'|'v'|'w'|'x'|'y'|'z'|'A'|'B'|'C'|'D'|
         'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|
         'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|
         'Y'|'Z'|'_'.
digits = '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'.
decDigits = digits | '_'.
hexDigits = digits | 'A'|'B'|'C'|'D'|'E'|'F'|'a'|'b'|'c'|'d'|'e'|'f'|'_'.
binDigits = '0'|'1'|'_'.
numEnding = 'p'|'s'|'ss'|'l'|'ll'|'us'|'uss'|'ul'|'ull'.
number := decimal | hexadecimal | binary.
decimal := digits {decDigits} [numEnding].
hexadecimal := '0x' hexDigits {hexDigits} [numEnding].
binary := '0b' binDigits {binDigits} [numEnding].

escapes := '\\"' | '\\'' | '\\n' | '\\t' | '\\r'.
string := '"' {ascii|escapes} '"'.
char := '\'' (ascii|escapes) '\''.

keywords :=
    'var'   | 'proc'   | 'begin'  | 'end' |
    'while' | 'if'     | 'else'   | 'elseif' |
    'or'    | 'and'    | 'not'    | 'data' |
    'i8'    | 'i16'    | 'i32'    | 'i64' |
    'u8'    | 'u16'    | 'u32'    | 'u64' |
    'bool'  | 'ptr'    | 'true'   | 'false' |
    'exit'  | 'import' | 'from'   | 'export' |
    'const' | 'sizeof' | 'return' | 'set' |
    'struct'.

ponctuation :=
    ','  | ':'  | '('  | ')'  | '['  | ']' |
    '='  | '==' | '!=' | '>'  | '>=' | '<' |
    '<=' | '+'  | '-'  | '*'  | '/'  | '%' |
    '-=' | '+=' | '*=' | '/=' | '%=' | '.' |
    '@'  | '::' | '~'  | '&&' | '||' | '!' |
    '|^' | '>>' | '<<' | '->' | '^'.

basicType :=
    'i8' | 'i16' | 'i32' | 'i64' | 'ptr' | 'bool' |
    'u8' | 'u16' | 'u32' | 'u64'



Module := {Coupling} {AttSymbol}.

Coupling := Import | FromImport | Export.

Import := 'import' IdList.
FromImport := 'from' id 'import' IdList.
Export := 'export' IdList.
IdList := id {',' id} [','].

AttSymbol := [Attributes] Symbol.
Attributes:= 'attr' IdList.

Symbol := Procedure
    | Data
    | Const
    | Struct.

Const := 'const' (SingleConst|MultipleConst).
SingleConst := id '=' Expr.
MultipleConst := 'begin' {SingleConst [';']} 'end'.

Data := 'data' (SingleData|MultipleData).
MultipleData := 'begin' {SingleData [';']} 'end'.
SingleData :=  id (DExpr|string|Blob|Typed).
Blob := '{' [Annot] ExprList '}'.
DExpr := '[' Expr ']'.
Typed := Annot.

Struct := 'struct' id [Size] 'begin' Fields 'end'.
Size := '[' Expr ']'.
Field := id [Annot] [Offset].
Offset := '{' Expr '}'.

Procedure := 'proc' id [Args [Rets]] [Vars] Block.
Args := '[' [DeclList] ']'.
Vars := 'var' DeclList.
Rets := TypeList.
TypeList := Type {',' Type} [','].

DeclList := Decl {',' Decl} [','].
Decl := idList Annot.
Annot := ':' Type.

Type := basicType | ProcType | TName.
ProcType := 'proc' '[' [TypeList] ']' ProcTypeRet.
ProcTypeRet := '[' [TypeList] ']'
             | Type.
TName := Name ['.' id].

Block := 'begin' {CodeSemicolon} 'end'.

CodeSemicolon := Code [';'].

Code := If
      | While
      | Return
      | Set
      | Exit
      | Expr.

While := 'while' Expr Block.

If   := 'if' Expr Block {ElseIf} [Else].
ElseIf := 'elseif' Expr Block.
Else := 'else' Block.

Set := 'set' ExprList assignOp Expr.
assignOp := '=' | '-=' | '+=' | '/=' | '*=' | '%='.

Return := 'return' [ExprList].
Exit := 'exit' ['^'] Expr.

ExprList := Expr {',' Expr} [','].
Expr := And {'or' And}.
And := Comp {'and' Comp}.
Comp := Sum {compOp Sum}.
compOp := '==' | '!=' | '>' | '>=' | '<' | '<='.
Sum := Mult {sumOp Mult}.
sumOp := '+' | '-' | '||' | '|^'.
Mult := UnaryPrefix {multOp UnaryPrefix}.
multOp := '*' | '/' | '%' | '&&' | '<<' | '>>'.
UnaryPrefix := {Prefix} UnarySuffix.
UnarySuffix := Factor {Suffix}.

Prefix := 'not' | '~' | '!'.
Suffix := Conversion
    | Deref
    | Call
    | DotAccess
    | ArrowAccess.
Conversion := Annot.
Call := '[' [ExprList] ']'.
Deref := '@' Type.
DotAccess := '.' id.
ArrowAccess := '->' id.

Factor := Name
    | Literal
    | NestedExpr
    | "sizeof" Type.
NestedExpr := '(' Expr ')'.
Literal := true | false | number | char.
Name := id ["::" id].
```

## NOTE 1

When we do `data D:T`, T must be a struct, since data is always a pointer.
What we do then, is have something like:
```
struct I64 begin
    num:i64 {0}
end
```

And then we can access it with `D->num` or `D@i64`,
whatever floats your boat. The first one guarantees that
you'll never try to read a `ptr` or `u64` from a memory location
that should have an `i64` inside, which is better.

## NOTE 2

There are three ways to declare space for a struct,
considering the struct:

```
struct POINT begin
    X:i64 Y:i64
end
```

We can have these three `data` declarations:

```
data P [sizeof POINT]
data Q:POINT
data R {:POINT 1, 1}
```

The types are as follows:
 - `P` has type `ptr`
 - `Q` has type `POINT` (which is also a pointer)
 - `R` has type `ptr`

It's important that `R` is a pointer, since it may contain more
than one point inside it. Further improvements should address
syntax sugar for array-like structures.

These `data` declarations can be acessed like so:

```
proc main
var p:ptr, r:ptr
begin
    set p = P:POINT
    set p->X = 1
    set p->Y = 1

    set Q->X = 1
    set Q->Y = 1

    set r = R:POINT

    if r->X != Q->X or r->Y != Q->Y begin
        exit 1ss
    end

    if p->X != Q->X or p->Y != Q->Y begin
        exit 2ss
    end
end
```

We can cast `P` to `POINT` (`P:POINT`) since structures are just
pointer related syntax-sugar.

## NOTE 3

It is entirely possible to refer to previous struct fields when
declaring field offsets, and it is also possible to omit the
struct name.

```
struct UNPADDED begin
    X:i32 {0}
    Y:i64 {UNPADDED.X + sizeof i32}
    Z:i64 {Y + sizeof i64} # UNPADDED may be omitted
end
```

## NOTE 4

A struct can be used to type a blob if,
and only if it is *well-behaved* (see note 8).
If there is padding, the compiler should
pad it with zeroes.

```
struct Row begin
    X:i64 Y:i32 Proc:proc[ptr]
end

data Table
{:Row
     1,  2l,     ZeroX,
     3,  4l,     ZeroX,
}
```

Typechecking the table proceeds as a state machine, each item
should correspond in order to a field in the struct, resetting
the position after the last item is assigned.

## NOTE 5

```
struct T begin
    tag:i8   {0}
    contents {1}
end
```

`T.contents` is untyped, if `a` is of type `T` then
`a->contents` is not allowed, but allows `(a.contents):U`,
whatever struct `U` may be.

## NOTE 6

`sizeof` can be used in a struct if it is *well-behaved* (see note 8)
or if struct size is set explicitly.

## NOTE 7

It's entirely possible to do this:

```
struct POS {sizeof i64} begin
    Previous:i64 {~sizeof i64}
    Current:i64 {0}
    Next:i64 {sizeof i64}
end
```

And then, we can iterate an array using
`set item = (array + (i*sizeof POS):ptr):POS`,
which gives us `item->Previous`, `item->Current` and `item->Next`
to look at any given moment.

## NOTE 8

We call a struct **well-behaved** if, and only if
the struct contains no overlapping fields, no untyped fields,
no negative offsets and the fields fit entirely whithin struct
size.

All C structures without inner use of unions are well-behaved,
but not all well-behaved structures are C structures. This is
because the user is allowed to choose arbitrary padding.

## NOTE 9

Given the following struct:

```
struct POINT begin
    X:i64
    Y:i64
end
```

We can access a field in the following ways, all of which are
equivalent:

```
proc zero_x[p:POINT]
begin
    set (p+POINT.X)@i64 = 0
    set p.X@i64 = 0
    set p->X = 0
end
```

This means the following:
 - `POINT.X` is a global constant of type `ptr`
 - `p.X` calculates the offset `p+POINT.X`
 - `p->X` calculates the offset and uses the type information to make a deref

## NOTE 10

We should allow `sizeof POINT.X` to give the size of the underlying
type of a field, such that the following becomes the canonical way
the compiler computes the offsets:

```
struct POINT begin
    X:i64 {0}
    Y:i64 {X + sizeof POINT.X}
    Z:i64 {Y + sizeof Y}
end
```

We should also allow `sizeof Y` like above if we're inside a struct.

## NOTE 11

Note that structures only dictate the behaviour of pointers,
so, although `sizeof STRUCT` gives the size of the whole object,
if we declare a struct with an inner struct:

```
struct A begin
    X:i64
    INNER:B
end
struct B begin
    X:i64, Y:i64
end
```

Then `sizeof A.INNER` should output the same as `sizeof ptr`.
Consider the following `data` declarations:

```
data D1 [sizeof A]
data D2 [sizeof ptr]
data D3:A
```

This has the weird behaviour that `sizeof D1 == sizeof D3`
and that `sizeof D2 != sizeof D1`, even though `A` dictates
the behaviour of a pointer. When passing an argument to a procedure:

```
proc P[a, b:A]
begin
end
```

Both `a` and `b` are pointers, so it's possible to pass `D1` and `D3`
as arguments, as long as there is a cast (in case of `D1`). But passing
`D2` with a cast could lead to a segfault.

If we want to declare an array of structs, this behaviour becomes
less intuitive. Consider the following:

```
data M1 [5 * sizeof ptr]
data M2 [5 * sizeof A]
```

In this case, `sizeof M1 != sizeof M2`, and data inside `M2` would
need to be inlined. Procedures for accessing M1 and M2 would differ:

```
proc access_M1[i:i64] A
begin
    return (M1+(i*sizeof ptr):ptr)@A
end

proc access_M2[i:i64] A
begin
    return (M2+(i*sizeof A):ptr):A
end
```

The first one has to find the `ptr` pointing to an `A`, and
as such, needs a dereference,
while the second one just finds the index of the item `A`,
with a cast.
