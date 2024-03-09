# Millipascal Core

Initial subset that is able to bootstrap itself.

Constraints:
 - ASCII based, except for comments
 - Only plain arithmetical constant evaluation, no complex procedure call stuff

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
    'var'    | 'proc'   | 'begin'  | 'end'    |
    'while'  | 'if'     | 'else'   | 'elseif' |
    'or'     | 'and'    | 'not'    | 'data'   |
    'i8'     | 'i16'    | 'i32'    | 'i64'    |
    'u8'     | 'u16'    | 'u32'    | 'u64'    |
    'bool'   | 'ptr'    | 'true'   | 'false'  |
    'exit'   | 'import' | 'from'   | 'export' |
    'const'  | 'sizeof' | 'return' | 'set'    |
    'attr'   | 'as'     | 'is'     | 'all'    |
    'type'   | 'void'.

ponctuation :=
    ','  | ':'  | '('  | ')'  | '['  | ']' |
    '='  | '==' | '!=' | '>'  | '>=' | '<' |
    '<=' | '+'  | '-'  | '*'  | '/'  | '%' |
    '-=' | '+=' | '*=' | '/=' | '%=' | '.' |
    '@'  | '::' | '~'  | '&&' | '||' | '!' |
    '|^' | '>>' | '<<' | '->' | '^'.

basicType :=
    'i8' | 'i16' | 'i32' | 'i64' | 'ptr' | 'bool' |
    'u8' | 'u16' | 'u32' | 'u64' | 'void'.



Module := {Coupling} {AttSymbol}.

Coupling := Import | FromImport | Export.

Import := 'import' Items.
FromImport := 'from' id 'import' Items.
Export := 'export' Items.
Items := (AliasList | 'all')
AliasList := Alias {',' Alias} [','].
Alias := id ['as' id].

AttSymbol := [Attributes] Symbol.
Attributes:= 'attr' IdList.
IdList := id {',' id} [','].

Symbol := Procedure
    | Data
    | Const
    | TypeDef.

Const := 'const' (SingleConst|MultipleConst).
SingleConst := id '=' Expr.
MultipleConst := 'begin' {SingleConst ';'} 'end'.

Data := 'data' (SingleData|MultipleData).
MultipleData := 'begin' {SingleData ';'} 'end'.
SingleData :=  id [Annot] (DExpr|string|Blob).
Blob := '{' ExprList '}'.
DExpr := '[' [Expr] ']'.

TypeDef := 'type' (SingleType|MultipleType).
SingleType := id ('as'|'is') Type.
MultipleType := 'begin' {SingleType ';'} 'end'.

Procedure := 'proc' id [Annotatted|Direct] [Vars] Block.

Annotatted := Annot [AArgs].
AArgs := '[' [idList] ']'.

Direct := DArgs [Rets].
DArgs := '[' [DeclList] ']'.
Vars := 'var' DeclList.

Rets := TypeList.
TypeList := Type {',' Type} [','].

DeclList := Decl {',' Decl} [','].
Decl := IdList Annot.
Annot := ':' Type.

Type := (basicType | ProcType | Name) ['^' Layout].
ProcType := 'proc' ProcTTList ProcTTList.
ProcTTList := '[' [TypeList] ']'.

Layout := Type | Struct.
Struct := 'begin' [Size] {Field ';'} 'end'.
Size := '[' Expr ']'.
Field := IdList Annot [Offset].
Offset := '{' Expr '}'.

Block := 'begin' {Statement} 'end'.

Statement := If [';']
      | While [';']
      | Return ';'
      | Set ';'
      | Exit ';'
      | Expr ';'.

While := 'while' Expr Block.

If   := 'if' Expr Block {ElseIf} [Else].
ElseIf := 'elseif' Expr Block.
Else := 'else' Block.

Set := 'set' ExprList assignOp Expr.
assignOp := '=' | '-=' | '+=' | '/=' | '*=' | '%='.

Return := 'return' [ExprList].
Exit := 'exit' ['?'] Expr.

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

Prefix := 'not' | '~' | '!' | '@'.
Suffix := Conversion
    | Deref
    | Call
    | DotAccess
    | ArrowAccess.
Conversion := Annot.
Call := '[' [ExprList] ']'.
Deref := '@' Type.
DotAccess := '.' (id|Call).
ArrowAccess := '->' (id|Call).

Factor := Name
    | Literal
    | NestedExpr
    | Sizeof.
NestedExpr := '(' Expr ')'.
Literal := true | false | number | char.
Name := id ['::' id].
Sizeof := 'sizeof' ['^'] '[' Type {DotAccess} ']'.
```

#### NOTE 1

When we do `data D:T`, `T` must be a pointer, possibly with metadata.
What we do then, is have something like:

```
data D:ptr^i64 []
```

Which allows us to access it with `D->[0]` or
`@D`. Or declare something like:

```
type T as ptr^begin X:i64; Y:i64; end
data Origin:T []
```

Which allows us to access it like `Origin->X` and `Origin->Y`.
Leaving `[]` without expression makes the compiler initialize it to
`sizeof^[T]`.

#### NOTE 2

There are a bunch of ways to declare space for a struct,
considering the struct:

```
type POINT is ptr ^ begin
    X:i64; Y:i64;
end
```

We can have these `data` declarations:

```
data P [ sizeof[POINT] ]
data Q:POINT []
data R:POINT {
    1, 1,
    2, 2,
    3, 3,
}
data S:POINT [50]
```

The types are as follows:
 - `P` has type `ptr`
 - `Q`, `R` and `S` has type `POINT` (which is also a pointer)

It's important that `R` is a `POINT`, since this allows the
syntax `R.[i]` as described in the following sections.
Further improvements should address
syntax sugar for array-like structures.

These `data` declarations can be acessed like so:

```
proc main
var p:ptr, r:ptr
begin
    set p = P:POINT;
    set p->X = 1;
    set p->Y = 1;

    set Q->X = 1;
    set Q->Y = 1;

    if R->X != Q->X or R->Y != Q->Y begin
        exit 1ss;
    end

    if p->X != Q->X or p->Y != Q->Y begin
        exit 2ss;
    end
end
```

We can cast `P` to `POINT` (`P:POINT`) since structures are just
pointer related syntax-sugar.

#### NOTE 3

It is entirely possible to refer to previous struct fields when
declaring field offsets, and it is also possible to omit the
struct name.

```
type UNPADDED is ptr ^ begin
    X:i32 { 0 };
    Y:i64 { UNPADDED.X + sizeof[i32] };
    Z:i64 { Y + sizeof[i64] }; # UNPADDED may be omitted
end
```

#### NOTE 4

A struct can be used to type a blob if,
and only if it is *well-behaved*.
If there is padding, the compiler should
pad it with zeroes.

```
type Row is ptr ^ begin
    X:i64;
    Y:i32;
    Proc:proc[ptr][];
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

#### NOTE 5

```
type T is ptr ^ begin
    tag:i8        {0};
    contents:void {1};
end
```

`T.contents` is untyped, given `a:T` then
`a->contents` is not allowed, but allows `(a.contents):U`,
whatever struct `U` may be.

#### NOTE 6

`sizeof^` can be used in a struct if it is *well-behaved*
or if struct size is set explicitly.

#### NOTE 7

It's entirely possible to do this:

```
type POS is ptr ^ begin [ sizeof[i64] ]
    Previous:i64 { ~sizeof[i64] };
    Current:i64 { 0 };
    Next:i64 { sizeof[i64] };
end
```

And then, we can iterate an array,
which gives us `array.[i]->Previous`,
`array.[i]->Current` and `array.[i]->Next`
to look at any given moment.

#### NOTE 8

The size of the data declaration should be based of the underlying type,
for example:

```
data Ints:ptr^i64 [64]
```

Should allocate space for 64 `i64` to fit. Then `sizeof[Ints]`
should yield the size in bytes, while `sizeof^[Ints]` should
yield 64, ie, `sizeof^[Ints] == sizeof[Ints]/sizeof[i64]`.

#### WELL-BEHAVED STRUCTS

We call a struct **well-behaved** if, and only if
the struct contains no overlapping fields, no untyped fields,
no negative offsets and the fields fit entirely whithin struct
size.

All C structures without inner use of unions are well-behaved,
but not all well-behaved structures are C structures. This is
because the user is allowed to choose arbitrary padding.

Attributes may be used to make the compiler pad fields to specified
needs, maybe, for example `attr pad_c` can pad
things to the C specification.

#### ALIASES

Aliases work as you expect, they work as declarations.

```
import M as N
```

Here, the module `M` is imported, but the name `M` is shadowed
in favor of the name `N`.

```
from M import p as q, r as s
```

Something similar happens here, the module `M` is imported
and the items `p` and `r` are brought into global scope,
but their names are shadowed in favour of `q` and `s`,
respectively.

```
export P as Q, R as S
```

Finally, in the `export` clause, we have `P` and `R` being exposed
externally, but they must be imported as `Q` and `S`, respectively.

#### TYPE IDENTITY

Type identity is given by name and structure:
 - two named types in the same module are identical if they have equal names
 - two named types in different modules are always different
 - two unnamed types are identical if they are structurally identical

Structural identity is given by the following rules:
 - Two procedure types are identical if they have the same argument types,
layed out in the same order, and if they have the same return types, in order.
 - Two struct types are identical if they have the same size, same fields,
each field with identical types, in order and with equal offsets from the base pointer.

Basic types are considered named types, of course.
Consider the following types:

```
type begin
    A is i64;
    B is i64;
    C as i64;
end
```

Here `A` and `B` are different, while `C` is identical to `i64`, that is,
`C` is an alias

#### OPERATIONS ON PTRS W/ METADATA

Given the following struct:

```
type POINT is ptr ^ begin
    X:i64;
    Y:i64;
end
```

We can access a field in the following ways, all of which are
equivalent:

```
proc zero_x[p:POINT]
begin
    set (p+POINT.X)@i64 = 0;
    set p.X@i64 = 0;
    set @p.X = 0;
    set p->X = 0;
end
```

This means the following:
 - `POINT.X` is a global constant of type `i32`
 - `p.X` calculates the offset `p+POINT.X` and casts it to `ptr^i64`.
 - `p->X` calculates the offset and uses the type information to make a deref

As another syntax sugar, given any struct `A` defined as `ptr^i64`,
and a value `a:A`
then it's possible to write `a.[i]` as syntax sugar for
`a+(i*sizeof^[A])`, such that `a.[0] == a` and `a.[1] == a + sizeof^[A]`, etc.
Analogous to fields, the syntax `a->[i]` should be syntax sugar
for `(a + i*sizeof^[A])@i64`.

If a struct `B` is defined as:

```
type B is ptr ^ begin
    X:i64;
    Y:i64;
end
```

Then given a variable `b:B`, the expression `b.[i]` computes
`b+(i*sizeof[B])` while `b->[i]` is forbidden, since
computing `(b+(i*sizeof[B]))@B` makes no sense. What
`B` is saying about the pointer `b` is: "at offsets X and Y
from a multiple of b, we can find i64 fields", ie,
that if there is more than one structure at the pointer `b`,
it is inlined.

This means that, given a data declaration `D:A` in blob form,
we can write `D.[i]` to access each item declared, with ease.

Of course, it's also possible to combine this with field syntax:

```
set a.[0]->f1.[5]->a->b = b.[5]->f5.[7]->b->a;
```

Much to the detriment of readability :).

Given a string representation like:

```
type str is ptr ^ begin
    Cap:i32;
    Size:i32;
    Buff:ptr^i8;
end
```

And a variable `s:str`, the following lines show
the behaviour of a few expressions:

```
s.Size == s+str.Size
s->Size == (s+str.Size)@i64
s->Buff == (s+str.Buff)@ptr^i8
s->Buff.[i] == ((s+str.Buff)@ptr^i8 + i*sizeof^[ptr^i8])
s->Buff->[i] == ((s+str.Buff)@ptr^i8 + i*sizeof^[ptr^i8])@i8
@s->Buff.[i] == s->Buff->[i]   # suffix precedes prefix
```

Note that `sizeof^[ptr^i8] == sizeof[i8]`

#### SIZES OF TYPES

There are two ways to find the size of things:
 - `sizeof[y]`: gives the size of a type
 - `sizeof^[y]`: gives the size of the structure

Given the following types:

```
type A is ptr^begin
    X:i64;
    INNER:B;
end
type B is ptr^begin
    X:i64; Y:i64; Z:i64;
end
```

We have `sizeof[A] == sizeof[B] == sizeof[ptr]` but
`sizeof^[A] == (sizeof[i64] + sizeof[B])` while
`sizeof^[B] == 3 * sizeof[i64]`. Note that
`sizeof[A.INNER] == sizeof[B]` and not `sizeof^[B]`.

Given the following data declarations:

```
data D1 [ sizeof[A] ]
data D2 [ sizeof[ptr] ]
data D3:A
```

We have that `sizeof[D1] == sizeof[D2]`, while `sizeof[D3] == sizeof^[A]`.

This allows us to reserve data in two ways:

```
data M1 [5 * sizeof[A]]
data M2 [5 * sizeof^[A]]
```

In this case, `sizeof[M1] != sizeof[M2]`, and data inside `M2` would
need to be inlined. Procedures for accessing M1 and M2 would differ:

```
proc access_M1[i:i64] A
begin
    return (M1 + i*sizeof[A])@A;
end

proc access_M2[i:i64] A
begin
    return M2 + i*sizeof^[A];
end
```

The first one has to find the `ptr` pointing to an `A`, and
as such, needs a dereference,
while the second one just finds the index of the item `A`,
with a cast.
Or, using the syntax sugar, we can annotate
`M1:ptr^A` and `M2:A`, which allows you to access
each as `M1->[i]` and `M2.[i]`, which are analogous
to the previous procedures.

We should allow `sizeof[POINT.X]` to give the size of the underlying
type of a field, such that the following becomes the canonical way
the compiler computes the offsets:

```
type POINT is ptr ^ begin
    X:i64 {0};
    Y:i64 {X + sizeof[POINT.X]};
    Z:i64 {Y + sizeof[Y]};
end
```

We should also allow `sizeof[Y]` like above if we're inside a struct.

#### TODO

Can we make the syntax of annotations in structs be more 
coherent with the rest of the language?
