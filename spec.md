# Millipascal

## Lexical Elements

Millipascal source is UTF8 encoded, but only comments
can carry arbitrary codepoints, source code is limited
to ASCII. Whitespace serves only as token separators,
and otherwise have no semantic meaning.

### Identifiers

Identifiers are similar to C, if not identical.
They're simply the regex `[a-zA-Z_][a-zA-Z0-9_]*`.

```ebnf
id = letterPlus {letterPlus | digit}.
letterPlus = letter | '_'.
letter = 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|
         'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|
         'u'|'v'|'w'|'x'|'y'|'z'|'A'|'B'|'C'|'D'|
         'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|
         'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|
         'Y'|'Z'.
digits = '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'.
```

### Keywords

Millipascal is a very keywordy language, most of them are used
only once. In general, i find keywords easier to read than
arbitrary symbols.
Make sure the following identifiers are reserved:

```
var   proc   begin  end    while if     else   elseif
or    and    not    data   i8    i16    i32    i64   
u8    u16    u32    u64    bool  ptr    true   false 
exit  import from   export const sizeof return set   
attr  as     all    struct void  asm
```

### Operators and ponctuation

Even though there are quite a few keywords, because
there's almost no operator overloading, there are
quite a few symbols too.

```
,   :   (   )   [   ]   {   }
=   ==  !=  >   >=  <   <=  +
-   *   /   %   -=  +=  *=  /=
%=  .   @   ::  ~   &   |   !
^   >>  <<  ->  ?   '   "   <>
++  --
```

As usual, operators listed here are considered a single token.

### Numerical Literals

```ebnf
number = decimal | hexadecimal | binary.
decimal = digits {decDigits} [numEnding].
hexadecimal = '0x' hexDigits {hexDigits} [numEnding].
binary = '0b' binDigits {binDigits} [numEnding].
decDigits = digits | '_'.
hexDigits = digits | 'A'|'B'|'C'|'D'|'E'|'F'|'a'|'b'|'c'|'d'|'e'|'f'|'_'.
binDigits = '0'|'1'|'_'.
numEnding = 'p'|'s'|'ss'|'l'|'u'|'us'|'uss'|'ul'.
```

Literals can be written in three bases: decimal, hexadecimal and binary.
Octal is not provided. Each number can end with a postfix that specifies
a type. Underscore is ignored, and can be used to group digits.
Literal type is defined like the following:

 - `p`: `ptr`
 - `ss`: `i8` (shorter short)
 - `s`: `i16` (short)
 - no suffix: `i32`
 - `l`: `i64` (long)
 - `uss`: `u8` (unsigned shorter short)
 - `us`: `u16` (unsigned short)
 - `u`: `u32` (unsigned)
 - `ul`: `u64` (unsigned long)

Notice that this has not much to do with how C does integers,
that's because i don't care.

A number without postfix defaults to `i32`.

### Comments

Comments start with `#` and end with a newline. These are not
specified in the full grammar, and are considered whitespace.
Parsers can completely ignore them.

### Strings and chars

```ebnf
escapes = '\\"' | '\\'' | '\\n' | '\\t' | '\\r'.
string = '"' {ascii|escapes} '"'.
char = '\'' (ascii|escapes) '\''.
```

Strings and characters are ascii, no fuzz here.
If you need to encode other UTF8 codepoints use blobs and
number literals. These two require a little smartness
in the lexer, because of escapes and arbitrary ascii
values, but can be dealt easily.

## Gramatical Elements

### Module

```ebnf
Module = {Coupling} {AttSymbol}.
```

Each file in Millipascal is a module. Modules can be imported,
but must be done at the top of each file. The grammar
separates module information from symbol information,
and makes sure the source is properly organized.

### Coupling

```ebnf
Coupling = Import | FromImport | Export.

Import = 'import' Items.
FromImport = 'from' id 'import' Items.
Export = 'export' Items.
Items = (AliasList | 'all').
AliasList = Alias {',' Alias} [','].
Alias = id ['as' id].
```

Aliases work as you expect, similar to declarations.

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

Note that modules are imported by name, and names must be identifiers,
so file names must obey identifier restrictions, otherwise it is
not possible to import it.

### Symbols and Attributes

```ebnf
AttSymbol = [Attributes] Symbol [';'].
Attributes = 'attr' IdList.
IdList = id {',' id} [','].

Symbol = Procedure
    | Data
    | Const
    | Struct.
```

Symbols may be preceded by attributes, these are simply
identifiers that are recognized by the compiler, and can
be used to perform conditional compilation for different targets,
setting padding, etc.

### Data

```ebnf
Data = 'data' (SingleData|MultipleData).
MultipleData = 'begin' {SingleData ';'} 'end'.
SingleData =  id [Annot] (DExpr|string|Blob).
Blob = '{' ExprList '}'.
DExpr = '[' [Expr] ']'.
```

Data defines static memory, available without interference from the operating
system, that is, with no interference besides program loading, i guess. It can
be reserved or declared, reserved takes an expression inside square brackets
that define the amount to be reserved, and depends on the underlying type
of the data definition (`data a:S [52]` will allocate `52*sizeof[S]` bytes).
While declared can be of two other subkinds: blob and string.

String data declarations encode a string into memory, computing their
size. If `data M "abcdefgh"` is a declaration, then `sizeof[M]`
gives the size of `M` in bytes.

Blob data declarations encode arbitrary numbers and symbols into
a section of memory, and may be optionally annotatted with structs
so that the compiler may check whether the types are placed correctly,
this is optional, however, you must know what you're doing.

### Const

```ebnf
Const = 'const' (SingleConst|MultipleConst).
SingleConst = id '=' Expr.
MultipleConst = 'begin' {SingleConst ';'} 'end'.
```

Constants works basically as `define` in C, but they are evaluated
at compile time with arbitrary precision arithmetic, when casting
the value to the actual type, values should saturate instead of
overflowing, that is: if `300` is assigned to an `i8`, the value
is set to the largest `i8` value, ie, `127`, instead of overflowing.

### Structs

```ebnf
Struct = 'struct' id [Size] 'begin' {Field ';'} 'end'.
Size = '[' Expr ']'.
Field = IdList Annot [Offset].
Offset = '{' Expr '}'.
```

Structs create new nominal types that represent the structure of an
object at a pointer. This is very important: structs are not first
class, they are only additional information for pointers,
and are not "value based", like in C. This means you can
readily cast a `ptr` to any struct, and any struct to a `ptr`,
and even any struct to any other struct.

A struct can either be explicitly or implicitly set.
When explicitly set, the size of the whole struct
and the offsets of each field must be specified by the programmer
using constant expressions.
When implicitly set, the size and offsets must be left out,
and the compiler computes those for you, by default, all structs
are packed, but it should be possible to add attributes
to change the padding requirements.

Padding attributes that should be reserved are: `c_pad` and
`align_pack`. The first computes padding just like a C struct,
with proper internal and trailing padding, the second one allows
the compiler to reorder fields for proper structure packing,
without losing alignment guarantees.

Inner fields of structs are not inlined, in the following
struct, the field `List.Next` is only a pointer,
**structs are not first class objects**.

```
struct List begin
    X, Y:i64;
    Next:List;
end
```

Structs behave just like constants in a sense, they specify
offset tables and object size, and these must be verified for circularity.
The size of a struct is a constant in itself, just like each field,
if the field offset refers to the struct size in
any way, shape or form, that should be considered a cycle, and
should be reported. The following struct is misbehaving in this
regard:

```
const size = A.X + 8;
struct A [size] begin
    X:i64 {size+1};
end
```

Given a structure `B`, then `sizeof[B]` yields the size of the struct
in bytes, even if it is explicit or implicit, all structs have size,
but keep in mind that structs are sugar for `ptr`s, and fields or arrays
may have these structs either inlined or through a layer of indirection,
as such, the size might be `sizeof[B]` or `sizeof[ptr]`. Good practice
is to do the following:

```
struct refB begin
    B:B;
end

data M1:B [8]
data M2:refB [8]
```

In the above example, `sizeof[M1]` is `8 * sizeof[B]` while
`sizeof[M2]` is `8 * sizeof[ptr]` (`sizeof[ptr] == sizeof[refB]`),
that is, the second buffer has only pointers to these `B` objects.

If multiple field names precede a type `a, b, c:i64`, then all those fields
are of that type, a single offset can't be applied to all of them at the same time,
`a, b, c:i64 {0}` should result in an error.

### Procedure

```ebnf
Procedure = 'proc' id Signature [Vars] (Asm|Block).

Signature = DArgs [Rets].
DArgs = '[' [DeclList] ']'.
Vars = 'var' DeclList.

Rets = TypeList.
TypeList = Type {',' Type} [','].

DeclList = Decl {',' Decl} [','].
Decl = IdList Annot.
Annot = ':' Type.
```

Procedures can have multiple returns, obviously. Variables
are declared separatedly, together with the procedure, which
means the language is only losely lexical scoped, there are two scopes:
global and local. Nothing else.

It is possible to write `a,b,c:i64` in a declaration,
which means all preceding variables have that type.

The body of the procedure can be of two types, one is
in a high level language, with a handful of features,
while the other is a low level language, architecture specific,
that can encode specific instructions of the target machine.

### Blocks

```
Block = 'begin' {Statement} 'end'.

Statement = If [';']
      | While [';']
      | DoWhile [';']
      | Return ';'
      | Set ';'
      | Exit ';'
      | Expr ';'.
```

Blocks are delimited sequences of statements, most of which
are terminated with semicolons. Millipascal does not have
naked blocks, nested arbitrarely, that is not useful, as
there are only two scopes (local and global) and blocks
can't create new ones.

All of these statements, except expressions, begin with a keyword.

### If Statements

```
If = 'if' Expr Block {ElseIf} [Else].
ElseIf = 'elseif' Expr Block.
Else = 'else' Block.
```

If statements are the only form of selective control flow.
They accept an expression of type `bool`, if it is `true`,
they execute the immediate block, if not, they descend into
the next one, if no blocks are entered, the `else` block,
if present, is executed.

### While and Do-While Statements

```
While = 'while' Expr Block.
DoWhile = 'do' Block 'while' Expr.
```

There are only two loop constructs, only one would suffice,
but the other is included for simmetry.

While loops execute the condition expression first, if it is true, the
block is executed and returns back to check the condition, if it is false,
it continues to the next statement.

Do-while loops execute the block first, then check the condition,
looping back if true, continuing if false.

### Return and Exit Statements

```
Return = 'return' [ExprList].
Exit = 'exit' ['?'] [Expr].
```

There are two ways to exit early from a procedure, returning
from the procedure or terminating the program. These are the
only two ways to introduce exit points.

You may return more than a single value from any procedure,
as such, the return statement can take multiple expressions,
or none, for that matter.

The expression in an exit statement is the exit code of the program,
this may be ignored by the operating system, but on linux, it is useful
for debugging.

Exit statements may optionally include a interrogation mark,
by writting `exit?;`, the program should unwind the stack
trace and print it before terminating, this is used mostly
for debugging, and should be turned off in "release" builds.

### Set Statements

```
Set = 'set' ExprList (Assign|IncDec).
IncDec = '++' | '--'.
Assign = assignOp Expr.
assignOp = '=' | '-=' | '+=' | '/=' | '*=' | '%=' | '<>'.
```

Set statements are quite complex and complexity must be justified.

In it's simplest form, `set a++;` or `set a--;` simply increment
or decrement a numerical or pointer value. In special, if `a` is a
struct type, it is incremented of decremented by `sizeof[STRUCT]`,
that is, in this case, `set a++` would be the same as `set a += sizeof[STRUCT]`,
while for numbers and raw pointers, it is the same as `set a += 1;`.
These exist to avoid doing pointer arithmetic with structs,
ie, `set a += 1;` would slide the struct representation one byte to the
left, this is can lead to careless mistakes.

If instead an assignment operator is being used, we speak of the
values at the left hand side of the operator as LHS, and 
the values at the right hand side as RHS. RHS always evaluates
before the LHS, and if multiple expressions are in the LHS,
they are evaluated from left to right.

The expressions in the LHS must be *assignable* in all cases,
the RHS must be *assignable* only in swap (`<>`) operations.

We define an **assignable** expression simply by the
top-level expression type. If the expression is a simple identifier,
then it must be a local (be it argument or variable) to be assignable,
if, however, the expression is a dereference, it is always assignable.
This includes struct field dereference operations (`a->b`),
so that `set a->b = 1;` is valid.

Note that `set a[i] = j;` is invalid, as `a[i]` is a computed offset,
if you're working with arrays, you must write `set a[i]->field = j;`,
where `field` is the one (and maybe only) field you want to modify.

The LHS is allowed to contain multiple expressions if, and only if
the assignment operator is `=` and the RHS contains an expression
that is multi-valued, ie, a procedure with multiple returns. If
these conditions are not met, the compiler should yield an error.
We allow this so that procedures can return multiple values freely.
In case multiple expressions are allowed, types should be checked
in order, from left to right, so that each type on the LHS
corresponts to the same type from the multi-valued expression.

Arithmetical operators ('-=', '+=', '/=', '*=' and '%=')
are included for the sake of brevity, and because they fit
marvelously with two and three-address instructions.
They work the same as `set LHS = LHS <op> RHS;`,
but the LHS is evaluated only once.

Last, but not least, is the swap operator `<>`, which is
included for brevity. Swapping two values
is a common operation, and should have built-in support.
In this case both the LHS and RHS must be *assignable*,
and RHS is still evaluated before the LHS.

### expressions

Expressions can be statements, or can appear in other statements,
it's allowed to be a statement so that we can evaluate procedures
that return no values, or those who values are being
ignored.

```
ExprList = Expr {',' Expr} [','].
Expr = And {'or' And}.
And = Comp {'and' Comp}.
Comp = Sum {compOp Sum}.
compOp = '==' | '!=' | '>' | '>=' | '<' | '<='.
Sum = Mult {sumOp Mult}.
sumOp = '+' | '-' | '|' | '^'.
Mult = UnaryPrefix {multOp UnaryPrefix}.
multOp = '*' | '/' | '%' | '&' | '<<' | '>>'.
UnaryPrefix = {Prefix} UnarySuffix.
UnarySuffix = Factor {Suffix}.

Prefix = 'not' | '~' | '!'.
Suffix = Conversion
    | Deref
    | Call
    | DotAccess
    | ArrowAccess.

Conversion = Annot.
Call = '[' [ExprList] ']'.
Deref = '@' Type.
DotAccess = '.' id.
ArrowAccess = '->' id.
```

Expression syntax definition is ugly because we encode the precedence
directly in the grammar, this allows us to parse it without ambiguity,
and doesn't require any additional constructs beside standard recursive
descent.

Precedence can be viewed separatedly in a table:

| Precedence | Operators                                    |
|:----------:|:--------------------------------------------:|
|     0      | `or`                                         |
|     1      | `and`                                        |
|     2      | `==`, `!=`, `>`, `>=`, `<`, `<=`,            |
|     3      | `+`, `-`, `|`, '^'                           |
|     4      | `*`, `/`, `%`, `&`, `<<`, `>>`               |
|     5      | (suffix) `:`, `[]`, `.`, `->`, `@`           |
|     6      | (prefix) `not`, `~`, `!`                     |

Operations are described in the following table:

| Operation  | Description                             |
|:----------:|:---------------------------------------:|
|    `or`    | Logical disjunction                     |
|    `and`   | Logical conjunction                     |
|    `not`   | Logical negation                        |
|    `==`    | Equals                                  |
|    `!=`    | Unequals                                |
|    `>`     | Greater than                            |
|    `>=`    | Greater or equals                       |
|    `<`     | Less                                    |
|    `<=`    | Less or equals                          |
|    `~`     | Arithmetic Negation                     |
|    `+`     | Addition                                |
|    `-`     | Subtraction                             |
|    `*`     | Multiplication                          |
|    `/`     | Truncated Division                      |
|    `%`     | Truncated Remainder                     |
|    `|`     | Bitwise OR                              |
|    `&`     | Bitwise AND                             |
|    `!`     | Bitwise NOT                             |
|    `^`     | Bitwise XOR                             |
|    `<<`    | Bitwise Left Shift                      |
|    `>>`    | Bitwise Right Shift                     |
|    `:`     | Type Annotation or Type Cast            |
|    `[]`    | Procedure call or struct indexing       |
|    `.`     | struct field offset (dot access)        |
|    `->`    | struct field derefernce (arrow access)  |
|    `@`     | dereference                             |

The following caveats are important:
 - If `a` is a pointer, then `a+b` and `a-b` are allowed only if `b` is numerical;
 - `or` and `and` are *not short-circuited*, and operate only on `bool` types;
 - If `a` is of a numerical type `T`, then `a+b` is allowed only if `b` is also of type `T`,
this applies to all other arithmetical operations;
 - Indexing with negative values is allowed, `a[i]` computes `a + i*sizeof[STRUCT]`,
and should accept any integer;
 - The difference between type annotattion and cast is between a no-op and actual conversion.
Given `a:T`, if `a` is already of type `T`, nothing is done, while if `a` is of another type,
a cast is one;
 - With unsigned integers casting a big integer type to a smaller integer type is allowed,
and should truncate the value accordingly, ie, copy only the least significant bits.
 - Dereference asks for a type `a@T`, where `T` can be any type.

The following is a list of behaviours that are **implementation defined**,
each item also includes clues as to how the program may behave, but compilers
may choose to do as they please:

 - Dereferencing a pointer that does not point to a valid object. It may segfault, or just retrieve garbage;
 - Casting a non-procedure to a procedure and performing a call. It may segfault or execute arbitrary code;
 - Dereferencing a type `T` from a pointer where `U` was stored. It may retrieve garbage;
 - Casting a big signed integer to a smaller signed integer. It may truncate the bits, without preserving sign, effectively creating garbage;
 - If `b` is zero and you perform a division or remainder, `a/b` or `a%b`. It is preferred that the program crashes with a signaling error;
 - If `b` is negative and you perform a shift `a<<b` or `a>>b`. It may use only the lower bits of `b`;

Logical operators are not short-circuited to avoid branches in expressions,
all expressions in Millipascal are linearly executed.

If `a->b`, `a@T` or `a` (local) occurs in the LHS of an assignment, then the
value is not evaluated directly, but is represented as a sort of *lvalue*,
and is the target of the assigment.

Indexing, dot and arrow accesses behave like syntax sugar,
given `a:T`, with `T` being a struct with a field `b:U`:
 - `a.b` can be translated to `a+T.b`;
 - `a->b` can be translated to `(a+T.b)@U`;
 - `a[i]` can be translated to `a+i*sizeof[T]`;

### Factors

```
Factor = Name
    | Literal
    | NestedExpr
    | Sizeof.
NestedExpr = '(' Expr ')'.
Literal = true | false | number | char.
Name = id ['::' id].
Sizeof = 'sizeof' '[' Type [DotAccess] ']'.
```

Factors represent the primary elements that are manipulated in expressons.

The literals `true` and `false` represent the two possible boolean values,
and have type `bool`.
The number literals are have their respective type according to
the suffix, and are evaluated to the number they represent. 
A char literal is a particular kind of number literal that is of type `i8`,
and evaluates to the *ascii* value of that character.

A "Name" represents a global, local or external name. If it is a local,
the type of the expression is the declared type of that local, and it
evaluates to the value contained in that local.
If it is a global or external, it may refer to a data declaration, a struct,
a constant or a procedure.
 - If it refers to a data declaration the type is either `ptr` or
the type annotatted in the data declaration, it evaluetes to the address
of that declaration;
 - If it refers to a procedure, the type is the inferred type of the procedure.
The expression evaluates to the address of the procedure, allowing you to call it,
or pass it around;
 - If it refers to a struct, it may only be in the compound expression `STRUCT.FIELD`,
which has type `i32` and evaluates to the offset of that field;
 - If it refers to a constant, it will have the type inferred or annotatted in that constant,
and if will evaluate to the value of the constant. That value is expected to be evaluated
at compile time.

Of course, if the expression occurs in the LHS of an assignment, the local identifier
will be interpreted as something of a *lvalue*, and will be the target of the assignment,
instead of evaluated directly.

The "Sizeof" expression has different semantics based on the thing it is applied to.
 - `sizeof[data]`: evaluates to the *byte* size of that data declaration;
 - `sizeof[struct]`: evaluates to the *byte* size of the struct;
 - `sizeof[struct.field]`: evaluates to the *byte* size that field takes in the struct;
 - `sizeof[T]`: evaluates to the *byte* size of the type `T`;

That is, `sizeof` always computes the *byte* size.

## Full Grammar

```ebnf
id = letterPlus {letterPlus | digit}.
letterPlus = letter | '_'.
letter = 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|
         'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|
         'u'|'v'|'w'|'x'|'y'|'z'|'A'|'B'|'C'|'D'|
         'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|
         'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|
         'Y'|'Z'.
digits = '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'.

number = decimal | hexadecimal | binary.
decimal = digits {decDigits} [numEnding].
hexadecimal = '0x' hexDigits {hexDigits} [numEnding].
binary = '0b' binDigits {binDigits} [numEnding].
decDigits = digits | '_'.
hexDigits = digits | 'A'|'B'|'C'|'D'|'E'|'F'|'a'|'b'|'c'|'d'|'e'|'f'|'_'.
binDigits = '0'|'1'|'_'.
numEnding = 'p'|'s'|'ss'|'l'|'ll'|'us'|'uss'|'ul'|'ull'.

escapes = '\\"' | '\\'' | '\\n' | '\\t' | '\\r'.
string = '"' {ascii|escapes} '"'.
char = '\'' (ascii|escapes) '\''.

keywords =
    'var'    | 'proc'   | 'begin'  | 'end'    |
    'while'  | 'if'     | 'else'   | 'elseif' |
    'or'     | 'and'    | 'not'    | 'data'   |
    'i8'     | 'i16'    | 'i32'    | 'i64'    |
    'u8'     | 'u16'    | 'u32'    | 'u64'    |
    'bool'   | 'ptr'    | 'true'   | 'false'  |
    'exit'   | 'import' | 'from'   | 'export' |
    'const'  | 'sizeof' | 'return' | 'set'    |
    'attr'   | 'as'     | 'all'    | 'struct' |
    'void'   | 'asm'    | 'do'.

ponctuation =
    ','  | ':'   | '('  | ')'  | '['  | ']' |
    '='  | '=='  | '!=' | '>'  | '>=' | '<' |
    '<=' | '+'   | '-'  | '*'  | '/'  | '%' |
    '-=' | '+='  | '*=' | '/=' | '%=' | '.' |
    '@'  | '::'  | '~'  | '&'  | '|'  | '!' |
    '^'  | '>>'  | '<<' | '->' | '?'  | '\''|
    '"'  | '<>'  | '++' | '--'.

basicType =
    'i8' | 'i16' | 'i32' | 'i64' | 'ptr' | 'bool' |
    'u8' | 'u16' | 'u32' | 'u64' | 'void'.

Module = {Coupling} {AttSymbol}.

Coupling = Import | FromImport | Export.

Import = 'import' Items.
FromImport = 'from' id 'import' Items.
Export = 'export' Items.
Items = (AliasList | 'all').
AliasList = Alias {',' Alias} [','].
Alias = id ['as' id].

AttSymbol = [Attributes] Symbol [';'].
Attributes = 'attr' IdList.
IdList = id {',' id} [','].

Symbol = Procedure | Data | Const | Struct.

Const = 'const' (SingleConst|MultipleConst).
SingleConst = id [Annot] '=' Expr.
MultipleConst = 'begin' {SingleConst ';'} 'end'.

Data = 'data' (SingleData|MultipleData).
MultipleData = 'begin' {SingleData ';'} 'end'.
SingleData =  id [Annot] (DExpr|string|Blob).
Blob = '{' ExprList '}'.
DExpr = '[' [Expr] ']'.

Struct = 'struct' id [Size] 'begin' {Field ';'} 'end'.
Size = '[' Expr ']'.
Field = IdList Annot [Offset].
Offset = '{' Expr '}'.

Procedure = 'proc' id [Signature] [Vars] (Asm|Block).

Signature = DArgs [Rets].
DArgs = '[' [DeclList] ']'.
Vars = 'var' DeclList.

Rets = TypeList.
TypeList = Type {',' Type} [','].

DeclList = Decl {',' Decl} [','].
Decl = IdList Annot.
Annot = ':' Type.

Type = basicType | ProcType | Name.
ProcType = 'proc' ProcTTList ProcTTList.
ProcTTList = '[' [TypeList] ']'.

Asm = 'asm' [ClobberSet] 'begin' {AsmLine} 'end'.
ClobberSet = '[' idList ']'.
AsmLine = Label | Instruction.
Label = '.' id ':'.
Instruction = InstrName [OpList] ';'.
InstrName = id.
OpList = Op {',' Op} ','.
Op = Name | Addressing | ConstOp | Literal.
Addressing = '[' OpList ']' ['@' id].
ConstOp = '{' Expr '}'.

Block = 'begin' {Statement} 'end'.

Statement = If [';']
      | While [';']
      | DoWhile [';']
      | Return ';'
      | Set ';'
      | Exit ';'
      | Expr ';'.

While = 'while' Expr Block.
DoWhile = 'do' Block 'while' Expr.

If   = 'if' Expr Block {ElseIf} [Else].
ElseIf = 'elseif' Expr Block.
Else = 'else' Block.

Set = 'set' ExprList (Assign|IncDec).
IncDec = '++' | '--'.
Assign = assignOp Expr.
assignOp = '=' | '-=' | '+=' | '/=' | '*=' | '%=' | '<>'.

Return = 'return' [ExprList].
Exit = 'exit' ['?'] [Expr].

ExprList = Expr {',' Expr} [','].
Expr = And {'or' And}.
And = Comp {'and' Comp}.
Comp = Sum {compOp Sum}.
compOp = '==' | '!=' | '>' | '>=' | '<' | '<='.
Sum = Mult {sumOp Mult}.
sumOp = '+' | '-' | '|' | '^'.
Mult = UnaryPrefix {multOp UnaryPrefix}.
multOp = '*' | '/' | '%' | '&' | '<<' | '>>'.
UnaryPrefix = {Prefix} UnarySuffix.
UnarySuffix = Factor {Suffix}.

Prefix = 'not' | '~' | '!'.
Suffix = Conversion
    | Deref
    | Call
    | DotAccess
    | ArrowAccess.
Conversion = Annot.
Call = '[' [ExprList] ']'.
Deref = '@' Type.
DotAccess = '.' id.
ArrowAccess = '->' id.

Factor = Name
    | Literal
    | NestedExpr
    | Sizeof.
NestedExpr = '(' Expr ')'.
Literal = true | false | number | char.
Name = id ['::' id].
Sizeof = 'sizeof' '[' Type [DotAccess] ']'.
```
