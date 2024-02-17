# Millipascal

Millipascal is basically a thin wrap over assembly that evolved to
be used as an intermediate language.

```
memory hello "Hello, World!\n"

proc main
begin
	write[hello, hello.size]
end proc
```

It has 10 basic types:

```
i8    i16    i32    i64
u8    u16    u32    u64
bool  ptr
```

And the `proc` complex types, a few examples:

```
proc[i8]bool    proc[ptr, i64][]    proc[][]
```

`ptr` is an untyped pointer, similar to `void *`

Procedures are first-class and can have multiple arguments and multiple returns:

```
memory buff "am i uppercase yet?\n"

proc main
begin
	byte_map[buff, buff.size, upper_case]
	write[buff, buff.size]
end proc

proc byte_map[b:ptr, bsize:i64, op:proc[i8]i8]
var i:ptr
begin
	set i = 0p
	while i < bsize:ptr
	begin
		set (b+i)@i8 = op[(b+i)@i8]
		set i += 1p
	end while
end proc

proc upper_case[a:i8] i8
begin
	if a >= 'a' and a <= 'z'
	begin
		return a - 32ss
	end if
	return a
end proc
```

Globals can be declared in any order.

Procedures use square brackets `[]` instead of parenthesis `()`, so that
it doesn't share parenthesis in the middle of expressions, being easier to read.

Control flow is `if`, `while`, `return` and `exit`.

If you notice, omiting the type of an argument makes it default to `i64`.
The compiler allocates on the stack every type as 8 byte chunks
anyway, so it doesn't matter.

To change the value of a variable, you use `set`.
You can declare static memory with `memory`:

```
memory w 32

proc main
begin
	set (w + 0p)@i8 = 'H'
	set (w + 1p)@i8 = 'e'
	set (w + 2p)@i8 = 'y'
	set (w + 3p)@i8 = '\n'

	write[w, 4]
end proc
```

This should print `Hey\n` to the console.

There are three built-in procedures:
`write[str:ptr, size:i64]`, `read[buffer:ptr, amount:i64] i64` and
`error[str:ptr, size:i64]`.
They will `write` to `STDOUT`, `read` from `STDIN` and
write to `STDERR`, respectivelly.

Pointer indirection is with the `@` operator, the right side is the type
expected at that location. `p@i64` reads 8 bytes from the pointer `p` as an
`i64`. For amd64 any `proc` type has 8 bytes of size.

Variables are declared together with the procedure, and cannot be
declared inside any inner block.

```
proc my_proc[]
var this, is, the, only, place, you, can, declare, variables
begin
	exit 0ss;
end proc
```

This comes from old TurboPascal implementations, it just makes it easier
to find names in the frontend, and makes lifetimes easier in the IR,
but not by much.

The language also has a very simple module system, each file is a module
and all modules must live in the same folder. The name of the module is the
name of the file. Check the `test_suite/runtime` folder.

# Style

Although the language is deceptively simple, it's good to keep a 
consistent style. If something is not listed here, just try to follow
the style of `./test_suite/runtime/bigint.mp`.

## Identifiers

Constants and string memory should be in ALL_CAPS_SNAKE_CASE:

```
const OBJ_SIZE_OFFSET 8p
const OBJ_TAG_OFFSET 6p
const OBJ_BITMAP_OFFSET 4p

memory ERR_NAT_OVERFLOW "number is too big (max 72 digits)\n"
memory ERR_NAT_NEGATIVE "number subtraction went negative\n"
memory ERR_DIVISION_BY_ZERO "division by zero\n"
```

Procedures should be in lower_snake_case:

```
proc test_guess[
    natIDD:ptr,
    scratch:ptr,
    natB:ptr,
    guess:i32
] i64
begin
	...
end proc

proc slow_div[
		natA:ptr,
		natB:ptr,
		natQ:ptr,
		natRem:ptr
]
begin
	...
end proc
```

Reserved memory should follow UpperCammelCase,
while local variables and arguments should follow lowerCamelCase:

```
memory NatIDD 40
memory Scratch 40

proc div[natA:ptr, natB:ptr, natQ:ptr, natRem:ptr]
var sizeA, sizeB, i, j,
    low:i32, high:i32, guess:i32,
    res
begin
end proc
```

## Block delimiters

Regarding `begin`/`end` keywords, they should
always come in the same line as `if` and `while` keywords, unless
the conditions are broken into multiple lines. Eg:

```
if res == HIGH begin
    set high = guess
end elseif res == LOW begin
    set low = guess
end else begin 
    exit 1ss
end if
```

If the conditions are in multiple lines, it's preferable to
keep `begin` in another line:

```
if (guess*y == x) or
   (guess*y < x and (guess+1)*y > x)
begin
    return EQ
end if
```

However, never keep `begin` in the same line as `end`.

# TODO

 - [ ] Allow main to have the signature: `proc[argc:i64, argv:ptr] int`
 - [ ] Allow arguments and variables be marked to signal that it's pointing to an object
 - [ ] Have a way to access the frame pointer
 - [ ] Inject function information on stack frame
 - [ ] Have a built-in to open files
