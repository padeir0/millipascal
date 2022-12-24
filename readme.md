# Millipascal

Small procedural language so i can learn code generation.
Don't use it, if you do, be prepared for it to blow up in your face.

```
memory hello "Hello, World!\n"

proc main
begin
	write[hello, hello.size]
end proc
```

It has only 6 basic types:

```
i8	i16 	i32	i64	bool	ptr
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
		return a - 32r
	end if
	return a
end proc
```

Globals can be declared in any order.

Procedures use square brackets `[]` instead of parenthesis `()`, so that
it doesn't share parenthesis in the middle of expressions, being easier to read.

Control flow is `if`, `while`, `return` and `exit`.

```
proc iffi[a] i64
begin
	if a == 0 begin
		return iffi[a+1];
	end elseif a == 1 begin
		return iffi[a*5];
	end else begin
		while a < 10
		begin
			set a = a + 1;
		end while
		return a;
	end if
end proc
```

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
proc MyProc[]
var this, is, the, only, place, you, can, declare, variables
begin
	exit 0r;
end proc
```

This comes from old TurboPascal implementations, it just makes it easier
to find names in the frontend, and makes lifetimes easier in the IR,
but not by much.
I won't change this, however, since this is how the stack frame
works, this language is just a thin wrap over assembly.

Optimizations:
 - Properly deal with dirty values in the register allocator
 - Generate minimum amount of copies in the frontend
