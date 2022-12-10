# Millipascal

Small procedural language so i can learn code generation.
Don't use it, if you do, be prepared for it to blow up in your face.
In fact, it's not even working right now :D

It has only 6 basic types:

```
i8	i16 	i32	i64	bool	ptr
```

`ptr` is an untyped pointer, similar to `void *`

Procedures are not first-class
(for no good reason, maybe i'll fix this in the future),
but can have multiple arguments and multiple returns:

```
proc Split[p:ptr, size] ptr, ptr
begin
	return ptr, (size/2):ptr + p
end proc
```

Procedures use square brackets `[]` instead of parenthesis `()`, so that
there's little need for semicolons, except in code like:

```
proc main
begin
	1
	-2
end proc
```

That will be parsed as `1 - 2` instead of `1; -2;`.
But i don't know why would you write code like that in the first place.
In that case, you can use semicolons.

Control flow is only `if`, `while` and `return`.

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

It's incredibly verbose so that it will never be used by anyone.
If you notice, omiting the type of an argument makes it default to `i64`.
The compiler allocates on the stack every type as 8 byte chunks
anyway so fuck it.

To change the value of a variable, you use `set`, similar to Lisp.
You can't use `+=`, `-=` etc, again, for no good reason.

You can declare static memory with `memory`:

```
memory write_buffer 32

proc main
var w:ptr
begin
	set w = write_buffer
	set w@i8 = 72:i8
	set w = w + 1
	set w@i8 = 69:i8
	set w = w + 1
	set w@i8 = 89:i8
	set w = w + 1
	set w@i8 = 10:i8

	write[write_buffer, 4]
end proc
```

If anything worked at all, this would print `Hey\n` to the console.
There are no string literals for now, i will eventually fix this issues.

There are [will be] three built-in procedures:
`write[str:ptr, size:int]`, `read[buffer:ptr, amount:int]` and
`error[str:ptr, size:int]`.
They will `write` to `STDOUT`, `read` from `STDIN` and
write to `STDERR`, respectivelly.
The `exit` syscall will be implemented as an language
feature, so that the inner intermediate representation
can be aware of termination.

Pointer indirection is with the `@` operator, the right side is the type
expected at that location. `p@i64` reads 8 bytes from the pointer `p` as an
`i64`.

Variables are declared together with the procedure, and cannot be
declared inside any block.

```
proc MyProc[] i64
var this, is, the, only, place, you, can, declare, variables
begin
	return only + you + can + declare + variables;
end proc
```

This comes from old TurboPascal implementations, it just makes it easier
to find names in the frontend, but not by much.
I won't change this, however, since this is how the stack frame
works, this language is just a thin wrap over assembly.

Missing features that will be added:

 - String literals: `memory hello "Hello, world!\n"`
 - `.size` property for `memory` declarations
 - `exit` statement: `exit 0`
 - `write`, `read` and `error` built-in procedures
 - op + assign operators: `+=`, `-=`, `*=`, `/=` and `%=`
 - hex and binary literals: `0xFFFFFF` and `0b10011001`
 
Improvements:
 - Properly deal with dirty values in the register allocator
 - Generate minimum amount of copies in the frontend

Structs and unions will never be added because that will need a complete
refactor of the IR, currently, it treats stack frame slots as isomorphic
8 byte cells, which doesn't work well with those compound types.
