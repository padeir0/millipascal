# Millipascal

Millipascal is a thin wrap over assembly.

```millipascal
data hello "Hello, World!\n"

proc main
begin
  write[hello, sizeof[hello]];
end
```

It has 10 basic types:

```millipascal
i8    i16    i32    i64
u8    u16    u32    u64
bool  ptr
```

And the `proc` complex types, a few examples:

```millipascal
proc[i8][bool]    proc[ptr, i64][]    proc[][]
```

`ptr` is an untyped pointer, similar to `void *`

Procedures are first-class and can have multiple arguments and multiple returns:

```millipascal
data buff "am i uppercase yet?\n"

proc main
begin
  byte_map[buff, sizeof[buff], upper_case];
  write[buff, sizeof[buff]];
end

proc byte_map[b:ptr, bsize:i64, op:proc[i8][i8]]
var i:i64
begin
  set i = 0;
  while i < bsize begin
    set (b+i)@i8 = op[(b+i)@i8];
    set i += 1;
  end
end

proc upper_case[a:i8] i8
begin
  if a >= 'a' and a <= 'z' begin
    return a - 32ss;
  end
  return a;
end
```

Globals can be declared in any order.

Procedures use square brackets `[]` instead of parenthesis `()`, so that
it doesn't share parentheses in the middle of expressions.

Control flow is `if`, `while`, `return` and `exit`.

To change the value of a variable, you use `set`.
You can declare static memory with `data`:

```millipascal
data w 32

proc main
begin
  set (w + 0)@i8 = 'H'
  set (w + 1)@i8 = 'e'
  set (w + 2)@i8 = 'y'
  set (w + 3)@i8 = '\n'

  write[w, 4]
end
```

This should print `Hey\n` to the console.

Pointer indirection is with the `@` operator, the right side is the type
expected at that location. `p@i64` reads 8 bytes from the pointer `p` as an
`i64`. For amd64 any `proc` type has 8 bytes of size.

Variables are declared together with the procedure, and cannot be
declared inside any inner block.

```millipascal
proc my_proc[]
var this, is, the, only, place, you, can, declare, variables:i64
begin
  exit 0ss;
end
```

The language also has a very simple module system, each file is a module
and all modules must live in the same folder. The name of the module is the
name of the file. Check the `test_suite/runtime` folder.
