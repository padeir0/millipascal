# Style

Although the language is deceptively simple, it's good to keep a 
consistent style.

## Identifiers

Constants and read-only string data should be in ALL_CAPS_SNAKE_CASE:

```millipascal
const OBJ_SIZE_OFFSET 8
const OBJ_TAG_OFFSET 6
const OBJ_BITMAP_OFFSET 4

data ERR_NAT_OVERFLOW "number is too big (max 72 digits)\n"
data ERR_NAT_NEGATIVE "number subtraction went negative\n"
data ERR_DIVISION_BY_ZERO "division by zero\n"
```

Procedures, variables, modules and writable data
should be in lower_snake_case:

```millipascal
data pool [_1KB]

proc test_guess[
    nat_IDD:ptr,
    scratch:ptr,
    nat_B:ptr,
    guess:i32
] i64
begin
	...
end

proc slow_div[nat_A, nat_B, nat_Q, nat_Rem:ptr]
begin
	...
end
```

While structs and fields should be in UpperCamelCase;

```millipascal
struct BigInt begin
    Array:I32A;
    Cap,Len:i16;
    Neg:bool;

    _pad1:u8; _pad2:u16;
end
```

Except unusable padding.

## Block delimiters

Regarding `begin`/`end` keywords, they should
always come in the same line as `if` and `while` keywords, unless
the conditions are broken into multiple lines. Eg:

```millipascal
if res == HIGH begin
    set high = guess;
end elseif res == LOW begin
    set low = guess;
end else begin 
    exit 1ss;
end
```

If the conditions are in multiple lines, it's preferable to
keep `begin` in another line:

```millipascal
if (guess*y == x) or
   (guess*y < x and (guess+1)*y > x)
begin
    return EQ;
end
```

However, never keep `begin` in the same line as `end`.
