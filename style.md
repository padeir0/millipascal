# Style

Although the language is deceptively simple, it's good to keep a 
consistent style. If something is not listed here, just try to follow
the style of `./test_suite/runtime/bigint.mp`.

## Identifiers

Constants and string data should be in ALL_CAPS_SNAKE_CASE:

```millipascal
const OBJ_SIZE_OFFSET 8
const OBJ_TAG_OFFSET 6
const OBJ_BITMAP_OFFSET 4

data ERR_NAT_OVERFLOW "number is too big (max 72 digits)\n"
data ERR_NAT_NEGATIVE "number subtraction went negative\n"
data ERR_DIVISION_BY_ZERO "division by zero\n"
```

Procedures should be in lower_snake_case:

```millipascal
proc test_guess[
    natIDD:ptr,
    scratch:ptr,
    natB:ptr,
    guess:i32
] i64
begin
	...
end

proc slow_div[natA, natB, natQ, natRem:ptr]
begin
	...
end
```

Reserved data should follow UpperCammelCase,
while local variables and arguments should follow lowerCamelCase:

```millipascal
data NatIDD 40
data Scratch 40

proc div[natA:ptr, natB:ptr, natQ:ptr, natRem:ptr]
var sizeA, sizeB, i, j:i64,
    low:i32, high:i32, guess:i32,
    res:i64,
begin
end
```

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
