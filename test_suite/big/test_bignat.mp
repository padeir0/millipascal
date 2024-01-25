from ioutil import put_char, put_int
from bignat import
    BASE, NAT_SIZE, CAP, EQ, MORE, LESS,
    div, slow_div, # div_digit, div_base,
    comp, comp_digit,
    mult, mult_digit, mult_base,
    add, add_digit,
    sub, # sub_digit,
    put_bnat,
    set_bnat, set_zero,
    less, less_eq, more, more_eq, eq, neq,
    put_res

memory num_A 40 # [length:i64, i32...]
memory num_B 40 # 8 * i32
memory num_C 40
memory num_D 40

memory num_E 40
memory num_F 40

proc main
begin
    test_div1[] # too slow testing
    test_div2[]
    test_add1[]
end proc

proc test_div1
var i:i32, j:i32,
    a:ptr, b:ptr, c:ptr, d:ptr, e:ptr, f:ptr
begin
    set a = num_A
    set b = num_B
    set c = num_C
    set d = num_D
    set e = num_E
    set f = num_F

    set i = 1l
    while i < 3l begin
        set_bnat[a, i*111l, i, 0l]

        set j = 113l
        while j < 4096l begin
            set_bnat[b, j, 0l, 0l]

            set_zero[c]
            set_zero[d]
            set_zero[e]
            set_zero[f]

            div[a, b, c, d]
            slow_div[a, b, e, f]

            if comp[c, e] != EQ begin
                put_bnat[c]
                put_char['!']
                put_char['=']
                put_bnat[e]
                put_char['\n']
                exit 1ss
            end if
            if comp[d, f] != EQ begin
                put_bnat[d]
                put_char['!']
                put_char['=']
                put_bnat[f]
                put_char['\n']
                exit 2ss
            end if

            set j *= 7l
        end while
        set i += 1l
    end while
end proc

proc test_div2
var i:i32, j:i32,
    a:ptr, b:ptr, c:ptr, d:ptr
begin
    set a = num_A
    set b = num_B
    set c = num_C
    set d = num_D

    set i = 1l
    while i < 65536l begin
        set_bnat[a, i, 0l, 0l]

        set j = 1l
        while j < 65536l begin
            set_bnat[b, j, 0l, 0l]

            set_zero[c]
            set_zero[d]

            div[a, b, c, d]

            if comp_digit[c, i/j] != EQ begin
                put_bnat[c]
                put_char['!']
                put_char['=']
                put_int[(i/j):i64]
                put_char['\n']
                exit 3ss
            end if
            if comp_digit[d, i%j] != EQ begin
                put_bnat[d]
                put_char['!']
                put_char['=']
                put_int[(i%j):i64]
                put_char['\n']
                exit 4ss
            end if

            set j *= 7l
        end while
        set i *= 3l
    end while
end proc

proc test_add1
var i:i32, j:i32,
    a:ptr, b:ptr, c:ptr
begin
    set a = num_A
    set b = num_B
    set c = num_C

    set i = 1l
    while i < 1024l begin
        set_bnat[a, i, 0l, 0l]

        set j = 1l
        while j < 1024l begin
            set_bnat[b, j, 0l, 0l]
            set_zero[c]

            add[a, b]

            if comp_digit[a, i+j] != EQ begin
                put_bnat[c]
                put_char['!']
                put_char['=']
                put_int[(i/j):i64]
                put_char['\n']
                exit 5ss
            end if

            set_bnat[a, i, 0l, 0l]
            set j += 1l
        end while
        set i += 1l
    end while
end proc
