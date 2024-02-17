from ioutil import fatal, put_int, put_char, put_bool

from dfl_alloc import
    init, print_list, print_map, validate_init

from bigint import
    new_bint, set_zero, set_bint,
    put_bint, copy_bint, free_bint,

    BASE, EQ, MORE, LESS,
    put_res,

    comp, comp_i32,
    div, div_i32,
    mult, mult_i32,
    add, add_i32,
    sub, sub_i32,
    neg

proc main
var a:ptr, b:ptr, c:ptr
begin
    init[]

    test_add[]
    test_add_i32[]

    test_sub[]
    test_sub_i32[]

    test_mult[]
    test_mult_i32[]

    test_div[]
    test_div_i32[]

    test_add_i32_vs_sub_i32[]
    test_add_vs_sub[]
    test_mult_i32_vs_mult[]
    test_copy[]
    test_mult_vs_div[]
end proc

proc test_div_i32
var a:ptr, q:ptr, rem:i32, i:i32, j:i32
begin
    set a = new_bint[~10l]
    set q = new_bint[0l]

    set i = ~100l
    while i < 100l
    begin
        set_bint[a, i]
        set j = ~100l
        while j < 100l
        begin
            set rem = div_i32[a, j, q]
            if comp_i32[q, i/j] != EQ begin
                put_bint[q]
                put_char[' ']
                put_int[(i/j):i64]
                put_char['\n']
                exit 36ss
            end if
            if rem != i%j begin
                exit 37ss
            end if

            set j += 1l
            if j == 0l begin
                set j += 1l
            end if
        end while
        set i += 1l
    end while

    free_bint[a]
    free_bint[q]
    validate_init[]
end proc

proc test_div
var a:ptr, b:ptr, q:ptr, r:ptr, i:i32, j:i32
begin
    set a = new_bint[~10l]
    set b = new_bint[1l]
    set q = new_bint[0l]
    set r = new_bint[0l]

    set i = ~100l
    while i < 100l
    begin
        set_bint[a, i]
        set j = ~100l
        while j < 100l
        begin
            set_bint[b, j]
            div[a, b, q, r]
            if comp_i32[q, i/j] != EQ begin
                exit 34ss
            end if
            if comp_i32[r, i%j] != EQ begin
                exit 35ss
            end if

            set j += 1l
            if j == 0l begin
                set j += 1l
            end if
        end while
        set i += 1l
    end while

    free_bint[a]
    free_bint[b]
    free_bint[q]
    free_bint[r]
    validate_init[]
end proc

proc test_mult
var a:ptr, b:ptr, c:ptr,
    i:i32, j:i32
begin
    set a = new_bint[~10l]
    set b = new_bint[1l]
    set c = new_bint[0l]

    set i = ~100l
    while i < 100l
    begin
        set_bint[a, i]
        set j = ~100l
        while j < 100l
        begin
            set_bint[b, j]
            mult[a, b, c]
            if comp_i32[c, i*j] != EQ begin
                exit 32ss
            end if

            set j += 1l
        end while
        set i += 1l
    end while

    free_bint[a]
    free_bint[b]
    free_bint[c]
    validate_init[]
end proc

proc test_mult_i32
var a:ptr, b:ptr,
    i:i32, j:i32
begin
    set a = new_bint[~10l]
    set b = new_bint[0l]

    set i = ~100l
    while i < 100l
    begin
        set_bint[a, i]
        set j = ~100l
        while j < 100l
        begin
            mult_i32[a, j, b]
            if comp_i32[b, i*j] != EQ begin
                put_char['$']
                put_int[i:i64]
                put_char[' ']
                put_int[j:i64]
                put_char[' ']
                put_bint[b]
                put_char['\n']
                exit 33ss
            end if

            set j += 1l
        end while
        set i += 1l
    end while

    free_bint[a]
    free_bint[b]
    validate_init[]
end proc

proc test_add
var a:ptr, b:ptr,
    i, res
begin
    set a = new_bint[~10l]
    set b = new_bint[1l]

    set i = 0
    while i < 20 begin
        add[a, b, a]
        set i += 1
    end while
    if comp_i32[a, 10l] != EQ begin
        exit 25ss
    end if

    set_bint[b, ~1l]
    set i = 0
    while i < 20 begin
        add[a, b, a]
        set i += 1
    end while
    if comp_i32[a, ~10l] != EQ begin
        exit 26ss
    end if
    
    free_bint[a]
    free_bint[b]
    validate_init[]
end proc

proc test_add_i32
var a:ptr, i, res
begin
    set a = new_bint[~10l]

    set i = 0
    while i < 20 begin
        add_i32[a, 1l, a]
        set i += 1
    end while
    if comp_i32[a, 10l] != EQ begin
        exit 27ss
    end if

    set i = 0
    while i < 20 begin
        add_i32[a, ~1l, a]
        set i += 1
    end while
    if comp_i32[a, ~10l] != EQ begin
        exit 28ss
    end if
    
    free_bint[a]
    validate_init[]
end proc

proc test_sub
var a:ptr, b:ptr,
    i, res
begin
    set a = new_bint[~10l]
    set b = new_bint[~1l]

    set i = 0
    while i < 20 begin
        sub[a, b, a]
        set i += 1
    end while
    if comp_i32[a, 10l] != EQ begin
        exit 29ss
    end if

    set_bint[b, 1l]
    set i = 0
    while i < 20 begin
        sub[a, b, a]
        set i += 1
    end while
    if comp_i32[a, ~10l] != EQ begin
        exit 30ss
    end if
    
    free_bint[a]
    free_bint[b]
    validate_init[]
end proc

proc test_sub_i32
var a:ptr, i, res
begin
    set a = new_bint[~10l]

    set i = 0
    while i < 20 begin
        sub_i32[a, ~1l, a]
        set i += 1
    end while
    if comp_i32[a, 10l] != EQ begin
        exit 31ss
    end if

    set i = 0
    while i < 20 begin
        sub_i32[a, 1l, a]
        set i += 1
    end while
    if comp_i32[a, ~10l] != EQ begin
        exit 32ss
    end if
    
    free_bint[a]
    validate_init[]
end proc

proc test_copy
var a:ptr, b:ptr, c:ptr, d:ptr, e:ptr
begin
    set a = new_bint[99l]
    mult_i32[a, 100_000_000l, a]
    mult_i32[a, 10l, a]
    set b = new_bint[0l]
    copy_bint[b, a]
    if comp[a, b] != EQ begin
        exit 14ss
    end if
    free_bint[a]
    free_bint[b]
    validate_init[]
end proc

proc test_mult_i32_vs_mult
var a:ptr, c:ptr, d:ptr, e:ptr, i
begin
    set a = new_bint[99l]
    set c = new_bint[99l]
    set d = new_bint[1l]
    mult_i32[d, 100_000_000l, d]
    mult_i32[d, 10l, d]
    set e = new_bint[0l]

    set i = 0
    while i < 100 begin
        mult_i32[a, 100_000_000l, a]
        mult_i32[a, 10l, a]

        mult[c, d, e]
        copy_bint[c, e]

        if comp[a, c] != EQ begin
            exit 13ss
        end if
        set i += 1
    end while

    free_bint[a]
    free_bint[c]
    free_bint[d]
    free_bint[e]
    validate_init[]
end proc

proc test_add_vs_sub
var a:ptr, b:ptr, c:ptr,
    i
begin
    set a = new_bint[64l]
    set b = new_bint[512l]
    mult_i32[a, 100_000_000l, a]
    mult_i32[a, 100_000_000l, a]
    mult_i32[b, 10_000_000l, b]
    set c = new_bint[0l]
    copy_bint[c, a]

    set i = 0
    while i < 1_000_000 begin
        sub[a, b, a]
        set i += 1
    end while
    set i = 0
    while i < 1_000_000 begin
        add[a, b, a]
        set i += 1
    end while

    if comp[a, c] != EQ begin
        put_char['!']
        put_bint[a]
        put_char[' ']
        put_bint[b]
        put_char[' ']
        put_bint[c]
        put_char['\n']
        exit 5ss
    end if

    free_bint[a]
    free_bint[b]
    free_bint[c]
    validate_init[]
end proc

proc test_add_i32_vs_sub_i32
var a:ptr, b:ptr, c:ptr,
    i
begin
    set a = new_bint[64l]
    set b = new_bint[0l]
    set c = new_bint[0l]
    mult_i32[a, 100_000_000l, c]
    copy_bint[a, c]
    copy_bint[b, c]

    set i = 0
    while i < 1_000_000 begin
        sub_i32[a, 64l, a]
        set i += 1
    end while
    set i = 0
    while i < 1_000_000 begin
        add_i32[a, 64l, a]
        set i += 1
    end while

    if comp[a, b] != EQ begin
        put_char['!']
        put_bint[a]
        put_char[' ']
        put_bint[b]
        put_char['\n']
        exit 6ss
    end if

    free_bint[a]
    free_bint[b]
    free_bint[c]
    validate_init[]
end proc

proc test_mult_vs_div
var a:ptr, b:ptr, c:ptr,
    q:ptr, r:ptr,
    i
begin
    set a = new_bint[64l]
    set b = new_bint[512l]
    mult_i32[a, 100_000_000l, a]
    mult_i32[a, 100_000_000l, a]
    set c = new_bint[0l]

    set q = new_bint[0l]
    set r = new_bint[0l]

    set i = 0
    while i < 1_000 begin
        div[a, b, q, r]
        mult[b, q, c]
        add[c, r, c]

        if comp[a, c] != EQ begin
            exit 21ss
        end if
        add_i32[a, 9l, a]
        set i += 1
    end while

    free_bint[a]
    free_bint[b]
    free_bint[c]
    free_bint[q]
    free_bint[r]
    validate_init[]
end proc
