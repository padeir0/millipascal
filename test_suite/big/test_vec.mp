from
    constants
import
    NULL

from
    ioutil
import
    put_int, put_uint, put_char, put_ln, debug

from
    vec
import
    new_vec, free_vec,
    put_vec, put_vec_header,
    length,
    last, push, pop, index,
    concat, copy_vec, eq_vec, slice,
    set_meta,
    get_eq, get_copy

from
    dfl_alloc
import
    init, validate_init, print_list, print_map

import bigint
import str

proc main
begin
    init[]
    test_i64[]
    test_int_naive[]
    #test_string[]
end proc

###### *str

proc test_string
begin
end proc

proc eq_string[a:ptr, b:ptr] bool
begin
    return false
end proc

proc copy_string[dest:ptr, source:ptr]
begin
end proc

###### *int (naïve)

data MetaIntNaive [32]

proc test_int_naive
begin
    set_meta[
        MetaIntNaive,
        8l,
        eq_int_naive,
        copy_int_naive,
        free_int_naive
    ]
    test_all[
        64,
        MetaIntNaive,
        setter_int_naive,
        tester_int_naive
    ]
end proc

proc eq_int_naive[a:ptr, b:ptr] bool
begin
    return bigint::comp[a@ptr, b@ptr] == bigint::EQ
end proc

proc copy_int_naive[dest:ptr, source:ptr]
begin
    set dest@ptr = bigint::new_bint[0l]
    bigint::copy_bint[dest@ptr, source@ptr]
end proc

proc setter_int_naive[vecA:ptr, i]
var bint:ptr
begin
    set bint = bigint::new_bint[1l]
    power2[bint, i:i32]
    set index[vecA, i]@ptr = bint
end proc

# testers return true on failure
proc tester_int_naive[vecA:ptr, i] bool
var bint:ptr, iRes:ptr, out:bool
begin
    set iRes = bigint::new_bint[1l]
    power2[iRes, i:i32]

    set bint = index[vecA, i]@ptr
    set out = bigint::comp[bint, iRes] != bigint::EQ
    bigint::free_bint[iRes]
    return out
end proc

proc power2[bint:ptr, pw:i32] ptr
var i
begin
    set i = 0
    while i < pw:i64 begin
        bigint::mult_i32[bint, 2l, bint]
        set i += 1
    end while
    return bint
end proc

proc free_int_naive[item:ptr]
begin
    bigint::free_bint[item@ptr]
end proc

###### *i64

data MetaI64 [32]

proc test_i64
begin
    set_meta[MetaI64, 8l, eq_i64, copy_i64, noop]
    test_all[
        64,
        MetaI64,
        setter_i64,
        tester_i64
    ]
end proc

proc eq_i64[a:ptr, b:ptr] bool
begin
    return a@i64 == b@i64
end proc

proc copy_i64[dest:ptr, source:ptr]
begin
    set dest@i64 = source@i64
end proc

proc setter_i64[vecA:ptr, i]
begin
    set index[vecA, i]@i64 = i*i
end proc

# testers return true on failure
proc tester_i64[vecA:ptr, i] bool
begin
    return i*i != index[vecA, i]@i64
end proc

proc noop[a:ptr]
begin
end proc

############## GENERAL TESTS

proc test_push[
    meta:ptr,
    size:i32,
    setter:proc[ptr, i64],
    tester:proc[ptr, i64] bool
]
var vecA:ptr, i
begin
    set vecA = new_vec[meta, 0l, size/8l]
    set i = 0
    while i < size:i64 begin
        push[vecA]
        setter[vecA, i]
        set i += 1
    end while
    set i = 0
    while i < size:i64 begin
        if tester[vecA, i] begin
            exit 1ss
        end if
        set i += 1
    end while

    free_vec[vecA]
    validate_init[]
end proc

proc test_iter_len[
    meta:ptr,
    size:i32,
    setter:proc[ptr, i64],
    tester:proc[ptr, i64] bool
]
var vecA:ptr, i
begin
    set vecA = new_vec[meta, size, size]
    set i = 0
    while i < length[vecA] begin
        setter[vecA, i]
        set i += 1
    end while
    set i = 0
    while i < length[vecA] begin
        if tester[vecA, i] begin
            put_vec[vecA, printer]
            put_ln[]
            exit 2ss
        end if
        set i += 1
    end while

    free_vec[vecA]
    validate_init[]
end proc

proc test_pop[
    meta:ptr,
    size:i32,
    setter:proc[ptr, i64],
    tester:proc[ptr, i64] bool
]
var vecA:ptr, i, initial_length
begin
    set vecA = new_vec[meta, size, size]

    set i = 0
    while i < length[vecA] begin
        setter[vecA, i]
        set i += 1
    end while
    set i = length[vecA]-1
    while length[vecA] > 0  begin
        if tester[vecA, i] begin
            exit 3ss
        end if
        pop[vecA]
        set i -= 1
    end while

    free_vec[vecA]
    validate_init[]
end proc

proc test_concat[
    meta:ptr,
    size:i32,
    setter:proc[ptr, i64],
    tester:proc[ptr, i64] bool
]
var vecA:ptr, vecB:ptr, vecC:ptr,
    i, j, z,
    eq:proc[ptr,ptr]bool
begin
    set vecA = new_vec[meta, size, size]
    set vecB = new_vec[meta, size/3l, size/3l]
    set eq = get_eq[vecA]

    set i = 0
    while i < length[vecA] begin
        setter[vecA, i]
        set i += 1
    end while

    set j = 0
    while j < length[vecB] begin
        setter[vecB, j]
        set j += 1
    end while

    set z = 0
    set vecC = concat[vecA, vecB]
    while z < i+j begin
        if z < i  begin
            # millipascal's operators are not shortcircuited
            if not eq[index[vecA, z], index[vecC, z]] begin
                exit 4ss
            end if
        end if
        if z >= i  begin
            if not eq[index[vecB, z-i], index[vecC, z]] begin
                exit 4ss
            end if
        end if
        set z += 1
    end while

    free_vec[vecA]
    free_vec[vecB]
    free_vec[vecC]
    validate_init[]
end proc

proc test_copy1_eq[
    meta:ptr,
    size:i32,
    setter:proc[ptr, i64],
    tester:proc[ptr, i64] bool
]
var vecA:ptr, vecB:ptr,
    i
begin
    set vecA = new_vec[meta, size, size]
    set i = 0
    while i < length[vecA] begin
        setter[vecA, i]
        set i += 1
    end while

    set vecB = new_vec[meta, 0l, size/2l]
    copy_vec[vecB, vecA]

    if not eq_vec[vecA, vecB] begin
        exit 5ss
    end if

    free_vec[vecA]
    free_vec[vecB]
    validate_init[]
end proc

proc test_copy2_eq[
    meta:ptr,
    size:i32,
    setter:proc[ptr, i64],
    tester:proc[ptr, i64] bool
]
var vecA:ptr, vecB:ptr, i
begin
    set vecA = new_vec[meta, size, size]
    set i = 0
    while i < length[vecA] begin
        setter[vecA, i]
        set i += 1
    end while

    set vecB = new_vec[meta, 0l, size]
    copy_vec[vecB, vecA]

    if not eq_vec[vecA, vecB] begin
        exit 6ss
    end if

    free_vec[vecA]
    free_vec[vecB]
    validate_init[]
end proc

proc test_slice[
    meta:ptr,
    size:i32,
    setter:proc[ptr, i64],
    tester:proc[ptr, i64] bool
]
var vecA:ptr, vecB:ptr, i, eq:proc[ptr, ptr]bool
begin
    set vecA = new_vec[meta, size, size]
    set eq = get_eq[vecA]
    set i = 0
    while i < length[vecA] begin
        setter[vecA, i]
        set i += 1
    end while

    set vecB = slice[vecA, size:i64/2, size:i64 - (size:i64/2)]

    while 0 < length[vecB] begin
        if not eq[last[vecA], last[vecB]] begin
            exit 7ss
        end if

        pop[vecA]
        pop[vecB]
    end while

    free_vec[vecA]
    free_vec[vecB]
    validate_init[]
end proc

############## UTILITARIES

proc test_all[
    amount,
    meta:ptr,
    setter:proc[ptr, i64],
    tester:proc[ptr, i64]bool
] begin
    test_n[amount, meta, test_push, setter, tester]
    # test_n[amount, meta, test_iter_len, setter, tester]
    # test_n[amount, meta, test_pop,      setter, tester]
    # test_n[amount, meta, test_concat,   setter, tester]
    # test_n[amount, meta, test_copy1_eq, setter, tester]
    # test_n[amount, meta, test_copy2_eq, setter, tester]
    # test_n[amount, meta, test_slice,    setter, tester]
end proc

proc test_n[
    amount,
    meta:ptr,
    test:proc[ptr, i32, proc[ptr, i64], proc[ptr, i64]bool],
    setter:proc[ptr, i64],
    tester:proc[ptr, i64]bool
]
var i
begin
    set i = 0
    while i < amount begin
        test[meta, i:i32, setter, tester]
        set i += 1
    end while
end proc

proc printer[a:ptr]
begin
    put_uint[a@u64]
end proc
