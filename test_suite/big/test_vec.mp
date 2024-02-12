from
    ioutil
import
    put_int, put_char, put_ln, debug

from
    vec
import
    new_vec, free_vec,
    put_vec, put_vec_header,
    length,
    last, push, pop, index,
    concat, copy_vec, eq_vec, slice,
    set_meta

from
    dfl_alloc
import
    init, validate_init, print_list


proc main
begin
    init[]
    test_i64[]
    test_int[]
    test_string[]
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

proc printer_string[p:ptr]
begin
end proc

###### *int

proc test_int
begin
end proc

proc eq_int[a:ptr, b:ptr] bool
begin
    return false
end proc

proc copy_int[dest:ptr, source:ptr]
begin
end proc

proc printer_int[p:ptr]
begin
end proc

###### *i64

memory MetaI64 24

proc test_i64
begin
    set_meta[MetaI64, 8l, eq_i64, copy_i64]

    test_n[64, test_push_i64]
    test_n[64, test_iter_len_i64]
    test_n[64, test_pop_i64]
    test_n[64, test_concat_i64]
    test_n[64, test_copy1_eq_i64]
    test_n[64, test_copy2_eq_i64]
    test_n[64, test_slice_i64]
end proc

proc test_push_i64[size:i32]
var vecA:ptr, i
begin
    set vecA = new_vec[MetaI64, 0l, size/8l]
    set i = 0
    while i < size:i64 begin
        push[vecA]
        set index[vecA, i]@i64 = i*i
        set i += 1
    end while
    set i = 0
    while i < size:i64 begin
        if i*i != index[vecA, i]@i64 begin
            exit 1ss
        end if
        set i += 1
    end while
    free_vec[vecA]
    validate_init[]
end proc

proc test_iter_len_i64[size:i32]
var vecA:ptr, i
begin
    set vecA = new_vec[MetaI64, size, size]
    set i = 0
    while i < length[vecA] begin
        set index[vecA, i]@i64 = i*i
        set i += 1
    end while
    set i = 0
    while i < length[vecA] begin
        if i*i != index[vecA, i]@i64 begin
            exit 2ss
        end if
        set i += 1
    end while
    free_vec[vecA]
    validate_init[]
end proc

proc test_pop_i64[size:i32]
var vecA:ptr, i
begin
    set vecA = new_vec[MetaI64, size, size]
    set i = 0
    while i < length[vecA] begin
        set index[vecA, i]@i64 = i*i
        set i += 1
    end while
    set i = length[vecA]-1
    while length[vecA] > 0  begin
        if last[vecA]@i64 != i*i begin
            exit 3ss
        end if
        pop[vecA]
        set i -= 1
    end while
    free_vec[vecA]
    validate_init[]
end proc

proc test_concat_i64[size:i32]
var vecA:ptr, vecB:ptr, vecC:ptr,
    i, j
begin
    set vecA = new_vec[MetaI64, size, size]
    set j = 0
    set i = 0
    while i < length[vecA] begin
        set index[vecA, i]@i64 = j*j
        set i += 1
        set j += 1
    end while

    set vecB = new_vec[MetaI64, size/3l, size/3l]
    set i = 0
    while i < length[vecB] begin
        set index[vecB, i]@i64 = j*j
        set i += 1
        set j += 1
    end while

    set i = 0
    set vecC = concat[vecA, vecB]
    while i < j begin
        if index[vecC, i]@i64 != i*i begin
            exit 4ss
        end if
        set i += 1
    end while

    free_vec[vecA]
    free_vec[vecB]
    free_vec[vecC]
    validate_init[]
end proc

proc test_copy1_eq_i64[size:i32]
var vecA:ptr, vecB:ptr,
    i
begin
    set vecA = new_vec[MetaI64, size, size]
    set i = 0
    while i < length[vecA] begin
        set index[vecA, i]@i64 = i*i
        set i += 1
    end while

    set vecB = new_vec[MetaI64, 0l, size/2l]
    copy_vec[vecB, vecA]

    if not eq_vec[vecA, vecB] begin
        put_vec[vecA, printer_i64]
        put_ln[]
        put_vec[vecB, printer_i64]
        put_ln[]
        exit 5ss
    end if

    free_vec[vecA]
    free_vec[vecB]
    validate_init[]
end proc

proc test_copy2_eq_i64[size:i32]
var vecA:ptr, vecB:ptr, i
begin
    set vecA = new_vec[MetaI64, size, size]
    set i = 0
    while i < length[vecA] begin
        set index[vecA, i]@i64 = i*i
        set i += 1
    end while

    set vecB = new_vec[MetaI64, 0l, size]
    copy_vec[vecB, vecA]

    if not eq_vec[vecA, vecB] begin
        exit 6ss
    end if

    free_vec[vecA]
    free_vec[vecB]
    validate_init[]
end proc

proc test_slice_i64[size:i32]
var vecA:ptr, vecB:ptr, i
begin
    set vecA = new_vec[MetaI64, size, size]
    set i = 0
    while i < length[vecA] begin
        set index[vecA, i]@i64 = i*i
        set i += 1
    end while

    set vecB = slice[vecA, size:i64/2, size:i64 - (size:i64/2)]

    while 0 < length[vecB] begin
        if last[vecA]@i64 != last[vecB]@i64 begin
            put_vec[vecA, printer_i64]
            put_vec[vecB, printer_i64]
            exit 7ss
        end if
        pop[vecA]
        pop[vecB]
    end while

    free_vec[vecA]
    free_vec[vecB]
    validate_init[]
end proc

proc eq_i64[a:ptr, b:ptr] bool
begin
    return a@i64 == b@i64
end proc

proc copy_i64[dest:ptr, source:ptr]
begin
    set dest@i64 = source@i64
end proc

proc printer_i64[p:ptr]
begin
    put_int[p@i64]
end proc


############## UTILITARIES

proc test_n[amount, tester:proc[i32]]
var i
begin
    set i = 0
    while i < amount begin
        tester[i:i32]
        set i += 1
    end while
end proc

