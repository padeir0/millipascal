from
    ioutil
import
    put_int, put_char, put_ln, debug

from
    vec
import
    new_vec, free_vec,
    put_vec, length,
    last, push, pop, index,
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
    test_push_i64[]
    test_iter_len_i64[]
    test_pop_i64[]
end proc

proc test_push_i64
var vecA:ptr, i
begin
    set vecA = new_vec[MetaI64, 0l, 4l]
    set i = 0
    while i < 32 begin
        push[vecA]
        set index[vecA, i]@i64 = i*i
        set i += 1
    end while
    set i = 0
    while i < 32 begin
        if i*i != index[vecA, i]@i64 begin
            exit 1ss
        end if
        set i += 1
    end while
    free_vec[vecA]
    validate_init[]
end proc

proc test_iter_len_i64
var vecA:ptr, i
begin
    set vecA = new_vec[MetaI64, 16l, 16l]
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

proc test_pop_i64
var vecA:ptr, i
begin
    set vecA = new_vec[MetaI64, 32l, 32l]
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

proc free_i64[p:ptr]
begin
    # does nothing :)
end proc
