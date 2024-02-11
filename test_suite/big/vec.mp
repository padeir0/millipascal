from ioutil import put_char, put_int, put_ln, debug, fatal
from constants import NULL, WORD
from dfl_alloc import alloc_f, alloc_b, free, print_list

export
    new_vec, free_vec,
    put_vec, length,
    last, push, pop, index,
    set_meta

proc main
begin
end proc

# each metadata object has 24 bytes, separated in the following layout:
#   - 4 bytes: size of each item (in words)
#   - 4 bytes padding
#   - 8 bytes: pointer to equality procedure (proc[a:ptr,b:ptr]bool)
#   - 8 bytes: pointer to copy procedure (proc[dest:ptr, source:ptr])
# all pointers in the above procedures are pointers to
# inside the array, that is, they're just calculated indexes
# of the array.

const META_SIZE 24

const M_ITEMSIZE_OFFSET 0p
const M_EQPROC_OFFSET 8p
const M_COPYPROC_OFFSET 16p

proc set_meta[
    meta:ptr,
    itemSize:i32,
    eqProc:proc[ptr,ptr]bool,
    copyProc:proc[ptr,ptr]
]
begin
    set (meta+M_ITEMSIZE_OFFSET)@i32 = itemSize
    set (meta+M_EQPROC_OFFSET)@proc[ptr,ptr]bool = eqProc
    set (meta+M_COPYPROC_OFFSET)@proc[ptr,ptr] = copyProc
end proc

# vec header has 24 bytes of size, consisting of:
#     - 8 bytes: pointer to underlying array
#     - 4 bytes: length
#     - 4 bytes: capacity
#     - 8 bytes: pointer to metadata table

const VEC_HEADER_SIZE 24

const VH_PTR_OFFSET 0p   # ptr
const VH_LEN_OFFSET 8p   # i32
const VH_CAP_OFFSET 12p  # i32
const VH_META_OFFSET 16p # ptr

memory ERR_INVALID_CAP "error: capacity is smaller than length\n"

# pointer in the first and third word
const VEC_PTR_BITMAP 0b10100000_00000000_00000000_00000000ul

proc new_vec[meta:ptr, length:i32, cap:i32] ptr
var itemSize:i32, header:ptr, array:ptr
begin
    if length > cap begin
        fatal[ERR_INVALID_CAP, ERR_INVALID_CAP.size]
    end if

    set itemSize = (meta+M_ITEMSIZE_OFFSET)@i32
    set header = alloc_b[VEC_HEADER_SIZE, 0uss, 0us, VEC_PTR_BITMAP]
    set array = alloc_f[(itemSize * cap):i64]

    set (header+VH_PTR_OFFSET)@ptr = array
    set (header+VH_LEN_OFFSET)@i32 = length
    set (header+VH_CAP_OFFSET)@i32 = cap
    set (header+VH_META_OFFSET)@ptr = meta
    
    return header
end proc

# no vectors share the underlying array,
# this is important! vectors are mutable.
proc free_vec[vec:ptr]
var i
begin
    free[(vec+VH_PTR_OFFSET)@ptr]
    free[vec]
end proc

proc put_vec[vec:ptr, printer:proc[ptr]]
var i, len
begin
    set len = (vec+VH_LEN_OFFSET)@i32:i64
    set i = 0
    put_char['[']
    while i < len begin
        printer[index[vec, i]]
        if i < len-1 begin
            put_char[',']
            put_char[' ']
        end if
        set i += 1
    end while
    put_char[']']
end proc

proc length[vec:ptr] i64
begin
    return (vec+VH_LEN_OFFSET)@i32:i64
end proc

proc last[vec:ptr] ptr
begin
    return index[vec, length[vec]-1]
end proc

memory ERR_OUT_OF_BOUNDS "out of bounds!\n"

# Computes only the address of the item in the array,
# any subsequent indirections and type specific shenanigans
# shall be done externally.
# This allows us to use the same implementation even
# if the compiler chooses to inline some structures.
# Bitvectors may be implemented separatedly if necessary.
proc index[vec:ptr, index] ptr
var meta:ptr, array:ptr, len:i32, itemSize:i32
begin
    set array = (vec+VH_PTR_OFFSET)@ptr
    set len = (vec+VH_LEN_OFFSET)@i32

    if index:i32 >= len and index:i32 < 0l begin
        fatal[ERR_OUT_OF_BOUNDS, ERR_OUT_OF_BOUNDS.size]
    end if

    set meta = (vec+VH_META_OFFSET)@ptr
    set itemSize = (meta+M_ITEMSIZE_OFFSET)@i32

    return array+(itemSize:ptr*index:ptr)
end proc

# allocates a new vector; copies A and B into it
# returns the vector;
proc concat[vecA:ptr, vecB:ptr] ptr
begin
    # if metaA != metaB then fatal, because they must match types
    return NULL
end proc

proc eq[vecA:ptr, vecB:ptr] bool
begin
    return false
end proc

proc copy[vecA:ptr, vecB:ptr] ptr
begin
    return NULL
end proc

# Make a new header; alloc a new array;
# copy the contents; return it;
proc slice[vec:ptr, start:i64, length:i64] ptr
begin
    return NULL
end proc

memory ERR_POPPED_TOO_MUCH "pop in array with length 0\n"

# just decrements the length by 1,
# everything else shall be type specific
proc pop[vec:ptr]
var len:i32
begin
    set len = (vec+VH_LEN_OFFSET)@i32
    if len == 0l begin
        fatal[ERR_POPPED_TOO_MUCH, ERR_POPPED_TOO_MUCH.size]
    end if
    set (vec+VH_LEN_OFFSET)@i32 -= 1l
end proc

# just increments the length by 1, may trigger allocation,
# everything else shall be type specific
proc push[vec:ptr]
var meta:ptr, array:ptr, len:i32, cap:i32, itemSize:i32,
    new_array:ptr, copy:proc[ptr, ptr],
    i, a:ptr, b:ptr
begin
    set len = (vec+VH_LEN_OFFSET)@i32
    set cap = (vec+VH_CAP_OFFSET)@i32

    if len == cap begin
        set array = (vec+VH_PTR_OFFSET)@ptr
        set meta = (vec+VH_META_OFFSET)@ptr
        set itemSize = (meta+M_ITEMSIZE_OFFSET)@i32
        set copy = (meta+M_COPYPROC_OFFSET)@proc[ptr, ptr]

        set cap *= 2l
        set new_array = alloc_f[ (cap*itemSize):i64 ]

        set i = 0
        while i < len:i64 begin
            set a = array+(itemSize:ptr*i:ptr)
            set b = new_array+(itemSize:ptr*i:ptr)

            copy[b, a]

            set i += 1
        end while
        free[array]
        set (vec+VH_PTR_OFFSET)@ptr = new_array
        set (vec+VH_CAP_OFFSET)@i32= cap
    end if

    set (vec+VH_LEN_OFFSET)@i32 += 1l
end proc
