from
    buffutil
import
    clear_bytes

from
    ioutil
import
    put_char, put_int, put_uint, put_ln, debug, fatal, oom

from
    constants
import
    NULL, WORD

from
    dfl_alloc
import
    alloc_f, alloc_b, free, print_list

export
    new_vec, free_vec,
    put_vec, put_vec_header,
    length,
    last, push, pop, index,
    concat, copy_vec, eq_vec, slice,
    set_meta,
    get_eq, get_copy,
    clear,
    META_SIZE

proc main
begin
end

# each metadata object has 32 bytes, separated in the following layout:
#   - 4 bytes: size of each item (in bytes)
#   - 4 bytes padding
#   - 8 bytes: pointer to equality procedure (proc[a:ptr,b:ptr]bool)
#   - 8 bytes: pointer to copy procedure (proc[dest:ptr, source:ptr])
#   - 8 bytes: pointer to free item procedure (proc[item:ptr])
# all pointers in the above procedures are pointers to
# inside the array, that is, they're just calculated indexes
# of the array.

const META_SIZE = 32

const M_ITEMSIZE_OFFSET = 0p  # i32
const M_EQPROC_OFFSET = 8p    # proc[ptr, ptr]bool
const M_COPYPROC_OFFSET = 16p # proc[ptr, ptr]
const M_FREEPROC_OFFSET = 24p # proc[ptr]

proc set_meta[
    meta:ptr,
    itemSize:i32,
    eqProc:proc[ptr,ptr]bool,
    copyProc:proc[ptr,ptr][],
    freeProc:proc[ptr][]
]
begin
    set (meta+M_ITEMSIZE_OFFSET)@i32 = itemSize
    set (meta+M_EQPROC_OFFSET)@proc[ptr,ptr]bool = eqProc
    set (meta+M_COPYPROC_OFFSET)@proc[ptr,ptr][] = copyProc
    set (meta+M_FREEPROC_OFFSET)@proc[ptr][] = freeProc
end

# vec header has 24 bytes of size, consisting of:
#     - 8 bytes: pointer to underlying array
#     - 4 bytes: length
#     - 4 bytes: capacity
#     - 8 bytes: pointer to metadata table

const VEC_HEADER_SIZE = 24

const VH_PTR_OFFSET = 0p   # ptr
const VH_LEN_OFFSET = 8p   # i32
const VH_CAP_OFFSET = 12p  # i32
const VH_META_OFFSET = 16p # ptr

data ERR_INVALID_CAP "error: capacity is smaller than length\n"

# pointer in the first and third word
const VEC_PTR_BITMAP = 0b10100000_00000000_00000000_00000000ul

proc new_vec[meta:ptr, length, cap:i32] ptr
var itemSize:i32, header, array:ptr
begin
    if length > cap begin
        fatal[ERR_INVALID_CAP, sizeof ERR_INVALID_CAP]
    end

    set itemSize = (meta+M_ITEMSIZE_OFFSET)@i32
    set header = alloc_b[VEC_HEADER_SIZE, 0uss, 0us, VEC_PTR_BITMAP]
    if header == NULL begin
        oom[]
    end

    set array = alloc_f[(itemSize * cap):i64]
    if array == NULL begin
        oom[]
    end

    set (header+VH_PTR_OFFSET)@ptr = array
    set (header+VH_LEN_OFFSET)@i32 = length
    set (header+VH_CAP_OFFSET)@i32 = cap
    set (header+VH_META_OFFSET)@ptr = meta
    
    return header
end

# no vectors share the underlying array,
# this is important! vectors are mutable.
proc free_vec[vec:ptr]
begin
    # if free != no_op begin # TODO: needs procedures to be comparable
    #     do the thing
    # end
    free_items[vec]
    free[(vec+VH_PTR_OFFSET)@ptr]
    free[vec]
end

proc free_items[vec:ptr]
var array, meta:ptr, itemSize:i64, free_item:proc[ptr][]
begin
    set meta      = (vec+VH_META_OFFSET)@ptr
    set free_item = (meta+M_FREEPROC_OFFSET)@proc[ptr][]
    set itemSize  = (meta+M_ITEMSIZE_OFFSET)@i32:i64
    set array     = (vec+VH_PTR_OFFSET)@ptr

    free_array_items[array, itemSize, length[vec], free_item]
end

proc free_array_items[array:ptr, itemSize, len:i64, free_item:proc[ptr][]]
var i:i64
begin
    set i = 0
    while i < len begin
        free_item[ array+(itemSize:ptr*i:ptr) ]
        set i += 1
    end
end

proc put_vec_header[vec:ptr]
var array, meta:ptr,
    len, cap:i64
begin
    set array = (vec+VH_PTR_OFFSET)@ptr
    set len   = (vec+VH_LEN_OFFSET)@i32:i64
    set cap   = (vec+VH_CAP_OFFSET)@i32:i64
    set meta  = (vec+VH_META_OFFSET)@ptr

    put_char['{']
    put_char['a']
    put_char[':']
    put_uint[array:u64]
    put_char[' ']
    put_char['l']
    put_char[':']
    put_int[len:i64]
    put_char[' ']
    put_char['c']
    put_char[':']
    put_int[cap:i64]
    put_char[' ']
    put_char['m']
    put_char[':']
    put_uint[meta:u64]
    put_char['}']
end

proc get_copy[vec:ptr] proc[ptr, ptr][] # TODO: refactor the code by employing these "get" procedures
begin
    return (
        (vec+VH_META_OFFSET)@ptr+ # meta
        M_COPYPROC_OFFSET
    )@proc[ptr,ptr][]
end

proc get_eq[vec:ptr] proc[ptr, ptr] bool
begin
    return (
        (vec+VH_META_OFFSET)@ptr+ # meta
        M_EQPROC_OFFSET
    )@proc[ptr,ptr]bool
end

proc put_vec[vec:ptr, printer:proc[ptr][]]
var i, len:i64
begin
    set len = (vec+VH_LEN_OFFSET)@i32:i64
    set i = 0
    put_char['[']
    while i < len begin
        printer[index[vec, i]]
        if i < len-1 begin
            put_char[',']
            put_char[' ']
        end
        set i += 1
    end
    put_char[']']
end

proc length[vec:ptr] i64
begin
    return (vec+VH_LEN_OFFSET)@i32:i64
end

proc last[vec:ptr] ptr
begin
    return index[vec, length[vec]-1]
end

data ERR_OUT_OF_BOUNDS "out of bounds!\n"

# Computes only the address of the item in the array,
# any subsequent indirections and type specific shenanigans
# shall be done externally.
# This allows us to use the same implementation even
# if the compiler chooses to inline some structures.
# Bitvectors may be implemented separatedly if necessary.
proc index[vec:ptr, index:i64] ptr
var meta, array:ptr,
    len, itemSize:i32
begin
    set array = (vec+VH_PTR_OFFSET)@ptr
    set len = (vec+VH_LEN_OFFSET)@i32

    if index:i32 >= len or index:i32 < 0l begin
        fatal[ERR_OUT_OF_BOUNDS, sizeof ERR_OUT_OF_BOUNDS]
    end

    set meta = (vec+VH_META_OFFSET)@ptr
    set itemSize = (meta+M_ITEMSIZE_OFFSET)@i32

    return array+(itemSize:ptr*index:ptr)
end

data ERR_INCOMPATIBLE_META "vectors have incompatible metadata\n"

# allocates a new vector; copies A and B into it
# returns the vector;
proc concat[vecA, vecB:ptr] ptr
var metaA, metaB:ptr,
    lenA, lenB, itemSize:i64,
    vecRes:ptr,
    aArray, bArray, destArray:ptr,
    copy:proc[ptr, ptr][]
begin
    # if metaA != metaB then fatal, because they must match types
    set metaA = (vecA+VH_META_OFFSET)@ptr
    set metaB = (vecB+VH_META_OFFSET)@ptr

    if metaA != metaB begin
        fatal[ERR_INCOMPATIBLE_META, sizeof ERR_INCOMPATIBLE_META]
    end

    set itemSize = (metaA+M_ITEMSIZE_OFFSET)@i32:i64
    set copy = (metaA+M_COPYPROC_OFFSET)@proc[ptr,ptr][]

    set lenA = (vecA+VH_LEN_OFFSET)@i32:i64
    set lenB = (vecB+VH_LEN_OFFSET)@i32:i64

    set vecRes = new_vec[metaA, (lenA+lenB):i32, (lenA+lenB):i32]

    set destArray = (vecRes+VH_PTR_OFFSET)@ptr
    set aArray = (vecA+VH_PTR_OFFSET)@ptr
    set bArray = (vecB+VH_PTR_OFFSET)@ptr

    copy_array[destArray, aArray, lenA, itemSize:i64, copy]
    copy_array[destArray+(lenA*itemSize):ptr, bArray, lenB, itemSize:i64, copy]

    return vecRes
end

proc eq_vec[vecA, vecB:ptr] bool
var lenA:i64, metaA:ptr,
    lenB:i64, metaB:ptr,
    eq:proc[ptr, ptr]bool, i:i64,
    a:ptr, b:ptr
begin
    set metaA = (vecA+VH_META_OFFSET)@ptr
    set metaB = (vecB+VH_META_OFFSET)@ptr

    if metaA != metaB begin # should we crash instead?
        return false
    end

    set eq = (metaA+M_EQPROC_OFFSET)@proc[ptr, ptr]bool

    set lenA = (vecA+VH_LEN_OFFSET)@i32:i64
    set lenB = (vecB+VH_LEN_OFFSET)@i32:i64
    if lenA != lenB begin
        return false
    end

    set i = 0
    while i < lenA begin
        set a = index[vecA, i]
        set b = index[vecB, i]

        if not eq[a, b] begin
            return false
        end
        set i += 1
    end

    return true
end

proc copy_vec[vecA, vecB:ptr]
var arrayA:ptr, lenA, capA:i64, metaA:ptr,
    arrayB:ptr, lenB, capB:i64, metaB:ptr,
    itemSize:i64, copy:proc[ptr, ptr][],
    newArray:ptr
begin
    set metaA = (vecA+VH_META_OFFSET)@ptr
    set metaB = (vecB+VH_META_OFFSET)@ptr

    if metaA != metaB begin
        fatal[ERR_INCOMPATIBLE_META, sizeof ERR_INCOMPATIBLE_META]
    end

    set itemSize = (metaA+M_ITEMSIZE_OFFSET)@i32:i64
    set copy = (metaA+M_COPYPROC_OFFSET)@proc[ptr, ptr][]

    set lenB = (vecB+VH_LEN_OFFSET)@i32:i64
    set capA = (vecA+VH_CAP_OFFSET)@i32:i64

    set arrayA = (vecA+VH_PTR_OFFSET)@ptr
    set arrayB = (vecB+VH_PTR_OFFSET)@ptr

    free_items[vecA]

    if capA < lenB begin
        free[arrayA]
        set newArray = alloc_f[ (lenB*itemSize):i64 ]
        if newArray == NULL begin
            oom[]
        end
        set (vecA+VH_PTR_OFFSET)@ptr = newArray
        set (vecA+VH_CAP_OFFSET)@i32 = lenB:i32
        set arrayA = (vecA+VH_PTR_OFFSET)@ptr
    end
    copy_array[arrayA, arrayB, lenB, itemSize, copy]
    set (vecA+VH_LEN_OFFSET)@i32 = lenB:i32
end

data ERR_SLICE_OUT_OF_BOUNDS "slicing goes out of bounds\n"

# Make a new header; alloc a new array;
# copy the contents; return it;
proc slice[vec:ptr, start, length:i64] ptr
var array:ptr, len:i64, meta:ptr,
    arrayOut, vecOut:ptr,
    itemSize:i64, copy:proc[ptr, ptr][]
begin
    set len   = (vec+VH_LEN_OFFSET)@i32:i64

    if start+length > len begin
        fatal[ERR_SLICE_OUT_OF_BOUNDS, sizeof ERR_SLICE_OUT_OF_BOUNDS]
    end
    set array    = (vec+VH_PTR_OFFSET)@ptr
    set meta     = (vec+VH_META_OFFSET)@ptr
    set itemSize = (meta+M_ITEMSIZE_OFFSET)@i32:i64
    set copy     = (meta+M_COPYPROC_OFFSET)@proc[ptr, ptr][]

    set vecOut   = new_vec[meta, length:i32, length:i32]
    set arrayOut = (vecOut+VH_PTR_OFFSET)@ptr
    
    copy_array[arrayOut, array+(start*itemSize):ptr, length, itemSize, copy]
    
    return vecOut
end

data ERR_POPPED_TOO_MUCH "pop in array with length 0\n"

# just decrements the length by 1, freeing the underlying object, if any
# everything else shall be type specific
proc pop[vec:ptr]
var len:i32, meta:ptr, free_item:proc[ptr][]
begin
    set len = (vec+VH_LEN_OFFSET)@i32
    if len == 0l begin
        fatal[ERR_POPPED_TOO_MUCH, sizeof ERR_POPPED_TOO_MUCH]
    end
    set meta = (vec+VH_META_OFFSET)@ptr
    set free_item = (meta+M_FREEPROC_OFFSET)@proc[ptr][]
    free_item[last[vec]]
    set (vec+VH_LEN_OFFSET)@i32 -= 1l
end

# just increments the length by 1, may trigger allocation,
# everything else shall be type specific
proc push[vec:ptr]
var meta, array:ptr, len, cap, itemSize:i64,
    newArray:ptr,
    copy:proc[ptr, ptr][], free_item:proc[ptr][]
begin
    set len = (vec+VH_LEN_OFFSET)@i32:i64
    set cap = (vec+VH_CAP_OFFSET)@i32:i64

    if len == cap begin
        set array     = (vec+VH_PTR_OFFSET)@ptr
        set meta      = (vec+VH_META_OFFSET)@ptr
        set itemSize  = (meta+M_ITEMSIZE_OFFSET)@i32:i64
        set copy      = (meta+M_COPYPROC_OFFSET)@proc[ptr, ptr][]
        set free_item = (meta+M_FREEPROC_OFFSET)@proc[ptr][]

        if cap == 0 begin
            set cap = 2
        end else begin
            set cap *= 2
        end
        set newArray = alloc_f[ cap*itemSize ]
        if newArray == NULL begin
            oom[]
        end

        copy_array[newArray, array, len, itemSize, copy]

        free_array_items[array, itemSize, len, free_item]
        free[array]
        set (vec+VH_PTR_OFFSET)@ptr = newArray
        set (vec+VH_CAP_OFFSET)@i32 = cap:i32
    end

    set (vec+VH_LEN_OFFSET)@i32 += 1l
end

proc clear[vec:ptr]
var array, meta:ptr,
    itemSize, i:i64
begin
    set meta = (vec+VH_META_OFFSET)@ptr
    set itemSize = (meta+M_ITEMSIZE_OFFSET)@i32:i64
    set i = 0
    while i < length[vec] begin
        clear_bytes[index[vec, i], itemSize]
        set i += 1
    end
end

############ UTILITARIES

proc copy_array[
    destArray:ptr,
    sourceArray:ptr,
    len:i64,
    itemSize:i64,
    copy:proc[ptr, ptr][]
]
var i:i64,
    a, b:ptr
begin
    set i = 0
    while i < len begin
        set a = sourceArray+(itemSize:ptr*i:ptr)
        set b = destArray+(itemSize:ptr*i:ptr)

        copy[b, a]

        set i += 1
    end
end
