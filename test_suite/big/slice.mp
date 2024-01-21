from ioutil import put_char, put_int, debug, fatal
from constants import NULL, WORD
from dfl_alloc import AllocFront, AllocBack, Free, Init,
       B_HEADER_SIZE,
       F_OBJ_SIZE_OFFSET, F_OBJ_LEN_OFFSET, F_OBJ_CAP_OFFSET,
       F_OBJ_PROP_OFFSET, F_OBJ_META_OFFSET

# Cada objeto de metadados tem 24 bytes, separados da seguinte forma:
#   - 4 bytes: tamanho de cada item (words)
#   - 1 byte:  caso os items tenham headers (se tiverem, será igual o B_HEADER)
#   - 3 bytes sobrando
#   - 8 bytes: ponteiro para procedimento de comparação (proc[a:ptr,b:ptr]bool)
#   - 8 bytes: ponteiro para procedimento de cópia (proc[dest:ptr, source:ptr])

const META_SIZE 24

const M_ITEMSIZE_OFFSET 0p
const M_HASHEADER_OFFSET 4p
const M_COMPPROC_OFFSET 8p
const M_COPYPROC_OFFSET 16p

memory ERR_INVALID_CAP "error: length is smaller than capacity\n"

proc main
var slice:ptr
begin
    Init[]
    setmetadata[]
    set slice = MakeSlice[test_meta, 8ul, 8ul]
    initIntSlice[slice]
    printIntSlice[slice]
end proc

proc initIntSlice[slice:ptr]
var meta:ptr, itemsize:u32, item:ptr, max:ptr, i,
    length:u32
begin
    set meta = (slice-F_OBJ_META_OFFSET)@ptr
    set length = (slice-F_OBJ_LEN_OFFSET)@u32
    set itemsize = (meta+M_ITEMSIZE_OFFSET)@u32
    set item = slice

    if (meta+M_HASHEADER_OFFSET)@bool begin
        set item += B_HEADER_SIZE:ptr
    end if

    set max = slice + (itemsize * length):ptr
    set i = 0
    while item < max begin
        set (item)@i64 = i
        set i += 1
        set item += itemsize:ptr
    end while
end proc

proc printIntSlice[slice:ptr]
var meta:ptr, itemsize:u32, item:ptr, max:ptr,
    length:u32
begin
    set meta = (slice-F_OBJ_META_OFFSET)@ptr
    set length = (slice-F_OBJ_LEN_OFFSET)@u32
    set itemsize = (meta+M_ITEMSIZE_OFFSET)@u32
    set item = slice

    if (meta+M_HASHEADER_OFFSET)@bool begin
        set item += B_HEADER_SIZE:ptr
    end if

    set max = slice + (itemsize * length):ptr
    while item < max begin
        put_int[item@i64]
        put_char['\n']
        set item += itemsize:ptr
    end while
end proc

proc MakeSlice[meta:ptr, length:u32, cap:u32] ptr
var itemsize:u32, bytesize
begin
    if length > cap begin
        fatal[ERR_INVALID_CAP, ERR_INVALID_CAP.size]
        return NULL
    end if
    set itemsize = (meta+M_ITEMSIZE_OFFSET)@u32 << 3ul
    set bytesize = (cap * itemsize):i64
    return AllocFront[bytesize, length, cap, 0ul,  meta]
end proc

proc Concat[a:ptr, b:ptr] ptr
var lenA:u32, lenB:u32, itemsize:u32, newsize, meta:ptr,
    copy:proc[ptr, ptr], p:ptr, 
    p_dest:ptr, p_source:ptr, p_source_max:ptr, hasHeader:bool
begin
    set lenA = (a-F_OBJ_LEN_OFFSET)@u32
    set lenB = (b-F_OBJ_LEN_OFFSET)@u32

    # meta é igual para os dois
    set meta = (a-F_OBJ_META_OFFSET)@ptr
    set itemsize = (meta+M_ITEMSIZE_OFFSET)@u32 << 3ul
    set hasHeader = (meta+M_HASHEADER_OFFSET)@bool
    set copy = (meta+M_COPYPROC_OFFSET)@proc[ptr, ptr]

    set newsize = ((lenA * itemsize) + (lenB * itemsize)):i64

    set p = AllocFront[newsize, lenA+lenB, lenA+lenB, 0ul,  meta]

    set p_dest = p + B_HEADER_SIZE:ptr
    set p_source_max = a + (lenA * itemsize):ptr
    set p_source = a
    if hasHeader begin
        set p_dest += B_HEADER_SIZE:ptr
        set p_source += B_HEADER_SIZE:ptr
        set p_source_max += B_HEADER_SIZE:ptr
    end if
    while p_source < p_source_max begin
        copy[p_dest, p_source]
        set p_source += itemsize:ptr
        set p_dest += itemsize:ptr
    end while

    set p_source_max = b + (lenB * itemsize):ptr
    set p_source = b
    if hasHeader begin
        set p_source += B_HEADER_SIZE:ptr
        set p_source_max += B_HEADER_SIZE:ptr
    end if
    while p_source < p_source_max begin
        copy[p_dest, p_source]
        set p_source += itemsize:ptr
        set p_dest += itemsize:ptr
    end while

    return p
end proc

proc Comp[a:ptr, b:ptr] bool
var meta:ptr, cmp:proc[ptr, ptr]bool,
    lenA:u32, lenB:u32, itemsize:u32,
    a_max:ptr, hasHeader:bool
begin
    set lenA = (a-F_OBJ_LEN_OFFSET)@u32
    set lenB = (b-F_OBJ_LEN_OFFSET)@u32

    if lenA != lenB begin
        return false
    end if
    
    set meta = (a-F_OBJ_META_OFFSET)@ptr
    set cmp = (meta+M_COMPPROC_OFFSET)@proc[ptr, ptr]bool
    set itemsize = (meta+M_ITEMSIZE_OFFSET)@u32

    set a_max = a + (lenA * itemsize):ptr
    if hasHeader begin
        set a += B_HEADER_SIZE:ptr
        set b += B_HEADER_SIZE:ptr
    end if
    while a < a_max begin
        if not cmp[a, b] begin
            return false
        end if
        set a += itemsize:ptr
        set b += itemsize:ptr
    end while

    return true
end proc

memory test_meta 24

proc setmetadata
begin
    set (test_meta+M_ITEMSIZE_OFFSET)@u32 = 1ul
    set (test_meta+M_HASHEADER_OFFSET)@bool = false
    set (test_meta+M_COMPPROC_OFFSET)@proc[ptr, ptr]bool = cmpInt
    set (test_meta+M_COPYPROC_OFFSET)@proc[ptr, ptr] = copyInt
end proc

proc cmpInt[a:ptr, b:ptr] bool
begin
    return a@i64 == b@i64
end proc

proc copyInt[dest:ptr, source:ptr]
begin
    set dest@i64 = source@i64
end proc
