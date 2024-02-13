# utf-8 string representation
# _NEEDS_ garbage collection, since the underlying array
# _WILL_ be referenced by multiple string headers.

from ioutil import put_char, put_ln, put_int, put_bool, debug, fatal
from constants import NULL, WORD
from dfl_alloc import alloc_f, alloc_b, free, init
from utf8 import utf8_decode, utf8_encode

export
    new_str, free_str, _free_buff,
    set_str,
    put_str, put_str_debug,
    concat, eq_str, copy_str, dup_str,
    peek_rune, peek, pop_rune, pop,
    hash, length

# 16 bytes
# - 8 bytes pointer to underlying byte array
# - 4 bytes i32 index to start of string
# - 4 bytes i32 byte length of string

const STR_HEADER_SIZE 16

# positive offset
const STR_PTR_OFFSET 0p   # ptr
const STR_START_OFFSET 8p # i32
const STR_SIZE_OFFSET 12p  # i32

proc main
begin
end proc

# pointer in the first word
const STR_BITMAP 0b10000000_00000000_00000000_00000000ul

# creates an empty string
proc new_str[] ptr
var array:ptr, header:ptr
begin
    set header = alloc_b[STR_HEADER_SIZE, 0uss, 0us, STR_BITMAP]
    set (header+STR_PTR_OFFSET)@ptr = NULL
    set (header+STR_START_OFFSET)@i32 = 0l
    set (header+STR_SIZE_OFFSET)@i32 = 0l

    return header
end proc

# we do not free the underlying array,
# since it may be shared. let the gc handle this.
proc free_str[str:ptr]
begin
    free[str]
end proc

proc _free_buff[str:ptr]
begin
    free[(str+STR_PTR_OFFSET)@ptr]
end proc

proc set_str[str:ptr, mem:ptr, mem_size]
var array:ptr, header:ptr
begin
    set (str+STR_PTR_OFFSET)@ptr = mem
    set (str+STR_START_OFFSET)@i32 = 0l
    set (str+STR_SIZE_OFFSET)@i32 = mem_size:i32
end proc

proc put_str[str:ptr]
var array:ptr, start:i32, size:i32
begin
    set array = (str+STR_PTR_OFFSET)@ptr
    set start = (str+STR_START_OFFSET)@i32
    set size = (str+STR_SIZE_OFFSET)@i32

    write[(array+start:ptr), size:i64]
end proc

memory NULL_STR "NULL"

proc put_str_debug[str:ptr]
begin
    if str == NULL begin
        write[NULL_STR, NULL_STR.size]
        return
    end if
    put_char['"']
    put_str[str]
    put_char['"']
end proc

# allocates a new str; copies A and B into it
# returns the str;
proc concat[strA:ptr, strB:ptr] ptr
var outStr:ptr, 
    sizeA:i32, sizeB:i32, sizeOut:i32,
    startA:i32, startB:i32,
    arrayOut:ptr, arrayA:ptr, arrayB:ptr
begin
    set startA = (strA+STR_START_OFFSET)@i32
    set startB = (strB+STR_START_OFFSET)@i32

    set sizeA = (strA+STR_SIZE_OFFSET)@i32
    set sizeB = (strB+STR_SIZE_OFFSET)@i32

    set arrayA = (strA+STR_PTR_OFFSET)@ptr
    set arrayB = (strB+STR_PTR_OFFSET)@ptr

    set sizeOut = sizeA + sizeB
    set outStr = new_str[]

    if sizeOut > 0l begin
        set arrayOut = alloc_f[sizeOut:i64]
    end else begin
        set arrayOut = NULL
    end if
    
    if arrayA != NULL begin # empty string is NULL
        copy[arrayOut, (arrayA+startA:ptr), sizeA:i64]
    end if
    if arrayB != NULL begin
        copy[arrayOut+sizeA:ptr, (arrayB+startB:ptr), sizeB:i64]
    end if

    set (outStr+STR_PTR_OFFSET)@ptr = arrayOut
    set (outStr+STR_START_OFFSET)@i32 = 0l
    set (outStr+STR_SIZE_OFFSET)@i32 = sizeOut

    return outStr
end proc

proc eq_str[strA:ptr, strB:ptr] bool
var sizeA:i32, sizeB:i32,
    startA:i32, startB:i32,
    arrayA:ptr, arrayB:ptr,
    i:i32
begin
    set startA = (strA+STR_START_OFFSET)@i32
    set startB = (strB+STR_START_OFFSET)@i32

    set arrayA = (strA+STR_PTR_OFFSET)@ptr
    set arrayB = (strB+STR_PTR_OFFSET)@ptr

    set sizeA = (strA+STR_SIZE_OFFSET)@i32
    set sizeB = (strB+STR_SIZE_OFFSET)@i32

    if sizeA != sizeB begin
        return false
    end if

    # empty strigns fall here too
    if arrayA == arrayB and
       sizeA == sizeB and
       startA == startB begin
        return true
    end if

    set i = 0l
    while i < sizeA begin
        if (arrayA+i:ptr+startA:ptr)@i8 !=
           (arrayB+i:ptr+startB:ptr)@i8 begin
            return false
        end if
        set i += 1l
    end while
    return true
end proc

proc copy_str[strDest:ptr, strSource:ptr]
begin
    set (strDest+STR_START_OFFSET)@i32 = (strSource+STR_START_OFFSET)@i32
    set (strDest+STR_PTR_OFFSET)@i32 = (strSource+STR_PTR_OFFSET)@i32
    set (strDest+STR_SIZE_OFFSET)@i32 = (strSource+STR_SIZE_OFFSET)@i32
end proc

proc dup_str[strSource:ptr] ptr
var strDest:ptr
begin
    set strDest = new_str[]
    copy_str[strDest, strSource]
    return strDest
end proc

proc peek_rune[str:ptr] i32
var array:ptr, start:i32, size:i32,
    rune:i32, runeSize
begin
    set array = (str+STR_PTR_OFFSET)@ptr
    set start = (str+STR_START_OFFSET)@i32
    set size = (str+STR_SIZE_OFFSET)@i32

    if size == 0l or array == NULL begin
        return ~1l
    end if

    set rune, runeSize = utf8_decode[array+start:ptr]

    return rune
end proc

# peek allocates a new header only,
# the underlying array is the same,
# strings are immutable
proc peek[str:ptr, length] ptr
var array:ptr, size:i32,
    old_start:i32, new_start:i32,
    outStr:ptr,
    rune:i32, runeSize, i
begin
    set array = (str+STR_PTR_OFFSET)@ptr
    set old_start = (str+STR_START_OFFSET)@i32
    set size = (str+STR_SIZE_OFFSET)@i32

    if size == 0l or array == NULL begin
        return NULL
    end if

    set i = 0
    set new_start = old_start
    while i < length begin
        if size <= 0l begin
            return NULL
        end if
        set rune, runeSize = utf8_decode[array+new_start:ptr]
        set size -= runeSize:i32
        set new_start += runeSize:i32
        set i += 1
    end while

    set outStr = new_str[]
    set (outStr+STR_PTR_OFFSET)@ptr = array
    set (outStr+STR_START_OFFSET)@i32 = old_start
    set (outStr+STR_SIZE_OFFSET)@i32 = new_start-old_start
    return outStr
end proc

proc pop_rune[str:ptr] i32
var array:ptr, start:i32, size:i32,
    rune:i32, runeSize
begin
    set array = (str+STR_PTR_OFFSET)@ptr
    set start = (str+STR_START_OFFSET)@i32
    set size = (str+STR_SIZE_OFFSET)@i32

    if size == 0l or array == NULL begin
        return ~1l
    end if

    set rune, runeSize = utf8_decode[array+start:ptr]

    set (str+STR_START_OFFSET)@i32 += runeSize:i32
    set (str+STR_SIZE_OFFSET)@i32 -= runeSize:i32

    return rune
end proc

# pop allocates a new header,
# but modifies the older one so that
# the substring returned is "popped"
# from the beginning of the older string
proc pop[str:ptr, length] ptr
var array:ptr, size:i32,
    old_start:i32, new_start:i32,
    outStr:ptr,
    rune:i32, runeSize, i
begin
    set array = (str+STR_PTR_OFFSET)@ptr
    set old_start = (str+STR_START_OFFSET)@i32
    set size = (str+STR_SIZE_OFFSET)@i32

    if size == 0l or array == NULL begin
        return NULL
    end if

    set i = 0
    set new_start = old_start
    while i < length begin
        if size <= 0l begin
            return NULL
        end if
        set rune, runeSize = utf8_decode[array+new_start:ptr]
        set size -= runeSize:i32
        set new_start += runeSize:i32
        set i += 1
    end while

    set (str+STR_START_OFFSET)@i32 = new_start
    set (str+STR_SIZE_OFFSET)@i32 = size

    set outStr = new_str[]
    set (outStr+STR_PTR_OFFSET)@ptr = array
    set (outStr+STR_START_OFFSET)@i32 = old_start
    set (outStr+STR_SIZE_OFFSET)@i32 = new_start-old_start
    return outStr
end proc

# Murmur Hash
proc hash[str:ptr] i64
var array:ptr, start:i32, size:i32,
    i:i32, out
begin
    set array = (str+STR_PTR_OFFSET)@ptr
    set start = (str+STR_START_OFFSET)@i32
    set size = (str+STR_SIZE_OFFSET)@i32

    set out = 0xCAFEBABE
    set i = 0l
    while i < size begin
        set out = (out |^ (array+(start+i):ptr)@i8:i64) * 0x5bd1e995
        set out = out |^ (out >> 15)
        set i += 1l
    end while

    return out
end proc

# scans the entire string and counts
# the number of utf-8 codepoints
proc length[str:ptr] i64
var array:ptr, size:i32, start:i32,
    rune:i32, runeSize, i
begin
    set array = (str+STR_PTR_OFFSET)@ptr
    set start = (str+STR_START_OFFSET)@i32
    set size = (str+STR_SIZE_OFFSET)@i32

    if size == 0l or array == NULL begin
        return 0 # empty string
    end if

    set i = 0
    while 0l < size begin
        set rune, runeSize = utf8_decode[array+start:ptr]
        set size -= runeSize:i32
        set start += runeSize:i32
        set i += 1
    end while

    return i
end proc

################################# INTERNAL UTILITARIES

proc copy[dest:ptr, source:ptr, source_size]
var i
begin
	set i = 0
	while i < source_size
	begin
		set (dest + i:ptr)@i8 = (source + i:ptr)@i8
		set i += 1
	end while
end proc
