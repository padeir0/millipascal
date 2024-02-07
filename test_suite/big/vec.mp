from ioutil import put_char, put_int, debug, fatal
from constants import NULL, WORD
from dfl_alloc import alloc_f, alloc_b, free, init

# each metadata object has 24 bytes, separated in the following layout:
#   - 4 bytes: size of each item (in words)
#   - 4 bytes padding
#   - 8 bytes: pointer to equality procedure (proc[a:ptr,b:ptr]bool)
#   - 8 bytes: pointer to copy procedure (proc[dest:ptr, source:ptr])
#
# all pointers in the above procedures are pointers to
# inside the array, that is, they're just calculated indexes
# of the array.

const META_SIZE 24

const M_ITEMSIZE_OFFSET 0p
const M_COMPPROC_OFFSET 8p
const M_COPYPROC_OFFSET 16p

memory ERR_INVALID_CAP "error: length is smaller than capacity\n"

proc main
begin
    init[]
end proc

# Computes only the address of the item in the array,
# any subsequent indirections and type specific shenanigans
# shall be done externally.
# This allows us to use the same implementation even
# if the compiler chooses to inline some structures.
# Bitvectors may be implemented separatedly if necessary.
proc index[vec:ptr, index:i64] ptr
begin
    return NULL
end proc

proc new_vec[meta:ptr, length:u32, cap:u32] ptr
begin
    return NULL
end proc

# allocates a new vector; copies A and B into it
# returns the vector;
proc concat[vecA:ptr, vecB:ptr] ptr
begin
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

# just decrements the length by 1,
# everything else shall be type specific
proc pop[vec:ptr]
begin
end proc

# just increments the length by 1, may trigger allocation,
# everything else shall be type specific
proc push[vec:ptr]
begin
end proc
