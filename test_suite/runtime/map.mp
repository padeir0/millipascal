# each metadata object has 40 bytes, separated in the following layout:
#   - 4 bytes: size of each key (in bytes)
#   - 4 bytes: size of each value (in bytes)
#   - 8 bytes: pointer to hash procedure (proc[ptr] i64)
#   - 8 bytes: pointer to equality procedure (proc[a:ptr,b:ptr]bool)
#   - 8 bytes: pointer to copy procedure (proc[dest:ptr, source:ptr])
#   - 8 bytes: pointer to free item procedure (proc[item:ptr])
# all pointers in the above procedures are pointers to
# inside the array, that is, they're just calculated indexes
# of the array.

const begin
    METADATA_SIZE = 40

    M_KEYSIZE_OFFSET = 0
    M_VALSIZE_OFFSET = 4
    M_HASHPROC_OFFSET = 8
    M_EQPROC_OFFSET = 16
    M_COPYPROC_OFFSET = 24
    M_FREEPROC_OFFSET = 32
end 

proc set_meta[
    meta:ptr,
    keySize:i32,
    valSize:i32,
    hashProc:proc[ptr]i64,
    eqProc:proc[ptr,ptr]bool,
    copyProc:proc[ptr,ptr][],
    freeProc:proc[ptr][]
]
begin
    set (meta+M_KEYSIZE_OFFSET)@i32 = keySize
    set (meta+M_VALSIZE_OFFSET)@i32 = valSize
    set (meta+M_HASHPROC_OFFSET)@proc[ptr]i64 = hashProc
    set (meta+M_EQPROC_OFFSET)@proc[ptr,ptr]bool = eqProc
    set (meta+M_COPYPROC_OFFSET)@proc[ptr,ptr][] = copyProc
    set (meta+M_FREEPROC_OFFSET)@proc[ptr][] = freeProc
end

proc main
begin
end
