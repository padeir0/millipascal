from
	ioutil
import
	put_char, put_int, debug, fatal, oom

from
	buffutil
import
	clear_bytes, clear_words

export
	init, alloc, free

const NULL = 0p
const MIN_CHUNK_SIZE = 8

data Pool [1<<10] # 1KB
data Head [sizeof ptr]
data Tail [sizeof ptr]
data ChunkSize [sizeof i64]

const FULL = 1ss
const EMPTY = 0ss
data ByteMap [Pool.size/MIN_CHUNK_SIZE]

data ERR_CHUNK "Chunk too small (minimum 8 bytes)\n"
data ERR_BOUNDS "Pointer to be freed is out of bounds\n"
data ERR_ALIGN "Pointer to be freed is out of alignment\n"

const NUM_POINTERS = 64
data Test [NUM_POINTERS * sizeof ptr]

proc main
var a:ptr, i:i64, out:ptr
begin
	init[16];
	set i = 0
	while i < NUM_POINTERS
	begin
		set out = alloc[]
		set (Test + (i*sizeof ptr):ptr)@ptr = out
		set i += 1
	end

	set i = 0
	while i < NUM_POINTERS
	begin
		if i % 2 == 0
		begin
			free[(Test + (i*sizeof ptr):ptr)@ptr]
		end
		set i += 1
	end

	set i = NUM_POINTERS-1
	while 0 <= i
	begin
		if i % 2 != 0
		begin
			free[(Test + (i*sizeof ptr):ptr)@ptr]
		end
		set i -= 1
	end
end

proc init[chunksize:i64]
begin
	if chunksize < MIN_CHUNK_SIZE
	begin
		fatal[ERR_CHUNK, ERR_CHUNK.size]
	end

	set ChunkSize@i64 = chunksize
	set Head@ptr = Pool
	clear_words[Pool, Pool.size]

	setList[Pool, Pool.size, chunksize]
	clear_bytes[ByteMap, ByteMap.size]
end

proc alloc[] ptr
var curr, position:ptr
begin
	if Head@ptr == NULL
	begin
		oom[]
	end

	set curr = Head@ptr
	set Head@ptr = (Head@ptr)@ptr # get the next one

	set position = (curr - Pool) / (ChunkSize@ptr)
	set (ByteMap + position)@i8 = FULL

	return curr
end

proc free[p:ptr]
var position:ptr
begin
	if p < Pool or p > (Pool + Pool.size:ptr)
	begin
		fatal[ERR_BOUNDS, ERR_BOUNDS.size]
	end

	if (p-Pool):i64 % ChunkSize@i64 != 0
	begin
		fatal[ERR_ALIGN, ERR_ALIGN.size]
	end

	set p@ptr = NULL
	set position = (p - Pool) / (ChunkSize@ptr)
	set (ByteMap + position)@i8 = EMPTY

	if Head@ptr == NULL
	begin
		set Head@ptr = p
		return;
	end

	set (Tail@ptr)@ptr = p
	set Tail@ptr = p
end

proc print_map[]
var i:i64
begin
	set i = 0
	put_char['[']
	while i < Pool.size/(ChunkSize@i64)
	begin
		if (ByteMap+i:ptr)@i8 == EMPTY
		begin
			put_char[' ']
		end
		if (ByteMap+i:ptr)@i8 == FULL
		begin
			put_char['X']
		end
		set i += 1
	end
	put_char[']']
	put_char['\n']
end

proc setList[start:ptr, size, chunksize:i64]
var i:i64
begin
	set i = 0
	while i < size
	begin
		set (start+i:ptr)@ptr = start + (i+chunksize):ptr
		set i += chunksize
	end
	set i -= chunksize
	set (start+i:ptr)@ptr = NULL
	set Tail@ptr = (start+i:ptr)
end
