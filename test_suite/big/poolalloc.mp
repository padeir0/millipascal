from
	ioutil
import
	put_char, put_int, debug, fatal, oom

from
	buffutil
import
	clear_bytes, clear_words

export
	Init, Alloc, Free

const NULL 0p
const MIN_CHUNK_SIZE 8

memory Pool 1024 # 8KB
memory Head 8
memory Tail 8
memory ChunkSize 8

const FULL  1ss
const EMPTY 0ss
memory ByteMap 128 # Pool.size/MIN_CHUNK_SIZE

memory ERRCHUNK "Chunk too small (minimum 8 bytes)\n"
memory ERRBOUNDS "Pointer to be freed is out of bounds\n"
memory ERRALIGN "Pointer to be freed is out of alignment\n"

memory test 512

proc main
var a:ptr, i, out:ptr
begin
	Init[16];
	set i = 0
	while i < 64
	begin
		set out = Alloc[]
		set (test + (i*8):ptr)@ptr = out
		set i += 1
	end while

	set i = 0
	while i < 64
	begin
		if i % 2 == 0
		begin
			Free[(test + (i*8):ptr)@ptr]
		end if
		set i += 1
	end while

	set i = 63
	while 0 <= i
	begin
		if i % 2 != 0
		begin
			Free[(test + (i*8):ptr)@ptr]
		end if
		set i -= 1
	end while
end proc

proc Init[chunksize]
begin
	if chunksize < MIN_CHUNK_SIZE
	begin
		fatal[ERRCHUNK, ERRCHUNK.size]
	end if

	set ChunkSize@i64 = chunksize
	set Head@ptr = Pool
	clear_words[Pool, Pool.size]

	setList[Pool, Pool.size, chunksize]
	clear_bytes[ByteMap, ByteMap.size]
end proc

proc Alloc[] ptr
var curr:ptr, position:ptr
begin
	if Head@ptr == NULL
	begin
		oom[]
	end if

	set curr = Head@ptr
	set Head@ptr = (Head@ptr)@ptr # get the next one

	set position = (curr - Pool) / (ChunkSize@ptr)
	set (ByteMap + position)@i8 = FULL

	return curr
end proc

proc Free[p:ptr]
var position:ptr
begin
	if p < Pool or p > (Pool + Pool.size:ptr)
	begin
		fatal[ERRBOUNDS, ERRBOUNDS.size]
	end if

	if (p-Pool):i64 % ChunkSize@i64 != 0
	begin
		fatal[ERRALIGN, ERRALIGN.size]
	end if

	set p@ptr = NULL
	set position = (p - Pool) / (ChunkSize@ptr)
	set (ByteMap + position)@i8 = EMPTY

	if Head@ptr == NULL
	begin
		set Head@ptr = p
		return;
	end if

	set (Tail@ptr)@ptr = p
	set Tail@ptr = p
end proc

proc PrintMap[]
var i
begin
	set i = 0
	put_char['[']
	while i < Pool.size/(ChunkSize@i64)
	begin
		if (ByteMap+i:ptr)@i8 == EMPTY
		begin
			put_char[' ']
		end if
		if (ByteMap+i:ptr)@i8 == FULL
		begin
			put_char['X']
		end if
		set i += 1
	end while
	put_char[']']
	put_char['\n']
end proc

proc setList[start:ptr, size, chunksize]
var i
begin
	set i = 0
	while i < size
	begin
		set (start+i:ptr)@ptr = start + (i+chunksize):ptr
		set i += chunksize
	end while
	set i -= chunksize
	set (start+i:ptr)@ptr = NULL
	set Tail@ptr = (start+i:ptr)
end proc
