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
data Head [sizeof[ptr]]
data Tail [sizeof[ptr]]
data ChunkSize [sizeof[i64]]

const FULL = 1ss
const EMPTY = 0ss
data ByteMap [sizeof[Pool]/MIN_CHUNK_SIZE]

data ERR_CHUNK "Chunk too small (minimum 8 bytes)\n"
data ERR_BOUNDS "Pointer to be freed is out of bounds\n"
data ERR_ALIGN "Pointer to be freed is out of alignment\n"

const NUM_POINTERS = 64
data Test [NUM_POINTERS * sizeof[ptr]]

proc main
var a:ptr, i:i64, out:ptr
begin
	init[16];
	set i = 0;
	while i < NUM_POINTERS
	begin
		set out = alloc[];
		set (Test + (i*sizeof[ptr]))@ptr = out;
		set i += 1;
	end

	set i = 0;
	while i < NUM_POINTERS
	begin
		if i % 2 == 0
		begin
			free[(Test + (i*sizeof[ptr]))@ptr];
		end
		set i += 1;
	end

	set i = NUM_POINTERS-1;
	while 0 <= i
	begin
		if i % 2 != 0
		begin
			free[(Test + (i*sizeof[ptr]))@ptr];
		end
		set i -= 1;
	end
end

proc init[chunksize:i64]
begin
	if chunksize < MIN_CHUNK_SIZE
	begin
		fatal[ERR_CHUNK, sizeof[ERR_CHUNK]];
	end

	set ChunkSize@i64 = chunksize;
	set Head@ptr = Pool;
	clear_words[Pool, sizeof[Pool]];

	setList[Pool, sizeof[Pool], chunksize];
	clear_bytes[ByteMap, sizeof[ByteMap]];
end

proc alloc[] ptr
var curr:ptr, position:i64
begin
	if Head@ptr == NULL
	begin
		oom[];
	end

	set curr = Head@ptr;
	set Head@ptr = (Head@ptr)@ptr; # get the next one

	set position = (curr:i64 - Pool:i64) / (ChunkSize@i64);
	set (ByteMap + position)@i8 = FULL;

	return curr;
end

proc free[p:ptr]
var position:i64
begin
	if p < Pool or p > (Pool + sizeof[Pool])
	begin
		fatal[ERR_BOUNDS, sizeof[ERR_BOUNDS]];
	end

	if (p-Pool:i64):i64 % ChunkSize@i64 != 0
	begin
		fatal[ERR_ALIGN, sizeof[ERR_ALIGN]];
	end

	set p@ptr = NULL;
	set position = (p:i64 - Pool:i64) / (ChunkSize@i64);
	set (ByteMap + position)@i8 = EMPTY;

	if Head@ptr == NULL
	begin
		set Head@ptr = p;
		return;
	end

	set (Tail@ptr)@ptr = p;
	set Tail@ptr = p;
end

proc print_map[]
var i:i64
begin
	set i = 0;
	put_char['['];
	while i < sizeof[Pool]/ChunkSize@i64
	begin
		if (ByteMap+i)@i8 == EMPTY
		begin
			put_char[' '];
		end
		if (ByteMap+i)@i8 == FULL
		begin
			put_char['X'];
		end
		set i += 1;
	end
	put_char[']'];
	put_char['\n'];
end

proc setList[start:ptr, size, chunksize:i64]
var i:i64
begin
	set i = 0;
	while i < size
	begin
		set (start+i)@ptr = start + (i+chunksize);
		set i += chunksize;
	end
	set i -= chunksize;
	set (start+i)@ptr = NULL;
	set Tail@ptr = (start+i);
end
