from ioutil import put_char, put_int, debug
export Init, Alloc, Free

memory Pool 8192 # 8KB
memory Head 8
memory ChunkSize 8

memory OOM "Out of memory\n"
memory ERRCHUNK "Chunk too small (minimum 8 bytes)\n"
memory ERRBOUNDS "Pointer to be freed is out of bounds\n"
memory ERRALIGN "Pointer to be freed is out of alignment\n"

memory test 128

proc main
var a:ptr, i:i64
begin
	debug[]
	Init[16];
	debug[]
	set i = 0
	while i < 16
	begin
		set (test + (i*8):ptr)@ptr = Alloc[]
		set i += 1
	end while

	debug[]

	set i = 0
	while i < 16
	begin
		Free[(test + (i*8):ptr)@ptr]
		set i += 1
	end while
end proc

proc Init[chunksize:i64]
begin
	if chunksize < 8
	begin
		fatal[ERRCHUNK, ERRCHUNK.size]
	end if

	set ChunkSize@i64 = chunksize
	set Head@ptr = Pool
	clear[Pool, Pool.size]

	debug[]
	setList[Pool, Pool.size, chunksize]
	debug[]
end proc

proc Alloc[] ptr
var curr:ptr
begin
	if Head@ptr == ~1p
	begin
		fatal[OOM, OOM.size]
	end if

	set curr = Head@ptr
	set Head@ptr = (Head@ptr)@ptr # get the next one

	return curr
end proc

proc Free[p:ptr]
var curr:ptr
begin
	if p < Pool or p > (Pool + Pool.size:ptr)
	begin
		fatal[ERRBOUNDS, ERRBOUNDS.size]
	end if

	if (p-Pool):i64 % ChunkSize@i64 != 0
	begin
		fatal[ERRALIGN, ERRALIGN.size]
	end if

	set p@ptr = ~1p; # set header as nil
	if Head@ptr == ~1p
	begin
		set Head@ptr = p
		return;
	end if

	set curr = Head@ptr
	while curr@ptr != ~1p
	begin
		set curr = curr@ptr
	end while
	set curr@ptr = p;
end proc

proc clear[start:ptr, size]
var p:ptr, i
begin
	set p = start
	set i = 0
	while i < size
	begin
		set (p+i:ptr)@i64 = 0
		set i += 8
	end while
end proc

proc setList[start:ptr, size, chunksize]
var i:i64
begin
	set i = 0
	while i < size
	begin
		set (start+i:ptr)@ptr = start + (i+chunksize):ptr
		put_int[i];
		put_char['\n'];
		# set i += chunksize # TODO: not working
		set i = i + chunksize
	end while
	set i -= chunksize
	set (start+i:ptr)@ptr = ~1p # set the last one null
end proc

proc fatal[p:ptr, size]
begin
	write[p, size]
	exit 1ss
end proc
