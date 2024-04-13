from
	constants
import
	NULL

from
	ioutil
import
	put_ln, put_char, put_int, put_uint, debug, fatal, oom

from
	buffutil
import
	clear_bytes, clear_words

from
	ptrutil
import
	distance

export
	init, alloc, free, Pool, empty, used, freed

const begin
	NULLNODE = NULL:Node;
	MIN_CHUNK_SIZE = sizeof[Node];
end

struct Node begin
	Next:Node;
end

# we allocate this at the beginning of each pool
# in a way such that, if p:Pool then p[1] is the
# actual start of the pool, that is:
#     p->Begin == p[1]:ptr
# after the slots are set, and p->End properly aligned
# then:
#     p->Size == distance[p->Begin, p->End]
struct Pool begin
	Head, Tail:Node;
	Begin, End:ptr;
	ChunkSize, Size:i32;
end

data begin
	ERR_CHUNK "Chunk too small (minimum 8 bytes)\n";
	ERR_BOUNDS "Pointer to be freed is outside all pools\n";
	ERR_ALIGN "Pointer to be freed is out of alignment\n";
	ERR_SMALLBUFF "Buffer too small (doesn't fit pool header plus a single chunk)\n";
	ERR_TOOMANYPOOLS "Too many pools\n";
end

proc init[buff:ptr, buffsize:i32, chunksize:i32] Pool
var pool:Pool, ok:bool
begin
	if chunksize < MIN_CHUNK_SIZE begin
		fatal[ERR_CHUNK, sizeof[ERR_CHUNK]];
	end

	if buffsize < sizeof[Pool] + chunksize begin
		fatal[ERR_SMALLBUFF, sizeof[ERR_SMALLBUFF]];
	end

	set pool = buff:Pool;
	set pool->Begin = buff + sizeof[Pool];
	set pool->End = buff+buffsize;
	set pool->ChunkSize = chunksize;
	set pool->Size = distance[pool->Begin, pool->End];

	clear_bytes[pool->Begin, pool->Size];
	setList[pool];
	insert[pool];

	return pool;
end

struct IA32 begin
	Num:i32;
end

struct PtrPool begin
	Pool:Pool;
end

const MAXPOOLS = 8

data begin
	Top:IA32 {0};
	PoolTable:PtrPool [MAXPOOLS];
end

# when inserting, if the new pool overlaps with an old pool,
# then the older one gets overwritten with the new pool
# leave them unordered, it shouldn't matter that much.
proc insert[p:Pool]
var i:i32, cmp:i16
begin
	set i = 0;
	while i < Top->Num begin
		set cmp = comp[PoolTable[i]->Pool, p];

		if cmp == INTERSECT begin
			set PoolTable[i]->Pool = p;
			return;
		end

		set i++;
	end

	if Top->Num >= MAXPOOLS begin
		fatal[ERR_TOOMANYPOOLS, sizeof[ERR_TOOMANYPOOLS]];
	end

	set PoolTable[Top->Num]->Pool = p;
	set Top->Num++;
end

const begin
	PRECEED = 1s;
	INTERSECT = 2s;
	SUCEED = 4s;
end

data wtf "wtf!\n"

proc comp[a, b:Pool] i16
begin
	# sanity check, can be removed :)
	if a->Begin >= a->End or b->Begin >= b->End begin
		fatal[wtf, sizeof[wtf]];
		exit 1ss;
	end

	if a->End < b->Begin begin
		return PRECEED;
	end
	if b->End < a->Begin begin
		return SUCEED;
	end

	return INTERSECT;
end

proc findPool[p:ptr] Pool, bool
var i:i32, curr:Pool
begin
	set i = 0;
	while i < Top->Num begin
		set curr = PoolTable[i]->Pool;
		if curr->Begin <= p and p < curr->End begin
			return curr, true;
		end

		set i++;
	end
	return NULL:Pool, false;
end

proc alloc[p:Pool] ptr
var curr:Node, position:i32
begin
	if p->Head == NULLNODE
	begin
		oom[];
		exit 1ss;
	end

	set curr = p->Head;
	set p->Head = p->Head->Next; # get the next one
	if p->Head == NULLNODE begin # was the last one
		set p->Tail = NULLNODE;
	end

	return curr:ptr;
end

proc free[p:ptr]
var position:i32, new:Node, pool:Pool, ok:bool,
begin
	set pool, ok = findPool[p];
	if not ok begin
		fatal[ERR_BOUNDS, sizeof[ERR_BOUNDS]];
	end

	if distance[p, pool->Begin] % pool->ChunkSize != 0
	begin
		fatal[ERR_ALIGN, sizeof[ERR_ALIGN]];
	end

	set new = p:Node;
	set new->Next = NULLNODE;

	if pool->Head == NULLNODE begin
		set pool->Head = new;
		set pool->Tail = new;
		return;
	end

	set pool->Tail->Next = new;
	set pool->Tail = new;
end

proc setList[pool:Pool]
var curr:Node, End:Node
begin
	set curr = pool->Begin:Node;
	# we need this because of alignment, the chunks may not align
	# and leave a padding at the end of the buffer
	set End = pool->End:Node - pool->ChunkSize;

	set pool->Head = curr;
	while curr < End begin
		set curr->Next = curr + pool->ChunkSize;
		set curr = curr->Next;
	end
	# curr is at the edge of the buffer, and may not be valid
	# in case the end is not aligned, we leave padding
	if curr:ptr + pool->ChunkSize != pool->End begin
		set pool->End = curr:ptr;
		set curr -= pool->ChunkSize;
		set pool->Size = distance[pool->Begin, pool->End];
	end

	set curr->Next = NULLNODE;
	set pool->Tail = curr;
end

proc used[pool:Pool] i32
begin
	return pool->Size - freed[pool];
end

proc freed[pool:Pool] i32
var curr:Node, total, chunksize:i32
begin
	set total = 0;
	set chunksize = pool->ChunkSize;
	set curr = pool->Head;
	while curr != NULLNODE begin
		set total += chunksize;
		set curr = curr->Next;
	end
	return total;
end

proc empty[pool:Pool] bool
var total:i32
begin
	set total = freed[pool];
	if total < pool->Size begin
		return false;
	end
	if pool->Size < total begin
		fatal[wtf, sizeof[wtf]];
	end
	return true;
end
