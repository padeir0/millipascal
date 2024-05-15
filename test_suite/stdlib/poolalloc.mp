from constants import NULL
from io import fatal
from ioutil import put_ln, put_char, put_int, put_uint, oom
from buffutil import clear_bytes, clear_words
from ptrutil import distance

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
	ERR_BOUNDS "Pointer to be freed is outside pool\n";
	ERR_ALIGN "Pointer to be freed is out of alignment\n";
	ERR_SMALLBUFF "Buffer too small (doesn't fit pool header plus a single chunk)\n";
	WTF "poolalloc: wtf\n";
end

proc init[buff:ptr, buff_size:i32, chunk_size:i32] Pool
var pool:Pool, ok:bool
begin
	if chunk_size < MIN_CHUNK_SIZE begin
		fatal[ERR_CHUNK, sizeof[ERR_CHUNK]];
	end

	if buff_size < sizeof[Pool] + chunk_size begin
		fatal[ERR_SMALLBUFF, sizeof[ERR_SMALLBUFF]];
	end

	set pool = buff:Pool;
	set pool->Begin = buff + sizeof[Pool];
	set pool->End = buff+buff_size;
	set pool->ChunkSize = chunk_size;
	set pool->Size = distance[pool->Begin, pool->End];

	clear_bytes[pool->Begin, pool->Size];
	set_list[pool];

	return pool;
end

proc alloc[p:Pool] ptr, bool
var curr:Node, position:i32
begin
	if p->Head == NULLNODE begin
		return NULL, false;
	end

	set curr = p->Head;
	set p->Head = p->Head->Next; # get the next one
	if p->Head == NULLNODE begin # was the last one
		set p->Tail = NULLNODE;
	end

	return curr:ptr, true;
end

proc free[pool:Pool, p:ptr]
var position:i32, new:Node
begin
	if not (pool->Begin <= p and p < pool->End) begin
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

proc set_list[pool:Pool]
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

proc used[pool:Pool] i64
begin
	return pool->Size:i64 - freed[pool];
end

proc freed[pool:Pool] i64
var curr:Node, total:i64, chunksize:i32
begin
	set total = 0l;
	set chunksize = pool->ChunkSize;
	set curr = pool->Head;
	while curr != NULLNODE begin
		set total += chunksize:i64;
		set curr = curr->Next;
	end
	return total;
end

proc empty[pool:Pool] bool
var total:i64
begin
	set total = freed[pool];
	if total < pool->Size:i64 begin
		return false;
	end
	if pool->Size:i64 < total begin
		fatal[WTF, sizeof[WTF]];
	end
	return true;
end
