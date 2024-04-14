from io import print, fatal
from buffutil import clear_bytes, clear_words
from ioutil import put_bool, put_int, put_uint, put_char, put_ln, debug, oom
from constants import WORD, NULL
from ptrutil import distance

export init, alloc, free, Heap, used, freed, empty, print_list

const begin
    NULLNODE = NULL:Node;
	NULLOBJ = NULL:Obj;
end

data begin
	ERR_BOUNDS "Pointer is outside all heaps\n";
	ERR_ALIGN "Pointer is out of alignment\n";
	ERR_SMALLBUFF "Buffer too small (doesn't fit header plus a single node)\n";
	ERR_TOOMANYHEAPS "Too many heaps\n";
end

# object header
struct Obj [sizeof[i64]] begin
    Size:i64 {~sizeof[i64]};
end

struct Node begin
    Size:i64;
    Next:Node;
end

struct Heap begin
    Head:Node;
    Begin, End:ptr;
    Size:i64;
end

proc init[buff:ptr, buff_size:i32] Heap
var h:Heap
begin
    if buff_size < sizeof[Heap]+sizeof[Node] begin
        fatal[ERR_SMALLBUFF, sizeof[ERR_SMALLBUFF]];
    end

    set h = buff:Heap;
    set h->Head = h[1]:Node;
    set h->Head->Size = (buff_size - sizeof[Heap]):i64;
    set h->Head->Next = NULLNODE;

    set h->Begin = h->Head:ptr;
    set h->End = buff+buff_size;
    
    set h->Size = distance[h->Begin, h->End]:i64;
	insert[h];
    return h;
end

proc alloc[h:Heap, size:i32] ptr
var p:ptr, alloc_size:i64
begin
	set size += sizeof[Obj];
	# alignment
	if size%WORD != 0 begin
		set size = size + (WORD-size%WORD);
	end

    # objects need space for a Node when deallocated
	if size < sizeof[Node] begin
		set size = sizeof[Node];
	end

	set p, alloc_size = get_node[h, size:i64];
	if p == NULL begin
		return NULL;
	end
	set p += sizeof[Obj];
    set (p:Obj)->Size = alloc_size;
	return p;
end

proc get_node[h:Heap, size:i64] ptr, i64
var curr, prev:Node
begin
	if h->Head == NULLNODE begin
		return NULL, 0l;
	end

	set curr = h->Head;
	set prev = NULLNODE;

	while curr != NULLNODE begin
		if curr->Size == size begin
			return pop[h, prev, curr], size;
		end
		if curr->Size > size begin
            # if we allocate an object and the remaining
            # size is not sufficient for a node,
            # we allocate the full space, without splitting
			if curr->Size - size < sizeof[Node]:i64 begin
				return pop[h, prev, curr], curr->Size;
			end
			return split[h, prev, curr, size], size;
		end

		set prev = curr;
		set curr = curr->Next;
	end

	return NULL, 0l;
end

proc pop[h:Heap, prev, curr:Node] ptr
begin
	if prev != NULLNODE begin
		set prev->Next = curr->Next;
	end else begin
        set h->Head = curr->Next;
	end
	return curr:ptr;
end

proc split[h:Heap, prev, curr:Node, requested_size:i64] ptr
var newnode:Node
begin
	set newnode = curr + requested_size;
	set newnode->Size = curr->Size - requested_size;
	set newnode->Next = curr->Next;

	set curr->Size =  requested_size;
	set curr->Next = newnode;

	return pop[h, prev, curr];
end

proc free[p:ptr]
var size:i64,
    new, prev, curr:Node,
    h:Heap, ok:bool
begin
    set h, ok = findHeap[p];
	if not ok begin
		fatal[ERR_BOUNDS, sizeof[ERR_BOUNDS]];
	end

	set size = (p:Obj)->Size;
    set new = (p-sizeof[Obj]):Node;

	set new->Size = size;
	set new->Next = NULLNODE;

	if h->Head == NULLNODE begin
		set h->Head = new;
		return;
	end

	if new < h->Head begin
        # in this case, 'new' is the first node
        # and will become the head
		prepend[h, new];
		return;
	end

	set prev = NULLNODE;
	set curr = h->Head;

	while curr != NULLNODE begin
		if prev != NULLNODE begin
			if prev < new and new < curr begin
                # in this case, 'new' is a middle node
				join[prev, new, curr];
				return;
			end
		end
		set prev = curr;
		set curr = curr->Next;
	end

    # in this case, 'new' is the last node
	append[prev, new];
end

proc append[prev, new:Node]
begin
	if prev + prev->Size== new begin
		# COALESCING, append
        set prev->Size = prev->Size + new->Size;
		return;
	end
    set prev->Next = new;
    set new->Next = NULLNODE;
end

proc prepend[h:Heap, new:Node]
begin
	if new + new->Size == h->Head begin
		# COALESCING, prepend
        set new->Size = new->Size + h->Head->Size;
        set new->Next = h->Head->Next;
		set h->Head = new;
		return;
	end

    set new->Next = h->Head;
    set h->Head = new;
end

proc join[prev, new, curr:Node]
var size:i64
begin
	if prev + prev->Size == new begin
		# COALESCING, append
        set size = prev->Size + new->Size;

		if prev+size == curr begin
			# in this case, prev, new and curr are adjacent
            set prev->Size = size + curr->Size;
            set prev->Next = curr->Next;
		end else begin
            # here only prev and new are adjacent
            set prev->Size = size;
		end
		return;
	end

	if new + new->Size == curr begin
		# COALESCING, prepend
        set prev->Next = new;
        set new->Size = new->Size + curr->Size;
        set new->Next = curr->Next;
		return;
	end

    set prev->Next = new;
    set new->Next = curr;
end

struct IA32 begin
	Num:i32;
end

struct PtrHeap begin
	Heap:Heap;
end

const MAXHEAPS = 8

data begin
	Top:IA32 {0};
	HeapTable:PtrHeap [MAXHEAPS];
end

# when inserting, if the new heap overlaps with an old heap,
# then the older one gets overwritten with the new heap
# leave them unordered, it shouldn't matter that much.
proc insert[h:Heap]
var i:i32, cmp:i16
begin
	set i = 0;
	while i < Top->Num begin
		set cmp = comp[HeapTable[i]->Heap, h];

		if cmp == INTERSECT begin
			set HeapTable[i]->Heap = h;
			return;
		end

		set i++;
	end

	if Top->Num >= MAXHEAPS begin
		fatal[ERR_TOOMANYHEAPS, sizeof[ERR_TOOMANYHEAPS]];
	end

	set HeapTable[Top->Num]->Heap = h;
	set Top->Num++;
end

const begin
	PRECEED = 1s;
	INTERSECT = 2s;
	SUCEED = 4s;
end

data BAD_HEAP "malformed heap header\n"

proc comp[a, b:Heap] i16
begin
	# sanity check, can be removed :)
	if a->Begin >= a->End or b->Begin >= b->End begin
		fatal[BAD_HEAP, sizeof[BAD_HEAP]];
		exit 2ss;
	end

	if a->End < b->Begin begin
		return PRECEED;
	end
	if b->End < a->Begin begin
		return SUCEED;
	end

	return INTERSECT;
end

proc findHeap[p:ptr] Heap, bool
var i:i32, curr:Heap
begin
	set i = 0;
	while i < Top->Num begin
		set curr = HeapTable[i]->Heap;
		if curr->Begin <= p and p < curr->End begin
			return curr, true;
		end

		set i++;
	end
	return NULL:Heap, false;
end

proc used[h:Heap] i64
begin
	return h->Size - freed[h]:i64;
end

proc freed[h:Heap] i64
var curr:Node, total:i64
begin
	set total = 0l;
	set curr = h->Head;
	while curr != NULLNODE begin
		set total += curr->Size;
		set curr = curr->Next;
	end
	return total;
end

data BAD_EMPTY "freed more memory than available???\n"

proc empty[h:Heap] bool
var total:i64
begin
	set total = freed[h]:i64;
	if total < h->Size begin
		return false;
	end
	if h->Size < total begin
		put_ln[];
		put_int[total];
		put_ln[];
		fatal[BAD_EMPTY, sizeof[BAD_EMPTY]];
	end
	return true;
end

proc print_list[h:Heap]
var curr:Node
begin
	set curr = h->Head;
	while curr != NULLNODE begin
		if h->Begin <= curr:ptr and curr:ptr<= h->End begin
			put_char['{'];
			put_uint[curr:u64]; put_char[','];
			put_int[curr->Size]; put_char[','];
			put_uint[curr->Next:u64];
			put_char['}'];
			put_char[' '];

			set curr = curr->Next;
		end else begin
			put_uint[h->Begin:u64];
			put_char[' '];
			put_uint[curr:u64];
			put_char[' '];
			put_uint[h->End:u64];
			put_ln[];
			print[ERR_BOUNDS, sizeof[ERR_BOUNDS]];
			return;
		end
	end
	put_char['\n'];
end
