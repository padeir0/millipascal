# malloc is a configurable allocator used in the whole
# standard library. As the internal metadata about
# allocators is dynamic, the library is made in such
# a way that new heaps can be added at any point in execution
# and old heaps can be removed.
#
# In reality, you'd have no more than a few heaps,
# something like 3 pools and a backup free-list,
# with size ranges:
#     (0, 16] first pool
#     (16, 32] second pool
#     (32, 64] third pool
#     (0, 2^16] backup free-list heap
# order matters, so the first region begin set has priority
# over a region set later.
from io import fatal
from ioutil import put_char, put_int
from constants import NULL
from arrays import I32A

import
    flalloc as fl,
    poolalloc as pool

export malloc, free, setPool, setHeap, used, freed, empty

const begin
    NO_KIND = 0ss;
    HEAP = 1ss;
    POOL = 2ss;
end

struct Region begin
    Kind:i8;
    _pad1:i8;
    _pad2:i16;

    Min, Max:i16;

    Heap:fl::Heap;
    Pool:pool::Pool;
end

const MAXREGIONS = 8

data begin
    Top:I32A {0};
    RegionTable:Region [MAXREGIONS];
end

data begin
    ERR_TOOMANYHEAPS "too many heaps\n";
    BAD_REGION "malformed region header\n";
    OUTSIDE_REGIONS "pointer is outside all regions\n";
    BAD_EMPTY "freed more memory than available???\n";
    ERR_BADKIND "internal error: bad region kind\n";
    ERR_DOESNTFIT "internal error: obj size doesn't fit in pool\n";
end

proc malloc[size:i32] ptr, bool
var i:i32, r:Region, p:ptr, ok:bool
begin
    set i = 0;
    while i < Top->Num begin
        set r = RegionTable[i];
        if r->Min:i32 <= size and size <= r->Max:i32 begin
            set p, ok = regionAlloc[r, size];
            # If we have two regions with overlapping sizes,
            # this ensures if one is full, we can use the other.
            # This is useful for backup heaps.
            if ok begin
                return p, true;
            end
        end
        set i++;
    end
    return NULL, false;
end

proc regionAlloc[r:Region, size:i32] ptr, bool
var p:ptr, ok:bool
begin
    if r->Kind == POOL begin
        if r->Pool->ChunkSize < size begin
            fatal[ERR_DOESNTFIT, sizeof[ERR_DOESNTFIT]];
        end
        set p, ok = pool::alloc[r->Pool];
        return p, ok;
    end elseif r->Kind == HEAP begin
        set p, ok = fl::alloc[r->Heap, size];
        return p, ok;
    end else begin
        put_char['a'];
        badKind[];
    end
    return NULL, false;
end

proc free[p:ptr]
var r:Region, ok:bool
begin
    set r, ok = findRegion[p];
    if not ok begin
        fatal[OUTSIDE_REGIONS, sizeof[OUTSIDE_REGIONS]];
    end
    if r->Kind == POOL begin
        pool::free[r->Pool, p];
    end elseif r->Kind == HEAP begin
        fl::free[r->Heap, p];
    end else begin
        put_char['\n'];
        badKind[];
    end
end

# chunkSize will be equal max
proc setPool[buff:ptr, buffsize:i32, min, max:i16]
var p:pool::Pool
begin
    set p = pool::init[buff, buffsize, max:i32];
    insertRegion[POOL, min, max, p->Begin, p->End, p:ptr];
end

proc setHeap[buff:ptr, buffsize:i32, min, max:i16]
var h:fl::Heap
begin
    set h = fl::init[buff, buffsize];
    insertRegion[HEAP, min, max, h->Begin, h->End, h:ptr];
end

# When inserting, if the new heap overlaps with an old heap,
# then the older one gets overwritten with the new heap.
# Leave them unordered, it shouldn't matter that much.
proc insertRegion[kind:i8, min,max:i16, Beg,End:ptr, r:ptr]
var i:i32, cmp:i16, reg:Region
begin
    set i = 0;
    while i < Top->Num begin
        set reg = RegionTable[i];
        if reg->Kind == POOL begin
            set cmp = comp[Beg, End, reg->Pool->Begin, reg->Pool->End];
        end elseif reg->Kind == HEAP begin
            set cmp = comp[Beg, End, reg->Heap->Begin, reg->Heap->End];
        end else begin
            put_char['c'];
            badKind[];
        end

        if cmp == INTERSECT begin
            setTable[i, kind, min, max, r];
            return;
        end

        set i++;
    end

    if Top->Num >= MAXREGIONS begin
        fatal[ERR_TOOMANYHEAPS, sizeof[ERR_TOOMANYHEAPS]];
    end

    setTable[Top->Num, kind, min, max, r];
    set Top->Num++;
end

proc setTable[i:i32, kind:i8, min,max:i16, r:ptr]
begin
    set RegionTable[i]->Kind = kind;
    set RegionTable[i]->Min = min;
    set RegionTable[i]->Max = max;
    if kind == POOL begin
        set RegionTable[i]->Heap = NULL:fl::Heap;
        set RegionTable[i]->Pool = r:pool::Pool;
    end elseif kind == HEAP begin
        set RegionTable[i]->Heap = r:fl::Heap;
        set RegionTable[i]->Pool = NULL:pool::Pool;
    end else begin
            put_char['d'];
        badKind[];
    end
end

proc badKind[]
begin
    fatal[ERR_BADKIND, sizeof[ERR_BADKIND]];
end

const begin
    PRECEED = 1s;
    INTERSECT = 2s;
    SUCEED = 4s;
end

proc comp[aBegin, aEnd, bBegin, bEnd:ptr] i16
begin
    # sanity check, can be removed :)
    if aBegin >= aEnd or bBegin >= bEnd begin
        fatal[BAD_REGION, sizeof[BAD_REGION]];
        exit 2ss;
    end

    if aEnd < bBegin begin
        return PRECEED;
    end
    if bEnd < aBegin begin
        return SUCEED;
    end

    return INTERSECT;
end

proc findRegion[p:ptr] Region, bool
var i:i32, r:Region
begin
    set i = 0;
    while i < Top->Num begin
        set r = RegionTable[i];
        if r->Kind == POOL begin
            if r->Pool->Begin <= p and p < r->Pool->End begin
                return r, true;
            end
        end elseif r->Kind == HEAP begin
            if r->Heap->Begin <= p and p < r->Heap->End begin
                return r, true;
            end
        end else begin
            put_char['e'];
            badKind[];
        end

        set i++;
    end
    return NULL:Region, false;
end

proc totalAndFreed[] i64, i64
var i:i32, total, tot_freed:i64, r:Region
begin
    set i = 0;
    set total = 0l;
    set tot_freed = 0l;
    while i < Top->Num begin
        set r = RegionTable[i];
        if r->Kind == POOL begin
            set tot_freed += pool::freed[r->Pool];
            set total += r->Pool->Size:i64;
        end elseif r->Kind == HEAP begin
            set tot_freed += fl::freed[r->Heap];
            set total += r->Heap->Size;
        end else begin
            put_char['f'];
            badKind[];
        end
        set i++;
    end
    return total, tot_freed;
end

proc used[] i64
var total, freed:i64
begin
    set total, freed = totalAndFreed[];
    return total-freed;
end

proc freed[] i64
var total, freed:i64
begin
    set total, freed = totalAndFreed[];
    return freed;
end

proc empty[] bool
var total, freed:i64
begin
    set total, freed = totalAndFreed[];
	if freed < total begin
		return false;
	end
	if total < freed begin
		fatal[BAD_EMPTY, sizeof[BAD_EMPTY]];
	end
	return true;
end
