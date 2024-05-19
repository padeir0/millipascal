# malloc is a configurable allocator used in the whole
# standard library. As the internal metadata about
# allocators is dynamic, the library is made in such
# a way that new heaps can be added at any point in execution
# and old heaps can be removed.
#
# THIS IS NOT THREAD SAFE.
#
# In reality, you'd have no more than a few regions,
# something like 3 pools and a backup free-list,
# with size ranges:
#     (0, 16] first pool
#     (16, 32] second pool
#     (32, 64] third pool
#     (0, 2^16] backup free-list heap
# order matters, so the first region has priority
# over a region set later.
from io import fatal
from ioutil import put_char, put_int, put_ln, oom, put_i32
from constants import NULL
from arrays import I32A

import
    flalloc as fl,
    poolalloc as pool

export malloc, malloc_panic, free, set_pool, set_heap,
       used, freed, empty, print_region_use

const begin
    NO_KIND = 0ss;
    HEAP = 1ss;
    POOL = 2ss;
end

struct Region begin
    Kind:i8;
    _pad1:i8;
    _pad2:i16;
    _pad3:i32;

    Min, Max:i32;

    Heap:fl::Heap;
    Pool:pool::Pool;
end

const MAX_REGIONS = 8

data begin
    top:I32A {0};
    region_table:Region [MAX_REGIONS];
end

data begin
    ERR_TOOMANYHEAPS "too many heaps\n";
    BAD_REGION "malformed region header\n";
    OUTSIDE_REGIONS "pointer is outside all regions\n";
    BAD_EMPTY "freed more memory than available???\n";
    ERR_BADKIND "internal error: bad region kind\n";
    ERR_DOESNTFIT "internal error: obj size doesn't fit in pool\n";
end

proc malloc_panic[size:i32] ptr
var p:ptr, ok:bool
begin
    set p, ok = malloc[size];
    if not ok begin
        put_ln[];
        put_i32[size];
        put_ln[];
        print_region_lists[];
        put_ln[];
        print_region_use[];
        oom[];
    end
    return p;
end

proc malloc[size:i32] ptr, bool
var i:i32, r:Region, p:ptr, ok:bool
begin
    set i = 0;
    while i < top->Num begin
        set r = region_table[i];
        if r->Min:i32 < size and size <= r->Max:i32 begin
            set p, ok = region_alloc[r, size];
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

proc region_alloc[r:Region, size:i32] ptr, bool
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
        bad_kind[];
    end
    return NULL, false;
end

proc free[p:ptr]
var r:Region, ok:bool
begin
    set r, ok = find_region[p];
    if not ok begin
        fatal[OUTSIDE_REGIONS, sizeof[OUTSIDE_REGIONS]];
    end
    if r->Kind == POOL begin
        pool::free[r->Pool, p];
    end elseif r->Kind == HEAP begin
        fl::free[r->Heap, p];
    end else begin
        put_char['\n'];
        bad_kind[];
    end
end

# chunkSize will be equal max
proc set_pool[buff:ptr, buffsize:i32, min, max:i32]
var p:pool::Pool
begin
    set p = pool::init[buff, buffsize, max:i32];
    insert_region[POOL, min, max, p->Begin, p->End, p:ptr];
end

proc set_heap[buff:ptr, buffsize:i32, min, max:i32]
var h:fl::Heap
begin
    set h = fl::init[buff, buffsize];
    insert_region[HEAP, min, max, h->Begin, h->End, h:ptr];
end

# When inserting, if the new heap overlaps with an old heap,
# then the older one gets overwritten with the new heap.
# Leave them unordered, it shouldn't matter that much.
proc insert_region[kind:i8, min,max:i32, beg,_end:ptr, r:ptr]
var i:i32, cmp:i16, reg:Region
begin
    set i = 0;
    while i < top->Num begin
        set reg = region_table[i];
        if reg->Kind == POOL begin
            set cmp = comp[beg, _end, reg->Pool->Begin, reg->Pool->End];
        end elseif reg->Kind == HEAP begin
            set cmp = comp[beg, _end, reg->Heap->Begin, reg->Heap->End];
        end else begin
            put_char['c'];
            bad_kind[];
        end

        if cmp == INTERSECT begin
            set_table[i, kind, min, max, r];
            return;
        end

        set i++;
    end

    if top->Num >= MAX_REGIONS begin
        fatal[ERR_TOOMANYHEAPS, sizeof[ERR_TOOMANYHEAPS]];
    end

    set_table[top->Num, kind, min, max, r];
    set top->Num++;
end

proc set_table[i:i32, kind:i8, min,max:i32, r:ptr]
begin
    set region_table[i]->Kind = kind;
    set region_table[i]->Min = min;
    set region_table[i]->Max = max;
    if kind == POOL begin
        set region_table[i]->Heap = NULL:fl::Heap;
        set region_table[i]->Pool = r:pool::Pool;
    end elseif kind == HEAP begin
        set region_table[i]->Heap = r:fl::Heap;
        set region_table[i]->Pool = NULL:pool::Pool;
    end else begin
        put_char['d'];
        bad_kind[];
    end
end

proc bad_kind[]
begin
    fatal[ERR_BADKIND, sizeof[ERR_BADKIND]];
end

const begin
    PRECEED = 1s;
    INTERSECT = 2s;
    SUCEED = 4s;
end

proc comp[a_begin, a_end, b_begin, b_end:ptr] i16
begin
    # sanity check, can be removed :)
    if a_begin >= a_end or b_begin >= b_end begin
        fatal[BAD_REGION, sizeof[BAD_REGION]];
        exit 2ss;
    end

    if a_end < b_begin begin
        return PRECEED;
    end
    if b_end < a_begin begin
        return SUCEED;
    end

    return INTERSECT;
end

proc find_region[p:ptr] Region, bool
var i:i32, r:Region
begin
    set i = 0;
    while i < top->Num begin
        set r = region_table[i];
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
            bad_kind[];
        end

        set i++;
    end
    return NULL:Region, false;
end

proc print_region_use[]
var i:i32, total, tot_freed:i64, r:Region
begin
    set i = 0;
    while i < top->Num begin
        set r = region_table[i];
        if r->Kind == POOL begin
            set tot_freed = pool::freed[r->Pool];
            set total = r->Pool->Size:i64;
        end elseif r->Kind == HEAP begin
            set tot_freed = fl::freed[r->Heap];
            set total = r->Heap->Size;
        end else begin
            put_char['f'];
            bad_kind[];
        end
        put_int[i:i64];
        put_char[':'];
        put_int[total-tot_freed];
        put_char['/'];
        put_int[total];
        put_ln[];
        set i++;
    end
end

proc print_region_lists[]
var i:i32, r:Region
begin
    set i = 0;
    while i < top->Num begin
        set r = region_table[i];
        if r->Kind == POOL begin
            put_char['!'];
        end elseif r->Kind == HEAP begin
            fl::print_list[r->Heap];
        end else begin
            put_char['f'];
            bad_kind[];
        end
        put_ln[];
        set i++;
    end
end

proc total_and_freed[] i64, i64
var i:i32, total, tot_freed:i64, r:Region
begin
    set i = 0;
    set total = 0l;
    set tot_freed = 0l;
    while i < top->Num begin
        set r = region_table[i];
        if r->Kind == POOL begin
            set tot_freed += pool::freed[r->Pool];
            set total += r->Pool->Size:i64;
        end elseif r->Kind == HEAP begin
            set tot_freed += fl::freed[r->Heap];
            set total += r->Heap->Size;
        end else begin
            put_char['f'];
            bad_kind[];
        end
        set i++;
    end
    return total, tot_freed;
end

proc used[] i64
var total, freed:i64
begin
    set total, freed = total_and_freed[];
    return total-freed;
end

proc freed[] i64
var total, freed:i64
begin
    set total, freed = total_and_freed[];
    return freed;
end

proc empty[] bool
var total, freed:i64
begin
    set total, freed = total_and_freed[];
	if freed < total begin
		return false;
	end
	if total < freed begin
		fatal[BAD_EMPTY, sizeof[BAD_EMPTY]];
	end
	return true;
end
