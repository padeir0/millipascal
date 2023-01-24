export Init, Alloc, Free

memory Heap 8192 # 8kb
memory Head 8

# [ SIZE, DATA...]
#         ^ Obj points here
# HEADER_OFFSET -8

# [ SIZE, NEXT, FREE_SPACE...]
#   ^ Node points here
# NODE_SIZE includes the NEXT slot
# FL_SIZE_OFFSET 0
# FL_NEXT_OFFSET 8
# Minimum cell size: 16

proc main
var obj:ptr
begin
	Init[]
	set obj = Alloc[4]
	set obj@i32 = 1g
	Free[obj]

	set obj = Alloc[1]
	set obj@i8 = 2r
	Free[obj]

	set obj = Alloc[4]
	set obj@i32 = 3g
	Free[obj]
end proc

memory OOM "Out of memory\n"

proc Alloc[size] ptr
var curr:ptr, curr_size, curr_next:ptr,
    prev:ptr, prev_size, prev_next:ptr
begin
	set curr = Head@ptr
	set curr_size = ~1
	set curr_next = ~1p

	set prev = ~1p
	set prev_size = ~1
	set prev_next = ~1p

	while curr != ~1p
	begin
		set curr_size = curr@i64
		set curr_next = (curr+8p)@ptr
		if curr_size == size
		begin
			return pop[prev, prev_size, curr, curr_next, curr_size]
		end if
		if curr_size > size
		begin
			#                         v minimum cell size
			if curr_size - (size+8) < 16
			begin
				return pop[prev, prev_size, curr, curr_next, curr_size]
			end if
			return split[prev, prev_size, curr, curr_next, curr_size, size]
		end if
		set prev = curr
		set prev_size = curr_size
		set prev_next = curr_next
		set curr = curr_next
	end while
	fatal[OOM, OOM.size]
	return ~1p
end proc

proc pop[prev:ptr, prev_size,
         curr:ptr, curr_next:ptr, curr_size] ptr
begin
	if prev != ~1p
	begin
	    #         v next_offset
		set (prev+8p)@ptr = curr_next
	end
	else
	begin
        set Head@ptr = curr_next
	end if
	clear[curr+8p, curr_size]
	return curr + 8p
end proc

proc split[prev:ptr, prev_size,
           curr:ptr, curr_next:ptr, curr_size,
		   requested_size] ptr
var actual_size,
	newnode:ptr, newnode_size, newnode_next:ptr
begin
	# adding the curr_size slot and aligning to 8
	set actual_size = requested_size + (8 - requested_size % 8) + 8

	set newnode_size = curr_size - actual_size
	set newnode = curr + actual_size:ptr
	set newnode_next = curr_next

	set newnode@i64 = newnode_size
	set (newnode+8p)@ptr = newnode_next

	set curr@i64 = requested_size
	set (curr+8p)@ptr = newnode

	return pop[prev, prev_size, curr, newnode, actual_size-8]
end proc

proc Free[obj:ptr]
var obj_size, new:ptr, prev:ptr, curr:ptr
begin
	#             v size_offset
	set new = obj-8p
	set obj_size = new@i64
	#        v next_offset
	set (new+8p)@ptr = ~1p

	set prev = ~1p
	set curr = Head@ptr

	# this is going to fail on a full heap
	while curr != ~1p
	begin
		if new < curr
		begin
			insertNode[prev, curr, new]
			defrag[]
			return
		end if
		set prev = curr
		#          v curr_next
		set curr = (curr+8p)@ptr
	end while
	defrag[]
end proc

proc insertNode[prev:ptr, curr:ptr, new:ptr]
var prev_next:ptr
begin
	if prev == ~1p
	begin
		if Head@ptr != ~1p
		begin
			set (new+8p)@ptr = Head@ptr
		end if
		set Head@ptr = new
	end
	else
	begin
		set prev_next = (prev+8p)@ptr
		if prev_next == ~1p
		begin
			set (prev+8p)@ptr = new
		end 
		else
		begin
			set (new+8p)@ptr = prev_next
			set (prev+8p)@ptr = new
		end if
	end if
end proc

proc defrag
var curr:ptr, curr_size, curr_next:ptr,
    next_size, next_next:ptr
begin
	set curr = Head@ptr
	set curr_size = ~1
	set curr_next = ~1p

	while curr != ~1p
	begin
		set curr_size = curr@i64
		set curr_next = (curr+8p)@ptr
		
		if curr + (curr_size + 8):ptr == curr_next
		begin
			set next_size = curr_next@i64
			set next_next = (curr_next+8p)@ptr

			set curr@i64 += next_size + 8;
			set (curr+8p)@ptr = next_next;
		end
		else
		begin
			return;
		end if
	end while
end proc

proc Init
begin
	clear[Heap, Heap.size]
	set Heap@i64 = Heap.size-8 # 8 bytes used for this field
	set (Heap+8p)@ptr = ~1p
	set Head@ptr = Heap
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

proc fatal[p:ptr, size]
begin
	write[p, size]
	exit 1r
end proc
