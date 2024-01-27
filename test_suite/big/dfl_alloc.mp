from buffutil import clear_bytes, clear_words
from ioutil import put_int, put_char, fatal, debug, put_2int, put_3int, put_4int
from constants import WORD, NULL, FULL, EMPTY

export Heap, ByteMap, Tail, Head, FrontLimit, BackLimit,
       alloc_f, alloc_b, free, init, get_total_free,
       print_list, print_map, validate_init,
       F_HEADER_SIZE,
       F_OBJ_SIZE_OFFSET,
	   B_HEADER_SIZE,
       B_OBJ_SIZE_OFFSET, B_OBJ_META_OFFSET,
	   B_OBJ_STUFF_OFFSET, B_OBJ_BITMAP_OFFSET

# cada objeto no frontheap vai ter um header de 8 bytes
#         v ptr
#     [   | ..... ]
#       ^
#       ├── 4 bytes, offset -8: Tamanho do objeto em WORDs
#       └── 4 bytes sobrando

const F_HEADER_SIZE 8

# offset negativo
const F_OBJ_SIZE_OFFSET 8p

# cada objeto no backheap vai ter um header de 8 bytes
#         v ptr
#     [   | ..... ]
#       ^
#       ├── 1 byte, offset -8: Tamanho do objeto em WORDs
#       ├── 1 byte, offset -7: Metadados pro GC sobre a natureza do objeto
#       ├── 2 bytes, offset -6: Lenght para bigint ou Tag pra tipos de união
#       └── 4 bytes, offset -4: Bitmap de ponteiros

const B_HEADER_SIZE 8

# offset negativo
const B_OBJ_SIZE_OFFSET 8p
const B_OBJ_META_OFFSET 7p
const B_OBJ_STUFF_OFFSET 6p
const B_OBJ_BITMAP_OFFSET 4p

const NODE_SIZE 24

const FL_SIZE_OFFSET 0p
const FL_PREV_OFFSET 8p
const FL_NEXT_OFFSET 16p

# memory Heap 2048 # 2KB
# memory ByteMap 256 # Heap.size / 8

memory Heap 1048576 # 1MB (wow)
memory ByteMap 131072 # Heap.size / 8

memory Head 8 # first node
memory Tail 8 # last node

memory FrontLimit 8 # limite do frontheap
memory BackLimit 8  # limit do backheap

memory UNREACHABLE "code should not reach here!\n"
memory OUTOFBOUNDS "pointer to be freed is out of heap bounds\n"
memory OUTOFLIMITS "pointer to be freed is out of back or front limit bounds\n"
memory OBJ_TOO_BIG "object is too big, max of 255 words\n"

proc main
begin
end proc

# size é em bytes, não em words
proc alloc_f[size] ptr
var out:ptr, alloc_size
begin
	set size += F_HEADER_SIZE
	# alinhamento
	if size%8 != 0 begin
		set size = size + (8-size%8)
	end if
	# os objetos precisam de ter espaço pra colocar
	# um nó quando forem dealocados
	if size < NODE_SIZE begin
		set size = NODE_SIZE
	end if

	set out, alloc_size = getNodeForward[size]
	if out == NULL begin
		return NULL
	end if

	if out + alloc_size:ptr > FrontLimit@ptr begin
		set FrontLimit@ptr = out + alloc_size:ptr
	end if
	
	set out += F_HEADER_SIZE:ptr
	
	# seta o header do objeto
	set (out-F_OBJ_SIZE_OFFSET)@u32 = (alloc_size>>3):u32

	setMap[out-F_HEADER_SIZE:ptr, alloc_size, FULL]

	return out
end proc

proc alloc_b[size, meta:u8, stuff:u16, bitmap:u32] ptr
var out:ptr, alloc_size
begin
	set size += B_HEADER_SIZE
	# alinhamento
	if size%8 != 0 begin
		set size = size + (8-size%8)
	end if
	# os objetos precisam de ter espaço pra colocar
	# um nó quando forem dealocados
	if size < NODE_SIZE begin
		set size = NODE_SIZE
	end if

	set out, alloc_size = getNodeBackwards[size]
	if out == NULL begin
		return NULL
	end if

	if (alloc_size>>3) > 255 begin
		fatal[OBJ_TOO_BIG, OBJ_TOO_BIG.size]
	end if
	
	if out < BackLimit@ptr begin
		set BackLimit@ptr = out
	end if
	set out += B_HEADER_SIZE:ptr
	
	# seta o header do objeto
	set (out-B_OBJ_SIZE_OFFSET)@u8 = (alloc_size>>3):u8
	set (out-B_OBJ_META_OFFSET)@u8 = meta
	set (out-B_OBJ_STUFF_OFFSET)@u16 = stuff
	set (out-B_OBJ_BITMAP_OFFSET)@u32 = bitmap

	setMap[out-B_HEADER_SIZE:ptr, alloc_size, FULL]

	return out
end proc

# precisamos retornar, junto com o ponteiro pro nó alocado,
# o tamanho do nó, já que pode ser que ele seja maior que o
# tamanho pedido
proc getNodeForward[size] ptr, i64
var curr:ptr, curr_size, next:ptr,
    prev:ptr
begin
	if Head@ptr == NULL begin
		return NULL, 0
	end if

	set curr = Head@ptr
	set curr_size = 0

	set next = NULL
	set prev = NULL

	while curr != NULL begin
		set curr_size = (curr+FL_SIZE_OFFSET)@i64
		set next = (curr+FL_NEXT_OFFSET)@ptr

		if curr_size == size begin
			return pop[prev, curr, next], curr_size
		end if
		if curr_size > size begin
			# se alocarmos um objeto e o espaço que sobrar
			# não ser suficiente pra um nó,
			# alocamos o espaço inteiro sem quebrar
			if curr_size - size < NODE_SIZE begin
				return pop[prev, curr, next], curr_size
			end if
			return splitFront[prev, curr, next, size], size
		end if

		set prev = curr
		set curr = next

		if BackLimit@ptr <= curr begin
			return NULL, 0
		end if
	end while

	return NULL, 0
end proc

proc getNodeBackwards[size] ptr, i64
var curr:ptr, curr_size, prev:ptr,
    next:ptr
begin
	if Tail@ptr == NULL begin
		return NULL, 0
	end if

	set next = NULL
	set prev = NULL

	set curr = Tail@ptr
	set curr_size = 0

	while curr != NULL begin
		set curr_size = (curr+FL_SIZE_OFFSET)@i64
		set prev = (curr+FL_PREV_OFFSET)@ptr
		if curr + curr_size:ptr <= FrontLimit@ptr begin
			return NULL, 0
		end if

		if curr_size == size begin
			return pop[prev, curr, next], curr_size
		end if
		if curr_size > size begin
			if curr_size - size < NODE_SIZE begin
				return pop[prev, curr, next], curr_size
			end if
			return splitBack[prev, curr, next, size], size
		end if

		set next = curr
		set curr = prev
	end while

	return NULL, 0
end proc

proc pop[prev:ptr, curr:ptr, next:ptr] ptr
begin
	if prev != NULL begin
		set (prev+FL_NEXT_OFFSET)@ptr = next
	end else begin
		if Head@ptr == Tail@ptr begin # nesse caso, temos que atualizar a tail tbm
			set Tail@ptr = next
		end if
        set Head@ptr = next
	end if

	if next != NULL begin
		set (next+FL_PREV_OFFSET)@ptr = prev
	end else begin
		if Head@ptr == Tail@ptr begin
			set Head@ptr = next
		end if
        set Tail@ptr = prev
	end if

	return curr:ptr
end proc

# divide o nó, alocando o espaço no começo
#
#   v alocado
# [xxxxx|           ]
#            ^ livre
proc splitFront[prev:ptr, curr:ptr, next:ptr, requested_size] ptr
var newnode:ptr, newnode_size, curr_size
begin
	set curr_size = (curr+FL_SIZE_OFFSET)@i64
	set newnode_size = curr_size - requested_size
	set newnode = curr + requested_size:ptr

	set (newnode + FL_SIZE_OFFSET)@i64 = newnode_size
	set (curr + FL_SIZE_OFFSET)@i64 = requested_size

	set (newnode + FL_NEXT_OFFSET)@ptr = next
	if next != NULL begin
		set (next + FL_PREV_OFFSET)@ptr = newnode
	end else begin
		# next == null implica que Tail@ptr == curr
		set Tail@ptr = newnode 
	end if

	set (curr + FL_NEXT_OFFSET)@ptr = newnode
	set (newnode + FL_PREV_OFFSET)@ptr = curr

	return pop[prev, curr, newnode]
end proc

# divide o nó, alocando o espaço no fim
#
#   v livre
# [            |xxxxxx]
#                ^ alocado
proc splitBack[prev:ptr, curr:ptr, next:ptr, requested_size] ptr
var newnode:ptr, newnode_size, remaining_size, curr_size
begin
	set curr_size = (curr+FL_SIZE_OFFSET)@i64
	set newnode_size = requested_size
	set remaining_size = curr_size - requested_size
	set newnode = curr + remaining_size:ptr

	set (newnode + FL_SIZE_OFFSET)@i64 = newnode_size
	set (curr + FL_SIZE_OFFSET)@i64 = remaining_size

	set (newnode + FL_NEXT_OFFSET)@ptr = next
	if next != NULL begin
		set (next + FL_PREV_OFFSET)@ptr = newnode
	end else begin
		# next == null implica que Tail@ptr == curr
		set Tail@ptr = newnode 
	end if

	set (curr + FL_NEXT_OFFSET)@ptr = newnode
	set (newnode + FL_PREV_OFFSET)@ptr = curr

	return pop[curr, newnode, next]
end proc

proc free[obj:ptr]
var obj_size, new:ptr, prev:ptr, curr:ptr, isFront:bool,
	leftmost:ptr, rightmost:ptr
begin
	if obj < FrontLimit@ptr begin
		set new = obj - F_HEADER_SIZE:ptr
		set obj_size = ((obj-F_OBJ_SIZE_OFFSET)@u32):i64 << 3
		set isFront = true
	end elseif obj > BackLimit@ptr begin
		set new = obj - B_HEADER_SIZE:ptr
		set obj_size = ((obj-B_OBJ_SIZE_OFFSET)@u16):i64 << 3
		set isFront = false
	end else begin
		fatal[OUTOFLIMITS, OUTOFLIMITS.size]
	end if

	if new < Heap or new > Heap + Heap.size:ptr begin
		fatal[OUTOFBOUNDS, OUTOFBOUNDS.size]
	end if

	setMap[new, obj_size, EMPTY]

	set (new+FL_SIZE_OFFSET)@i64 = obj_size
	set (new+FL_NEXT_OFFSET)@ptr = NULL
	set (new+FL_PREV_OFFSET)@ptr = NULL

	if Head@ptr == NULL or Tail@ptr == NULL begin
		set Head@ptr = new
		set Tail@ptr = new
		updateLimits[new, new + obj_size:ptr]
		return
	end if

	if new < Head@ptr begin
		# nesse caso, new é o primeiro nó da lista
		# e vai virar o novo Head
		prepend[new]
		updateLimits[new, new + obj_size:ptr]
		return
	end if
	if Tail@ptr < new begin
		# nesse caso, new é o ultimo nó da lista
		# e vai virar o novo Tail
		append[new]
		updateLimits[new, new + obj_size:ptr]
		return
	end if

	set prev = NULL
	set curr = Head@ptr

	while curr != NULL
	begin
		if prev != NULL begin
			if prev < new and new < curr begin
				# nesse caso, new é algum nó do meio
				set leftmost, rightmost = join[prev, new, curr]
				updateLimits[leftmost, rightmost]
				return
			end if
		end if
		set prev = curr
		set curr = (curr+FL_NEXT_OFFSET)@ptr
	end while

	# nesse caso, new é o ultimo nó (não deveria chegar aqui)
	# append[prev, new]
	# if not isFront begin
	# 	set BackLimit@ptr = new+obj_size:ptr
	# end if
	fatal[UNREACHABLE, UNREACHABLE.size]
end proc

proc updateLimits[leftmost:ptr, rightmost:ptr]
begin
	if leftmost <= BackLimit@ptr and BackLimit@ptr < rightmost begin
		set BackLimit@ptr = rightmost
	end if
	if FrontLimit@ptr <= rightmost and leftmost < FrontLimit@ptr begin
		set FrontLimit@ptr = leftmost
	end if
end proc

proc append[new:ptr]
var size
begin
	set size = (Tail@ptr+FL_SIZE_OFFSET)@i64
	if Tail@ptr+size:ptr == new begin
		# COALESCING, append
		set size += (new+FL_SIZE_OFFSET)@i64 
		set (Tail@ptr+FL_SIZE_OFFSET)@i64 = size
		return
	end if
	set (Tail@ptr + FL_NEXT_OFFSET)@ptr = new
	set (new+FL_PREV_OFFSET)@ptr = Tail@ptr
	set Tail@ptr = new
end proc

proc prepend[new:ptr]
var size, next:ptr
begin
	set size = (new+FL_SIZE_OFFSET)@i64
	if new + size:ptr == Head@ptr begin
		# COALESCING, prepend
		set next = (Head@ptr + FL_NEXT_OFFSET)@ptr
		set (new+FL_SIZE_OFFSET)@i64 = size + (Head@ptr + FL_SIZE_OFFSET)@i64
		set (new+FL_NEXT_OFFSET)@ptr = next
		if next != NULL begin
			set (next+FL_PREV_OFFSET)@ptr = new
		end else begin
			set Tail@ptr = new
		end if
		set Head@ptr = new
		return 
	end if

	set next = Head@ptr
	set (new+FL_NEXT_OFFSET)@ptr = next
	set (next+FL_PREV_OFFSET)@ptr = new
	set Head@ptr = new
end proc

proc join[prev:ptr, new:ptr, curr:ptr] ptr, ptr
var size, next:ptr
begin
	set size = (prev+FL_SIZE_OFFSET)@i64
	if prev+size:ptr == new begin
		# COALESCING, append
		set size = size + (new+FL_SIZE_OFFSET)@i64

		if prev+size:ptr == curr begin
			set next = (curr+FL_NEXT_OFFSET)@ptr
			# nesse caso, prev, new e curr são adjacentes
			set size += (curr+FL_SIZE_OFFSET)@i64
			set (prev+FL_SIZE_OFFSET)@i64 = size

			set (prev+FL_NEXT_OFFSET)@ptr = next
			if next != NULL begin
				set (next+FL_PREV_OFFSET)@ptr = prev
			end else begin
				set Tail@ptr = prev
			end if
		end else begin
			# aqui, somente prev e new são adjacentes
			set (prev+FL_SIZE_OFFSET)@i64 = size
		end if
		return prev, prev+size:ptr
	end if

	set size = (new+FL_SIZE_OFFSET)@i64
	if new + size:ptr == curr begin
		# COALESCING, prepend
		set next = (curr+FL_NEXT_OFFSET)@ptr

		set (prev+FL_NEXT_OFFSET)@ptr = new
		set (new+FL_PREV_OFFSET)@ptr = prev

		set size += (curr+FL_SIZE_OFFSET)@i64
		set (new+FL_SIZE_OFFSET)@i64 = size

		set (new+FL_NEXT_OFFSET)@ptr = next
		if next != NULL begin
			set (next+FL_PREV_OFFSET)@ptr = new
		end else begin
			set Tail@ptr = new
		end if
		return new, new+size:ptr
	end if

	set (prev+FL_NEXT_OFFSET)@ptr = new
	set (new+FL_PREV_OFFSET)@ptr = prev

	set (new+FL_NEXT_OFFSET)@ptr = curr
	set (curr+FL_PREV_OFFSET)@ptr = new
	return new, new+size:ptr
end proc

proc init
begin
	clear_words[Heap, Heap.size]
	clear_bytes[ByteMap, ByteMap.size]

	set Head@ptr = Heap
	set Tail@ptr = Heap

	set FrontLimit@ptr = Heap
	set BackLimit@ptr = Heap + Heap.size:ptr
	set (Heap+FL_SIZE_OFFSET)@i64 = Heap.size
	set (Heap+FL_NEXT_OFFSET)@ptr = NULL
end proc

proc setMap[obj:ptr, size, b:i8]
var start:ptr, i:ptr, size_words:ptr
begin
	set size_words = (size>>3):ptr
	set start = ByteMap + ((obj - Heap) / WORD:ptr)
	set i = 0p
	while i < size_words begin
		set (start+i)@i8 = b
		set i += 1p
	end while
end proc

memory msg_total_free "free: "
memory msg_flimit ", FrontLimit: "
memory msg_blimit ", BackLimit: "

proc print_list[]
var curr:ptr
begin
	set curr = Head@ptr
	while curr != NULL begin
		put_char['{']
		put_4int[curr:i64,
		         (curr+FL_SIZE_OFFSET)@i64,
				 (curr+FL_PREV_OFFSET)@i64,
				 (curr+FL_NEXT_OFFSET)@i64,
				]
		put_char['}']
		put_char[' ']
		set curr = (curr+FL_NEXT_OFFSET)@ptr
	end while
	put_char[' ']

	write[msg_total_free, msg_total_free.size]
	put_int[get_total_free[]]

	write[msg_flimit, msg_flimit.size]
	put_int[FrontLimit@i64]

	write[msg_blimit, msg_blimit.size]
	put_int[BackLimit@i64]

	put_char['\n']
end proc

proc get_total_free[] i64
var curr:ptr, total_free
begin
	set curr = Head@ptr
	set total_free = 0
	while curr != NULL begin
		set total_free += (curr+FL_SIZE_OFFSET)@i64
		set curr = (curr+FL_NEXT_OFFSET)@ptr
	end while
	return total_free
end proc

proc print_map[]
var i
begin
	set i = 0
	put_char['[']
	while i < Heap.size/WORD begin
		if (ByteMap+i:ptr)@i8 == EMPTY begin
			put_char[' ']
		end if
		if (ByteMap+i:ptr)@i8 == FULL begin
			put_char['X']
		end if
		set i += 1
	end while
	put_char[']']
	put_char['\n']
end proc

# validates if we're back to the initial position
proc validate_init
begin
	if get_total_free[] != Heap.size begin
		exit 2ss
	end if
	if Head@ptr != Tail@ptr begin
		exit 3ss
	end if
	if FrontLimit@ptr != Heap begin
		exit 4ss
	end if
	if BackLimit@ptr != Heap+Heap.size:ptr begin
		exit 5ss
	end if
end proc
