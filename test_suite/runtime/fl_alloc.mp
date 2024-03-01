from buffutil import clear_bytes, clear_words
from ioutil import put_int, put_char, fatal, debug, oom
from constants import WORD, NULL, FULL, EMPTY

export init, alloc, free

# cada objeto vai ter um header:
#         v ptr
#     [   | ..... ]
#       ^
#       ├── 2 bytes, offset -8: Tamanho do objeto em WORDs
#       ├── 2 bytes, offset -6: Tag pra tipos de união
#       └── 4 bytes, offset -4: Bitmap de ponteiros

const HEADER_SIZE = 8

# offset negativo
const OBJ_SIZE_OFFSET = 8
const OBJ_TAG_OFFSET = 6
const OBJ_BITMAP_OFFSET = 4

# Além disso, cada nó da free-list terá 8 bytes,
# Com os primeiros 8 bytes representando o tamanho da àrea a
# ser alocada (em bytes)
# e os 8 bytes sequentes sendo um ponteiro pro próximo nó

const NODE_SIZE = 16

const FL_SIZE_OFFSET = 0
const FL_NEXT_OFFSET = 8

data Heap [1<<16] # 64KB
data ByteMap [(sizeof Heap)/8] # cada byte representa uma WORD na Heap
data Head [sizeof ptr]

data UNREACHABLE "Code should not reach here!\n"
data OUTOFBOUNDS "Pointer to be freed is out of bounds\n"

const TOTAL_TEST_PTRS = 256
data Test [TOTAL_TEST_PTRS * sizeof ptr]

proc main
var i:i64
begin
	init[]

	set i = 0

	while i < 4 begin
		clear_words[Test, sizeof Test]
		_alloc[]
		set i += 1
	end
end

proc _alloc
var i:i64, a:ptr
begin
	set i = 0
	while i < TOTAL_TEST_PTRS begin
		set a = NULL
		if i % 5 == 0 begin
			set a = alloc[16, 0us, 0ul]
		end elseif i % 5 == 1 begin
			set a = alloc[24, 0us, 0ul]
		end elseif i % 5 == 2 begin
			set a = alloc[48, 0us, 0ul]
		end elseif i % 5 == 3 begin
			set a = alloc[25, 0us, 0ul]
		end elseif i % 5 == 4 begin
			set a = alloc[33, 0us, 0ul]
		end

		if i % 4 == 0 begin
			free_n[1]
		end elseif i % 4 == 3 begin
			free_n[2]
		end

		if a != NULL begin
			set (Test + i*sizeof ptr)@ptr = a
		end else begin
			oom[]
		end
		set i += 1
	end
	free_n[TOTAL_TEST_PTRS]
end

proc free_n[n:i64]
var i:i64, a:ptr
begin
	set i = 0
	while i < TOTAL_TEST_PTRS begin
		if n <= 0 begin
			return
		end
		set a = (Test + i*sizeof ptr)@ptr
		if a != NULL begin
			free[a]
			set (Test + i*sizeof ptr)@ptr = NULL
			set n -= 1
		end
		set i += 1
	end
end

proc alloc[size:i64, tag:u16, bitmap:u32] ptr
var out:ptr, alloc_size:i64
begin
	set size += HEADER_SIZE
	# alinhamento
	if size%8 != 0 begin
		set size = size + (8-size%8)
	end
	# os objetos precisam de ter espaço pra colocar
	# um nó quando forem dealocados
	if size < NODE_SIZE begin
		set size = NODE_SIZE
	end

	set out, alloc_size = get_node[size]
	if out == NULL begin
		return NULL
	end
	
	# seta o header do objeto
	set (out-OBJ_SIZE_OFFSET)@u16 = (alloc_size>>3):u16
	set (out-OBJ_TAG_OFFSET)@u16 = tag
	set (out-OBJ_BITMAP_OFFSET)@u32 = bitmap

	set_map[out-HEADER_SIZE, alloc_size, FULL]

	return out
end

# precisamos retornar, junto com o ponteiro pro nó alocado,
# o tamanho do nó, já que pode ser que ele seja maior que o
# tamanho pedido
proc get_node[size:i64] ptr, i64
var curr:ptr, curr_size:i64, curr_next:ptr,
    prev:ptr, prev_size:i64, prev_next:ptr
begin
	if Head@ptr == NULL begin
		return NULL, 0
	end

	set curr = Head@ptr
	set curr_size = 0
	set curr_next = NULL

	set prev = NULL
	set prev_size = 0
	set prev_next = NULL

	while curr != NULL begin
		set curr_size = (curr+FL_SIZE_OFFSET)@i64
		set curr_next = (curr+FL_NEXT_OFFSET)@ptr

		if curr_size == size begin
			return pop[prev, curr, curr_next], curr_size
		end
		if curr_size > size begin
			# se alocarmos um objeto e o espaço que sobrar
			# não ser suficiente pra um nó,
			# alocamos o espaço inteiro sem quebrar
			if curr_size - size < NODE_SIZE begin
				return pop[prev, curr, curr_next], curr_size
			end
			return split[prev, curr, curr_next, curr_size, size], size
		end

		set prev = curr
		set prev_size = curr_size
		set prev_next = curr_next
		set curr = curr_next
	end

	return NULL, 0
end

proc pop[prev, curr, curr_next:ptr] ptr
begin
	if prev != NULL begin
		set (prev+FL_NEXT_OFFSET)@ptr = curr_next
	end else begin
        set Head@ptr = curr_next
	end
	return curr + HEADER_SIZE
end

proc split[prev, curr, curr_next:ptr,
           curr_size, requested_size:i64] ptr
var newnode:ptr, newnode_size:i64, newnode_next:ptr
begin
	set newnode_size = curr_size - requested_size
	set newnode = curr + requested_size
	set newnode_next = curr_next

	set (newnode + FL_SIZE_OFFSET)@i64 = newnode_size
	set (newnode + FL_NEXT_OFFSET)@ptr = newnode_next

	set (curr + FL_SIZE_OFFSET)@i64 = requested_size
	set (curr + FL_NEXT_OFFSET)@ptr = newnode

	return pop[prev, curr, newnode]
end

proc free[obj:ptr]
var obj_size:i64, new, prev, curr:ptr
begin
	set new = obj - HEADER_SIZE
	if new < Heap or new > Heap + sizeof Heap begin
		fatal[OUTOFBOUNDS, sizeof OUTOFBOUNDS]
	end

	set obj_size = ((obj-OBJ_SIZE_OFFSET)@u16):i64 << 3

	set_map[new, obj_size, EMPTY]

	set (new+FL_SIZE_OFFSET)@i64 = obj_size
	set (new+FL_NEXT_OFFSET)@ptr = NULL

	if Head@ptr == NULL begin
		set Head@ptr = new
		return
	end

	if new < Head@ptr begin
		# nesse caso, new é o primeiro nó da lista
		# e vai virar o novo Head
		prepend[new]
		return
	end

	set prev = NULL
	set curr = Head@ptr

	while curr != NULL
	begin
		if prev != NULL begin
			if prev < new and new < curr begin
				# nesse caso, new é algum nó do meio
				join[prev, new, curr]
				return
			end
		end
		set prev = curr
		set curr = (curr+FL_NEXT_OFFSET)@ptr
	end

	# nesse caso, new é o ultimo nó
	append[prev, new]
end

proc append[prev, new:ptr]
var size:i64
begin
	set size = (prev+FL_SIZE_OFFSET)@i64
	if prev+size == new begin
		# COALESCING, append
		set (prev+FL_SIZE_OFFSET)@i64 = size + (new+FL_SIZE_OFFSET)@i64
		return
	end
	set (prev+FL_NEXT_OFFSET)@ptr = new
	set (new+FL_NEXT_OFFSET)@ptr = NULL
end

proc prepend[new:ptr]
var size:i64
begin
	set size = (new+FL_SIZE_OFFSET)@i64
	if new + size == Head@ptr begin
		# COALESCING, prepend
		set (new+FL_SIZE_OFFSET)@i64 = size + (Head@ptr + FL_SIZE_OFFSET)@i64
		set (new+FL_NEXT_OFFSET)@ptr = (Head@ptr + FL_NEXT_OFFSET)@ptr
		set Head@ptr = new
		return
	end

	set (new+FL_NEXT_OFFSET)@ptr = Head@ptr
	set Head@ptr = new
end

proc join[prev, new, curr:ptr]
var size:i64
begin
	set size = (prev+FL_SIZE_OFFSET)@i64
	if prev+size == new begin
		# COALESCING, append
		set size = size + (new+FL_SIZE_OFFSET)@i64

		if prev+size == curr begin
			# nesse caso, prev, new e curr são adjacentes
			set (prev+FL_SIZE_OFFSET)@i64 = size + (curr+FL_SIZE_OFFSET)@i64
			set (prev+FL_NEXT_OFFSET)@ptr = (curr+FL_NEXT_OFFSET)@ptr
		end else begin
			# aqui, somente prev e new são adjacentes
			set (prev+FL_SIZE_OFFSET)@i64 = size
		end
		return
	end

	set size = (new+FL_SIZE_OFFSET)@i64
	if new + size == curr begin
		# COALESCING, prepend
		set (prev+FL_NEXT_OFFSET)@ptr = new
		set (new+FL_SIZE_OFFSET)@i64 = size + (curr+FL_SIZE_OFFSET)@i64
		set (new+FL_NEXT_OFFSET)@ptr = (curr+FL_NEXT_OFFSET)@ptr
		return
	end

	set (prev+FL_NEXT_OFFSET)@ptr = new
	set (new+FL_NEXT_OFFSET)@ptr = curr
end

proc init
begin
	clear_words[Heap, sizeof Heap]
	clear_bytes[ByteMap, sizeof ByteMap]

	set Head@ptr = Heap
	set (Heap+FL_SIZE_OFFSET)@i64 = sizeof Heap
	set (Heap+FL_NEXT_OFFSET)@ptr = NULL
end

data MSG_TOTAL_FREE "free: "

proc print_list[]
var curr:ptr, totalFree:i64
begin
	set curr = Head@ptr
	set totalFree = 0
	while curr != NULL begin
		put_char['{']
		put_int[curr:i64]; put_char[',']
		put_int[(curr+FL_SIZE_OFFSET)@i64]; put_char[',']
		put_int[(curr+FL_NEXT_OFFSET)@i64]
		put_char['}']
		put_char[' ']
		set totalFree += (curr+FL_SIZE_OFFSET)@i64
		set curr = (curr+FL_NEXT_OFFSET)@ptr
	end
	put_char[' ']
	write[MSG_TOTAL_FREE, sizeof MSG_TOTAL_FREE]
	put_int[totalFree]
	put_char['\n']
end

proc print_map[]
var i:i64
begin
	set i = 0
	put_char['[']
	while i < (sizeof Heap)/WORD begin
		if (ByteMap+i)@i8 == EMPTY begin
			put_char[' ']
		end
		if (ByteMap+i)@i8 == FULL begin
			put_char['X']
		end
		set i += 1
	end
	put_char[']']
	put_char['\n']
end

proc set_map[obj:ptr, size:i64, b:i8]
var start:ptr, i, size_words:i64
begin
	set size_words = (size>>3)
	set start = ByteMap + ((obj:i64 - Heap:i64) / WORD)
	set i = 0
	while i < size_words begin
		set (start+i)@i8 = b
		set i += 1
	end
end
