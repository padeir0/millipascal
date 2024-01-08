from buffutil import clear_bytes, clear_words
from ioutil import put_int, put_char, fatal, debug

const WORD 8     # uma word tem 8 bytes, toda memória será alinhada dessa forma.
const NULL 0p

# Esse alocador é o principio do runtime do Anu, portanto
# cada objeto vai ter um header:
#         v ptr
#     [   | ..... ]
#       ^
#       ├── 2 bytes, offset -8: Tamanho do objeto em WORDs
#       ├── 2 bytes, offset -6: Tag pra tipos de união
#       └── 4 bytes, offset -4: Bitmap de ponteiros
#
# E teremos um bitmap separado do heap

# offset negativo
const OBJ_SIZE_OFFSET 8p
const OBJ_TAG_OFFSET 6p
const OBJ_BITMAP_OFFSET 4p
const HEADER_SIZE 8

# Além disso, cada nó da free-list terá 8 bytes,
# Com os primeiros 8 bytes representando o tamanho da àrea a
# ser alocada (em bytes)
# e os 8 bytes sequentes sendo um ponteiro pro próximo nó

const FL_SIZE_OFFSET 0p
const FL_NEXT_OFFSET 8p

const MIN_ALLOC_SIZE 8 # 8 bytes = WORD é o tamanho minimo
const NODE_SIZE 16

memory Heap 1024

memory ByteMap 128   # Heap.size / 8, cada byte representa uma WORD na Heap
const FULL  1ss
const EMPTY 0ss

memory Head 8

memory OOM "Out of memory\n"
memory UNREACHABLE "Code should not reach here!\n"

memory test 512

proc main
var i, a:ptr
begin
	clear_words[test, test.size]

	set i = 0 
	Init[]
	while i < 64 begin
		set a = NULL
		if i % 2 == 0 begin
			set a = Alloc[32, 0us, 0ul]
		end elseif i % 3 == 0 begin
			set a = Alloc[24, 0us, 0ul]
		end elseif i % 5 == 0 begin
			freeN[2]
		end else begin
			set a = Alloc[56, 0us, 0ul]
		end if

		if a != NULL begin
			set (test + (i*8):ptr)@ptr = a
		end if
		PrintMap[]
	end while
end proc

proc freeN[n]
var i, a:ptr
begin
	set i = 0
	while i < 64 begin
		if n <= 0 begin
			return
		end if
		set a = (test + (i*8):ptr)@ptr
		if a != NULL begin
			Free[a]
			set (test + (i*8):ptr)@ptr = NULL
			set n -= 1
		end if
	end while
end proc

proc Alloc[size, tag:u16, bitmap:u32] ptr
var out:ptr, alloc_size
begin
	set size += 8 # adiciona o tamanho do header do objeto
	# alinhamento
	if size%8 != 0 begin
		set size = size + (8-size%8)
	end if
	# os objetos precisam de ter espaço pra colocar
	# um nó quando forem dealocados
	if size < NODE_SIZE begin
		set size = NODE_SIZE
	end if

	set out, alloc_size = getNode[size]
	# seta o header do objeto
	set (out-OBJ_SIZE_OFFSET)@u16 = (alloc_size>>3):u16
	set (out-OBJ_TAG_OFFSET)@u16 = tag
	set (out-OBJ_BITMAP_OFFSET)@u32 = bitmap

	setMap[out, alloc_size, FULL]

	return out
end proc

# precisamos retornar, junto com o ponteiro pro nó alocado,
# o tamanho do nó, já que pode ser que ele seja maior que o
# tamanho pedido
proc getNode[size] ptr, i64
var curr:ptr, curr_size, curr_next:ptr,
    prev:ptr, prev_size, prev_next:ptr
begin
	if Head@ptr == NULL begin
		return NULL, 0
	end if

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
		end if
		if curr_size > size begin
			# se alocarmos um objeto e o espaço que sobrar
			# não ser suficiente pra um nó,
			# alocamos o espaço inteiro sem quebrar
			if curr_size - size < NODE_SIZE begin
				return pop[prev, curr, curr_next], curr_size
			end if
			return split[prev, curr, curr_next, curr_size, size], size
		end if

		set prev = curr
		set prev_size = curr_size
		set prev_next = curr_next
		set curr = curr_next
	end while

	return NULL, 0
end proc

proc pop[prev:ptr, curr:ptr, curr_next:ptr] ptr
begin
	if prev != NULL begin
		set (prev+FL_NEXT_OFFSET)@ptr = curr_next
	end else begin
        set Head@ptr = curr_next
	end if
	return curr + HEADER_SIZE:ptr
end proc

proc split[prev:ptr, curr:ptr, curr_next:ptr,
           curr_size, requested_size] ptr
var newnode:ptr, newnode_size, newnode_next:ptr
begin
	set newnode_size = curr_size - requested_size
	set newnode = curr + requested_size:ptr
	set newnode_next = curr_next

	set (newnode + FL_SIZE_OFFSET)@i64 = newnode_size
	set (newnode + FL_NEXT_OFFSET)@ptr = newnode_next

	set (curr + FL_SIZE_OFFSET)@i64 = requested_size
	set (curr + FL_NEXT_OFFSET)@ptr = newnode

	return pop[prev, curr, newnode]
end proc

proc Free[obj:ptr]
var obj_size, new:ptr, prev:ptr, curr:ptr
begin
	set obj_size = ((obj-OBJ_SIZE_OFFSET)@u16):i64 << 3
	set new = obj - HEADER_SIZE:ptr
	setMap[obj, obj_size, EMPTY]

	set (new+FL_SIZE_OFFSET)@i64 = obj_size
	set (new+FL_NEXT_OFFSET)@ptr = NULL

	if Head@ptr == NULL begin
		set Head@ptr = new
		return
	end if

	set prev = NULL
	set curr = Head@ptr

	if prev == NULL and new < curr begin
		# nesse caso, new é o primeiro nó da lista
		# e vai virar o novo Head
		join[new, curr]
		set Head@ptr = new
		return
	end if

	while curr != NULL
	begin
		if prev != NULL begin
			if prev < new and new < curr begin
				# nesse caso, new é algum nó do meio
				join[prev, new]
				join[new, curr]
			end if
		end if
		set prev = curr
		set curr = (curr+FL_NEXT_OFFSET)@ptr
	end while

	# nesse caso, new é o ultimo nó
	join[curr, new]
end proc

proc join[prev:ptr, new:ptr]
var prev_size
begin
	set prev_size = (prev+FL_SIZE_OFFSET)@i64
	if prev+prev_size:ptr == new begin
		# podemos fazer COALESCING aqui
		set (prev+FL_SIZE_OFFSET)@i64 = prev_size + (new+FL_SIZE_OFFSET)@i64
		set (prev+FL_NEXT_OFFSET)@ptr = (new+FL_NEXT_OFFSET)@ptr
		return
	end if

	set (prev+FL_NEXT_OFFSET)@ptr = new
end proc

proc Init
begin
	clear_words[Heap, Heap.size]
	clear_bytes[ByteMap, ByteMap.size]

	set Head@ptr = Heap
	set (Heap+FL_SIZE_OFFSET)@i64 = Heap.size
	set (Heap+FL_NEXT_OFFSET)@ptr = NULL
end proc

proc PrintMap[]
var i
begin
	set i = 0
	put_char['[']
	while i < Heap.size/WORD
	begin
		if (ByteMap+i:ptr)@i8 == EMPTY
		begin
			put_char[' ']
		end if
		if (ByteMap+i:ptr)@i8 == FULL
		begin
			put_char['X']
		end if
		set i += 1
	end while
	put_char[']']
	put_char['\n']
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
