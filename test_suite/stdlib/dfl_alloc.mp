from io import print
from buffutil import clear_bytes, clear_words
from ioutil import put_int, put_ln, put_char, fatal, debug
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

const F_HEADER_SIZE = 8

# offset negativo
const F_OBJ_SIZE_OFFSET = 8

# cada objeto no backheap vai ter um header de 8 bytes
#         v ptr
#     [   | ..... ]
#       ^
#       ├── 1 byte, offset -8: Tamanho do objeto em WORDs
#       ├── 1 byte, offset -7: Metadados pro GC sobre a natureza do objeto
#       ├── 2 bytes, offset -6: Lenght para bigint ou Tag pra tipos de união
#       └── 4 bytes, offset -4: Bitmap de ponteiros

# offset negativo
const begin
	B_HEADER_SIZE = 8;

	B_OBJ_SIZE_OFFSET = 8;
	B_OBJ_META_OFFSET = 7;
	B_OBJ_STUFF_OFFSET = 6;
	B_OBJ_BITMAP_OFFSET = 4;
end

const begin
	NODE_SIZE = 24;

	FL_SIZE_OFFSET = 0;
	FL_PREV_OFFSET = 8;
	FL_NEXT_OFFSET = 16;
end

data begin
	Heap [1<<20];# 1MB (wow)
	ByteMap [(sizeof[Heap])/8];

	Head [sizeof[ptr]]; # first node
	Tail [sizeof[ptr]]; # last node
	FrontLimit [sizeof[ptr]]; # limite do frontheap
	BackLimit [sizeof[ptr]];  # limit do backheap
end

data begin
	UNREACHABLE "code should not reach here!\n";
	OUTOFBOUNDS "pointer to be freed is out of heap bounds\n";
	OUTOFLIMITS "pointer to be freed is out of back or front limit bounds\n";
	OBJ_TOO_BIG "object is too big, max of 255 words\n";
end

# size é em bytes, não em words
proc alloc_f[size:i64] ptr
var out:ptr, alloc_size:i64
begin
	set size += F_HEADER_SIZE;
	# alinhamento
	if size%8 != 0 begin
		set size = size + (8-size%8);
	end
	# os objetos precisam de ter espaço pra colocar
	# um nó quando forem dealocados
	if size < NODE_SIZE begin
		set size = NODE_SIZE;
	end

	set out, alloc_size = getNodeForward[size];
	if out == NULL begin
		return NULL;
	end

	if out + alloc_size > FrontLimit@ptr begin
		set FrontLimit@ptr = out + alloc_size;
	end
	
	set out += F_HEADER_SIZE;
	
	# seta o header do objeto
	set (out-F_OBJ_SIZE_OFFSET)@u32 = (alloc_size>>3):u32;

	setMap[out-F_HEADER_SIZE, alloc_size, FULL];

	return out;
end

proc alloc_b[size:i64, meta:u8, stuff:u16, bitmap:u32] ptr
var out:ptr, alloc_size:i64
begin
	set size += B_HEADER_SIZE;
	# alinhamento
	if size%8 != 0 begin
		set size = size + (8-size%8);
	end
	# os objetos precisam de ter espaço pra colocar
	# um nó quando forem dealocados
	if size < NODE_SIZE begin
		set size = NODE_SIZE;
	end

	set out, alloc_size = getNodeBackwards[size];
	if out == NULL begin
		return NULL;
	end

	if (alloc_size>>3) > 255 begin
		fatal[OBJ_TOO_BIG, sizeof[OBJ_TOO_BIG]];
	end
	
	if out < BackLimit@ptr begin
		set BackLimit@ptr = out;
	end
	set out += B_HEADER_SIZE;
	
	# seta o header do objeto
	set (out-B_OBJ_SIZE_OFFSET)@u8 = (alloc_size>>3):u8;
	set (out-B_OBJ_META_OFFSET)@u8 = meta;
	set (out-B_OBJ_STUFF_OFFSET)@u16 = stuff;
	set (out-B_OBJ_BITMAP_OFFSET)@u32 = bitmap;

	setMap[out-B_HEADER_SIZE, alloc_size, FULL];

	return out;
end

# precisamos retornar, junto com o ponteiro pro nó alocado,
# o tamanho do nó, já que pode ser que ele seja maior que o
# tamanho pedido
proc getNodeForward[size:i64] ptr, i64
var curr, next, prev:ptr,
	curr_size:i64
begin
	if Head@ptr == NULL begin
		return NULL, 0;
	end

	set curr = Head@ptr;
	set curr_size = 0;

	set next = NULL;
	set prev = NULL;

	while curr != NULL begin
		set curr_size = (curr+FL_SIZE_OFFSET)@i64;
		set next = (curr+FL_NEXT_OFFSET)@ptr;

		if curr_size == size begin
			return pop[prev, curr, next], curr_size;
		end
		if curr_size > size begin
			# se alocarmos um objeto e o espaço que sobrar
			# não ser suficiente pra um nó,
			# alocamos o espaço inteiro sem quebrar
			if curr_size - size < NODE_SIZE begin
				return pop[prev, curr, next], curr_size;
			end
			return splitFront[prev, curr, next, size], size;
		end

		set prev = curr;
		set curr = next;

		if BackLimit@ptr <= curr begin
			return NULL, 0;
		end
	end

	return NULL, 0;
end

proc getNodeBackwards[size:i64] ptr, i64
var curr, prev, next:ptr,
	curr_size:i64
begin
	if Tail@ptr == NULL begin
		return NULL, 0;
	end

	set next = NULL;
	set prev = NULL;

	set curr = Tail@ptr;
	set curr_size = 0;

	while curr != NULL begin
		set curr_size = (curr+FL_SIZE_OFFSET)@i64;
		set prev = (curr+FL_PREV_OFFSET)@ptr;
		if curr + curr_size <= FrontLimit@ptr begin
			return NULL, 0;
		end

		if curr_size == size begin
			return pop[prev, curr, next], curr_size;
		end
		if curr_size > size begin
			if curr_size - size < NODE_SIZE begin
				return pop[prev, curr, next], curr_size;
			end
			return splitBack[prev, curr, next, size], size;
		end

		set next = curr;
		set curr = prev;
	end

	return NULL, 0;
end

proc pop[prev, curr, next:ptr] ptr
begin
	if prev != NULL begin
		set (prev+FL_NEXT_OFFSET)@ptr = next;
	end else begin
		if Head@ptr == Tail@ptr begin # nesse caso, temos que atualizar a tail tbm
			set Tail@ptr = next;
		end
        set Head@ptr = next;
	end

	if next != NULL begin
		set (next+FL_PREV_OFFSET)@ptr = prev;
	end else begin
		if Head@ptr == Tail@ptr begin
			set Head@ptr = next;
		end
        set Tail@ptr = prev;
	end

	return curr;
end

# divide o nó, alocando o espaço no começo
#
#   v alocado
# [xxxxx|           ]
#            ^ livre
proc splitFront[prev, curr, next:ptr, requested_size:i64] ptr
var newnode:ptr,
	newnode_size, curr_size:i64
begin
	set curr_size = (curr+FL_SIZE_OFFSET)@i64;
	set newnode_size = curr_size - requested_size;
	set newnode = curr + requested_size;

	set (newnode + FL_SIZE_OFFSET)@i64 = newnode_size;
	set (curr + FL_SIZE_OFFSET)@i64 = requested_size;

	set (newnode + FL_NEXT_OFFSET)@ptr = next;
	if next != NULL begin
		set (next + FL_PREV_OFFSET)@ptr = newnode;
	end else begin
		# next == null implica que Tail@ptr == curr
		set Tail@ptr = newnode;
	end

	set (curr + FL_NEXT_OFFSET)@ptr = newnode;
	set (newnode + FL_PREV_OFFSET)@ptr = curr;

	return pop[prev, curr, newnode];
end

# divide o nó, alocando o espaço no fim
#
#   v livre
# [            |xxxxxx]
#                ^ alocado
proc splitBack[prev, curr, next:ptr, requested_size:i64] ptr
var newnode:ptr,
	newnode_size, remaining_size, curr_size:i64
begin
	set curr_size = (curr+FL_SIZE_OFFSET)@i64;
	set newnode_size = requested_size;
	set remaining_size = curr_size - requested_size;
	set newnode = curr + remaining_size;

	set (newnode + FL_SIZE_OFFSET)@i64 = newnode_size;
	set (curr + FL_SIZE_OFFSET)@i64 = remaining_size;

	set (newnode + FL_NEXT_OFFSET)@ptr = next;
	if next != NULL begin
		set (next + FL_PREV_OFFSET)@ptr = newnode;
	end else begin
		# next == null implica que Tail@ptr == curr
		set Tail@ptr = newnode;
	end

	set (curr + FL_NEXT_OFFSET)@ptr = newnode;
	set (newnode + FL_PREV_OFFSET)@ptr = curr;

	return pop[curr, newnode, next];
end

proc free[obj:ptr]
var obj_size:i64,
	new, prev, curr, leftmost, rightmost:ptr,
	isFront:bool,
begin
	if obj < FrontLimit@ptr begin
		set new = obj - F_HEADER_SIZE;
		set obj_size = ((obj-F_OBJ_SIZE_OFFSET)@u32):i64 << 3;
		set isFront = true;
	end elseif obj > BackLimit@ptr begin
		set new = obj - B_HEADER_SIZE;
		set obj_size = ((obj-B_OBJ_SIZE_OFFSET)@u16):i64 << 3;
		set isFront = false;
	end else begin
		fatal[OUTOFLIMITS, sizeof[OUTOFLIMITS]];
	end

	if new < Heap or new > Heap + sizeof[Heap] begin
		fatal[OUTOFBOUNDS, sizeof[OUTOFBOUNDS]];
	end

	setMap[new, obj_size, EMPTY];

	set (new+FL_SIZE_OFFSET)@i64 = obj_size;
	set (new+FL_NEXT_OFFSET)@ptr = NULL;
	set (new+FL_PREV_OFFSET)@ptr = NULL;

	if Head@ptr == NULL or Tail@ptr == NULL begin
		set Head@ptr = new;
		set Tail@ptr = new;
		updateLimits[new, new + obj_size];
		return;
	end

	if new < Head@ptr begin
		# nesse caso, new é o primeiro nó da lista
		# e vai virar o novo Head
		prepend[new];
		updateLimits[new, new + obj_size];
		return;
	end
	if Tail@ptr < new begin
		# nesse caso, new é o ultimo nó da lista
		# e vai virar o novo Tail
		append[new];
		updateLimits[new, new + obj_size];
		return;
	end

	set prev = NULL;
	set curr = Head@ptr;

	while curr != NULL
	begin
		if prev != NULL begin
			if prev < new and new < curr begin
				# nesse caso, new é algum nó do meio
				set leftmost, rightmost = join[prev, new, curr];
				updateLimits[leftmost, rightmost];
				return;
			end
		end
		set prev = curr;
		set curr = (curr+FL_NEXT_OFFSET)@ptr;
	end

	# nesse caso, new é o ultimo nó (não deveria chegar aqui)
	# append[prev, new]
	# if not isFront begin
	# 	set BackLimit@ptr = new+obj_size
	# end
	fatal[UNREACHABLE, sizeof[UNREACHABLE]];
end

proc updateLimits[leftmost, rightmost:ptr]
begin
	if leftmost <= BackLimit@ptr and BackLimit@ptr < rightmost begin
		set BackLimit@ptr = rightmost;
	end
	if FrontLimit@ptr <= rightmost and leftmost < FrontLimit@ptr begin
		set FrontLimit@ptr = leftmost;
	end
end

proc append[new:ptr]
var size:i64
begin
	set size = (Tail@ptr+FL_SIZE_OFFSET)@i64;
	if Tail@ptr+size == new begin
		# COALESCING, append
		set size += (new+FL_SIZE_OFFSET)@i64 ;
		set (Tail@ptr+FL_SIZE_OFFSET)@i64 = size;
		return;
	end
	set (Tail@ptr + FL_NEXT_OFFSET)@ptr = new;
	set (new+FL_PREV_OFFSET)@ptr = Tail@ptr;
	set Tail@ptr = new;
end

proc prepend[new:ptr]
var size:i64, next:ptr
begin
	set size = (new+FL_SIZE_OFFSET)@i64;
	if new + size == Head@ptr begin
		# COALESCING, prepend
		set next = (Head@ptr + FL_NEXT_OFFSET)@ptr;
		set (new+FL_SIZE_OFFSET)@i64 = size + (Head@ptr + FL_SIZE_OFFSET)@i64;
		set (new+FL_NEXT_OFFSET)@ptr = next;
		if next != NULL begin
			set (next+FL_PREV_OFFSET)@ptr = new;
		end else begin
			set Tail@ptr = new;
		end
		set Head@ptr = new;
		return;
	end

	set next = Head@ptr;
	set (new+FL_NEXT_OFFSET)@ptr = next;
	set (next+FL_PREV_OFFSET)@ptr = new;
	set Head@ptr = new;
end

proc join[prev, new, curr:ptr] ptr, ptr
var size:i64, next:ptr
begin
	set size = (prev+FL_SIZE_OFFSET)@i64;
	if prev+size == new begin
		# COALESCING, append
		set size = size + (new+FL_SIZE_OFFSET)@i64;

		if prev+size == curr begin
			set next = (curr+FL_NEXT_OFFSET)@ptr;
			# nesse caso, prev, new e curr são adjacentes
			set size += (curr+FL_SIZE_OFFSET)@i64;
			set (prev+FL_SIZE_OFFSET)@i64 = size;

			set (prev+FL_NEXT_OFFSET)@ptr = next;
			if next != NULL begin
				set (next+FL_PREV_OFFSET)@ptr = prev;
			end else begin
				set Tail@ptr = prev;
			end
		end else begin
			# aqui, somente prev e new são adjacentes
			set (prev+FL_SIZE_OFFSET)@i64 = size;
		end
		return prev, prev+size;
	end

	set size = (new+FL_SIZE_OFFSET)@i64;
	if new + size == curr begin
		# COALESCING, prepend
		set next = (curr+FL_NEXT_OFFSET)@ptr;

		set (prev+FL_NEXT_OFFSET)@ptr = new;
		set (new+FL_PREV_OFFSET)@ptr = prev;

		set size += (curr+FL_SIZE_OFFSET)@i64;
		set (new+FL_SIZE_OFFSET)@i64 = size;

		set (new+FL_NEXT_OFFSET)@ptr = next;
		if next != NULL begin
			set (next+FL_PREV_OFFSET)@ptr = new;
		end else begin
			set Tail@ptr = new;
		end
		return new, new+size;
	end

	set (prev+FL_NEXT_OFFSET)@ptr = new;
	set (new+FL_PREV_OFFSET)@ptr = prev;

	set (new+FL_NEXT_OFFSET)@ptr = curr;
	set (curr+FL_PREV_OFFSET)@ptr = new;
	return new, new+size;
end

proc init
begin
	clear_words[Heap, sizeof[Heap]];
	clear_bytes[ByteMap, sizeof[ByteMap]];

	set Head@ptr = Heap;
	set Tail@ptr = Heap;

	set FrontLimit@ptr = Heap;
	set BackLimit@ptr = Heap + (sizeof[Heap]);
	set (Heap+FL_SIZE_OFFSET)@i64 = sizeof[Heap];
	set (Heap+FL_NEXT_OFFSET)@ptr = NULL;
end

proc setMap[obj:ptr, size:i64, b:i8]
var start:ptr, i, size_words:i64
begin
	set size_words = (size>>3);
	set start = ByteMap + ((obj:i64 - Heap:i64) / WORD);
	set i = 0;
	while i < size_words begin
		set (start+i)@i8 = b;
		set i += 1;
	end
end

data MSG_TOTAL_FREE "free: "
data MSG_FLIMIT ", FrontLimit: "
data MSG_BLIMIT ", BackLimit: "

proc print_list[]
var curr:ptr
begin
	set curr = Head@ptr;
	while curr != NULL begin
		put_char['{'];
		put_4int[curr:i64,
		         (curr+FL_SIZE_OFFSET)@i64,
				 (curr+FL_PREV_OFFSET)@i64,
				 (curr+FL_NEXT_OFFSET)@i64,
				];
		put_char['}'];
		put_char[' '];
		set curr = (curr+FL_NEXT_OFFSET)@ptr;
	end
	put_char[' '];

	print[MSG_TOTAL_FREE, sizeof[MSG_TOTAL_FREE]];
	put_int[get_total_free[]];

	print[MSG_FLIMIT, sizeof[MSG_FLIMIT]];
	put_int[FrontLimit@i64];

	print[MSG_BLIMIT, sizeof[MSG_BLIMIT]];
	put_int[BackLimit@i64];

	put_ln[];
end

proc put_4int[a, b, c, d:i64]
begin
	put_int[a];
	put_char[','];
	put_char[' '];
	put_int[b];
	put_char[','];
	put_char[' '];
	put_int[c];
	put_char[','];
	put_char[' '];
	put_int[d];
end

proc get_total_free[] i64
var curr:ptr, total_free:i64
begin
	set curr = Head@ptr;
	set total_free = 0;
	while curr != NULL begin
		set total_free += (curr+FL_SIZE_OFFSET)@i64;
		set curr = (curr+FL_NEXT_OFFSET)@ptr;
	end
	return total_free;
end

proc print_map[]
var i:i64
begin
	set i = 0;
	put_char['['];
	while i < (sizeof[Heap])/WORD begin
		if (ByteMap+i)@i8 == EMPTY begin
			put_char[' '];
		end
		if (ByteMap+i)@i8 == FULL begin
			put_char['X'];
		end
		set i += 1;
	end
	put_char[']'];
	put_ln[];
end

# validates if we're back to the initial position
proc validate_init
begin
	if get_total_free[] != sizeof[Heap] begin
		exit 2ss;
	end
	if Head@ptr != Tail@ptr begin
		exit 3ss;
	end
	if FrontLimit@ptr != Heap begin
		exit 4ss;
	end
	if BackLimit@ptr != Heap+(sizeof[Heap]) begin
		exit 5ss;
	end
end
