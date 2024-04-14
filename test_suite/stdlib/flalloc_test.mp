from flalloc import all
from buffutil import clear_bytes, clear_words
from ioutil import put_int, put_char, debug, oom
from io import fatal
from constants import WORD, NULL

const SIZEBUFF = 1<<16;

data begin
    buff1 [SIZEBUFF];
    buff2 [SIZEBUFF];
end

struct APtr begin
	Ptr:ptr;
end

const TOTAL_TEST_PTRS = 256
data Test:APtr [TOTAL_TEST_PTRS]

proc main
var i:i32, h:Heap
begin
	set i = 1;
	while i <= 4 begin
		_alloc[i];
		set i += 1;
	end
end

proc corrupt[p:ptr, size:i32]
var i:i32
begin
	set i = 0;
	while i < size begin
		set (p+i)@u8 = i:u8;
		set i += 1;
	end
end

proc _alloc[n:i32]
var i:i32, a:ptr, h:Heap, size:i32
begin
	clear_bytes[Test:ptr, sizeof[Test]];
	set h = init[buff1, sizeof[buff1]];
	set i = 0;
	while i < TOTAL_TEST_PTRS begin
		if i % 5 == 0 begin
			set size = 16*n;
		end elseif i % 5 == 1 begin
			set size = 24*n;
		end elseif i % 5 == 2 begin
			set size = 48*n;
		end elseif i % 5 == 3 begin
			set size = 25*n;
		end elseif i % 5 == 4 begin
			set size = 33*n;
		end

		set a = alloc[h, size];
		if a != NULL begin
			corrupt[a, size];
			set Test[i]->Ptr = a;
		end else begin
			oom[];
		end

		if i % 4 == 0 begin
			free_n[1*n];
		end elseif i % 4 == 3 begin
			free_n[2*n];
		end
		set i += 1;
	end
	free_n[TOTAL_TEST_PTRS];

	if not empty[h] begin
		print_list[h];
		exit 11ss;
	end
end

proc free_n[n:i32]
var i:i32, a:ptr
begin
	set i = 0;
	while i < TOTAL_TEST_PTRS begin
		if n <= 0 begin
			return;
		end
		set a = Test[i]->Ptr;
		if a != NULL begin
			free[a];
			set Test[i]->Ptr = NULL;
			set n -= 1;
		end
		set i += 1;
	end
end

