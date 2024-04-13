from flalloc import all
from buffutil import clear_bytes, clear_words
from ioutil import put_int, put_char, fatal, debug, oom
from constants import WORD, NULL, FULL, EMPTY

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
	set i = 0;
	while i < 4 begin
		_alloc[i];
		set i += 1;
	end
end

proc _alloc[n:i32]
var i:i32, a:ptr, h:Heap
begin
	clear_words[Test:ptr, sizeof[Test]];
	set h = init[buff1, sizeof[buff1]];
	set i = 0;
	while i < TOTAL_TEST_PTRS begin
		set a = NULL;
		if i % 5 == 0 begin
			set a = alloc[h, 16*n];
		end elseif i % 5 == 1 begin
			set a = alloc[h, 24*n];
		end elseif i % 5 == 2 begin
			set a = alloc[h, 48*n];
		end elseif i % 5 == 3 begin
			set a = alloc[h, 25*n];
		end elseif i % 5 == 4 begin
			set a = alloc[h, 33*n];
		end

		if i % 4 == 0 begin
			free_n[1*n];
		end elseif i % 4 == 3 begin
			free_n[2*n];
		end

		if a != NULL begin
			set Test[i]->Ptr = a;
		end else begin
			oom[];
		end
		set i += 1;
	end
	free_n[TOTAL_TEST_PTRS];

	if not empty[h] begin
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

