from io import fatal
from ioutil import oom, put_i32, put_ln
from malloc import all
from conv import I32MAX
from arrays import PtrA
from constants import NULL
from memsizes import _1KB

data begin
    pool1  [4  * _1KB];
    pool2  [8  * _1KB];
    pool3  [16 * _1KB];
    backup [64 * _1KB];
end

proc setup[]
begin
    set_pool[pool1, sizeof[pool1], 0, 16];
    set_pool[pool2, sizeof[pool2], 16, 32];
    set_pool[pool3, sizeof[pool3], 32, 64];
    set_heap[backup, sizeof[backup], 0, I32MAX];
end

proc main
var i:i32
begin
	setup[];
	set i = 1;
    while i < 4 begin
        base_test[i];
        set i += 1;
    end
	test_full_pool[16];
	test_full_pool[32];
	test_full_pool[64];
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

const TOTAL_TEST_PTRS = 512
data test:PtrA [TOTAL_TEST_PTRS]

proc base_test[n:i32]
var i, size:i32, a:ptr, ok:bool
begin
	set i = 0;
	while i < TOTAL_TEST_PTRS begin
		if i % 5 == 0 begin
			set size = 8*n;
		end elseif i % 5 == 1 begin
			set size = 16*n;
		end elseif i % 5 == 2 begin
			set size = 24*n;
		end elseif i % 5 == 3 begin
			set size = 32*n;
		end elseif i % 5 == 4 begin
			set size = 125*n;
		end

		set a, ok = malloc[size];

		if not ok begin
			put_i32[size];
			put_ln[];
			oom[];
		end

		corrupt[a, size];
		set test[i]->Ptr = a;

		if i % 4 == 0 begin
			free_n[1*(n/2)];
		end elseif i % 4 == 3 begin
			free_n[2*(n/2)];
		end
		set i += 1;
	end

	free_n[TOTAL_TEST_PTRS];
	if not empty[] begin
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
		set a = test[i]->Ptr;
		if a != NULL begin
			free[a];
			set test[i]->Ptr = NULL;
			set n -= 1;
		end
		set i += 1;
	end
end

# tests whether the backup heap is used after the pool is full
proc test_full_pool[size:i32]
var i:i32, a:ptr, ok:bool
begin
	set i = 0;
	while i < TOTAL_TEST_PTRS begin
		set a, ok = malloc[size];
		if not ok begin
			oom[];
		end
		corrupt[a, size];
		set test[i]->Ptr = a;
		set i++;
	end

	free_n[TOTAL_TEST_PTRS];
	if not empty[] begin
		exit 12ss;
	end
end
