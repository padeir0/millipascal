from io import fatal
from ioutil import oom, put_i32, put_ln
from malloc import all
from conv import I16MAX
from arrays import PtrA
from constants import NULL

const begin
    _64KB = 1<<16;
    _1MB = 1<<20;
end

data begin
    pool1  [_64KB];
    pool2  [_64KB];
    pool3  [_64KB];
    backup [_1MB];
end

proc setup[]
begin
    setPool[pool1, sizeof[pool1], 0s, 16s];
    setPool[pool2, sizeof[pool2], 16s, 32s];
    setPool[pool3, sizeof[pool3], 32s, 64s];
    setHeap[backup, sizeof[backup], 0s, I16MAX];
end

proc main
var i:i32
begin
	setup[];
	set i = 1;
    while i < 256 begin
        base_test[i];
        set i += 11;
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

const TOTAL_TEST_PTRS = 256
data Test:PtrA [TOTAL_TEST_PTRS]

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
		set Test[i]->Ptr = a;

		if i % 4 == 0 begin
			free_n[1*n];
		end elseif i % 4 == 3 begin
			free_n[2*n];
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
		set a = Test[i]->Ptr;
		if a != NULL begin
			free[a];
			set Test[i]->Ptr = NULL;
			set n -= 1;
		end
		set i += 1;
	end
end
