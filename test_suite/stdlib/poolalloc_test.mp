from io import print

from constants
import NULL

from arrays import PtrA

from
	poolalloc
import all

from
	ioutil
import
	put_int, put_char, put_i32, put_ln, oom

from
	buffutil
import
	clear_bytes, set_buff

data begin
	Buff1 [1<<16];
	Buff2 [1<<16];
end 

const NUM_POINTERS = 2048
data Test:PtrA [NUM_POINTERS]

proc main
begin
	base_test[];
	enum_allocs[];
	#TODO: Boundary tests
end

proc base_test
var a:ptr, i:i32, out:ptr, ok:bool, pool:Pool
begin
	clear_bytes[Test:ptr, sizeof[Test]];
	set pool = init[Buff1, sizeof[Buff1], 16];

	set i = 0;
	while i < NUM_POINTERS begin
		set out, ok = alloc[pool];
		if not ok begin
			oom[];
		end
		set out@i64 = 0xDEADBEEFl;
		set (out+sizeof[i64])@i64 = 0xDEADBEEFl;
		set Test[i]->Ptr = out;
		set i += 1;
	end

	set i = 0;
	while i < NUM_POINTERS begin
		if i % 2 == 0 begin
			free[pool, Test[i]->Ptr];
		end
		set i += 1;
	end

	set i = NUM_POINTERS-1;
	while 0 <= i begin
		if i % 2 != 0 begin
			free[pool, Test[i]->Ptr];
		end
		set i -= 1;
	end

	if not empty[pool] begin
		failed[11ss];
	end
end

proc corrupt[a:ptr, size:i32]
begin
	set_buff[a, size, ~1ss];
end

const begin
	SIZE1 = 16;
	SIZE2 = 24;
end

proc enum_allocs
var n1, n2, n3:i32
begin
	set n1 = 1;
	set n2 = 1;
	set n3 = 1;
	do begin
		do begin
			do begin
				clear_bytes[Test:ptr, sizeof[Test]];
				param_allocs[n1, n2, n3];
				set n3++;
			end while n3 < 3;
			set n2++;
		end while n2 < 10;
		set n1++;
	end while n1 < 10;
end

proc param_allocs[n1, n2, n3:i32]
var i:i32, pool1:Pool, a:ptr, ok:bool
begin
	set pool1 = init[Buff1, sizeof[Buff1], SIZE1*n3];

	set i = 0;
	do begin
		set a = NULL;
		if i % 2 == 0 begin
			set a, ok = alloc[pool1];
			if not ok begin
				oom[];
			end
		end

		if a != NULL begin
			corrupt[a, SIZE1*n3]; # SIZE1 < SIZE2 :)
			set Test[i]->Ptr = a;
 		end

		if i % n1 == 0 begin
			free_n[pool1, n2];
		end
		set i += 1;
	end while i < NUM_POINTERS;

	free_n[pool1, NUM_POINTERS];
	if not empty[pool1] begin
		failed[12ss];
	end
end

proc free_n[pool1:Pool, n:i32]
var i:i32, a:ptr
begin
	set i = 0;
	while i < NUM_POINTERS begin
		if n <= 0 begin
			return;
		end
		set a = Test[i]->Ptr;
		if a != NULL begin
			free[pool1, a];
			set Test[i]->Ptr = NULL;
			set n -= 1;
		end
		set i += 1;
	end
end

data err "error: "

proc failed[code:i8]
begin
	print[err, sizeof[err]];
	put_int[code:i64];
	put_char['\n'];
	exit code;
end
