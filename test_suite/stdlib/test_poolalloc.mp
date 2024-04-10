from io import print

from
	poolalloc
import all

from
	ioutil
import
	put_int, put_char, put_i32, put_ln

data begin
	Buff1 [1<<16];
	Buff2 [1<<16];
end 

proc main
begin
	basetest[];
end

struct APtr begin
	Ptr:ptr;
end

const NUM_POINTERS = 64
data Test:APtr [NUM_POINTERS]

proc basetest
var a:ptr, i:i32, out:ptr, pool:Pool
begin
	set pool = init[Buff1, sizeof[Buff1], 16];
	set i = 0;
	while i < NUM_POINTERS begin
		set out = alloc[pool];
		set out@i64 = 0xDEADBEEFl;
		set (out+sizeof[i64])@i64 = 0xDEADBEEFl;
		set Test[i]->Ptr = out;
		set i += 1;
	end

	set i = 0;
	while i < NUM_POINTERS begin
		if i % 2 == 0 begin
			free[Test[i]->Ptr];
		end
		set i += 1;
	end

	set i = NUM_POINTERS-1;
	while 0 <= i begin
		if i % 2 != 0 begin
			free[Test[i]->Ptr];
		end
		set i -= 1;
	end

	if not empty[pool] begin
		failed[11ss];
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
