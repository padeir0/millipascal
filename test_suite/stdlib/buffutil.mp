from intutil import min
export map_buff, set_buff, comp, copy, clear_bytes, clear_words

proc main
begin
	exit 0ss;
end

proc map_buff[p:ptr, size:i32, transform:proc[i8][i8]]
var a:i32
begin
	set a = 0;
	while a < size
	begin
		set (p+a)@i8 = transform[ (p+a)@i8 ];
		set a += 1;
	end
end

proc clear_bytes[start:ptr, size:i32]
begin
	set_buff[start, size, 0ss];
end

proc clear_words[start:ptr, size:i32]
var p:ptr, i:i32
begin
	set p = start;
	set i = 0;
	while i < size
	begin
		set (p+i)@i32 = 0;
		set i += 8;
	end
end

proc set_buff[p:ptr, size:i32, b:i8]
var a:i32
begin
	set a = 0;
	while a < size
	begin
		set (p+a)@i8 = b;
		set a += 1;
	end
end

proc comp[p1:ptr, size1:i32, p2:ptr, size2:i32] bool
var i:i32
begin
	if size1 != size2
	begin
		return false;
	end

	set i = 0;
	while i < size1
	begin
		if (p1 + i)@i8 != (p2 + i)@i8
		begin
			return false;
		end
		set i += 1;
	end
	return true;
end

proc copy[source:ptr, source_size:i32, dest:ptr, dest_size:i32]
var smallest, i:i32
begin
	set smallest = min[source_size, dest_size];
	set i = 0;
	while i < smallest
	begin
		set (dest + i)@i8 = (source + i)@i8;
		set i += 1;
	end
end
