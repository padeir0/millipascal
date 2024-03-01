from intutil import min
export map_buff, set_buff, comp, copy, clear_bytes, clear_words

proc main
begin
	#TODO: Write tests
	exit 0ss
end

proc map_buff[p:ptr, size:i64, transform:proc[i8]i8]
var a:i64
begin
	set a = 0
	while a < size
	begin
		set (p+a)@i8 = transform[ (p+a)@i8 ]
		set a += 1
	end
end

proc clear_bytes[start:ptr, size:i64]
begin
	set_buff[start, size, 0ss]
end

proc clear_words[start:ptr, size:i64]
var p:ptr, i:i64
begin
	set p = start
	set i = 0
	while i < size
	begin
		set (p+i)@i64 = 0
		set i += 8
	end
end

proc set_buff[p:ptr, size:i64, b:i8]
var a:i64
begin
	set a = 0
	while a < size
	begin
		set (p+a)@i8 = b
		set a += 1
	end
end

proc comp[p1:ptr, size1:i64, p2:ptr, size2:i64] bool
var i:i64
begin
	if size1 != size2
	begin
		return false
	end

	set i = 0
	while i < size1
	begin
		if (p1 + i)@i8 != (p2 + i)@i8
		begin
			return false
		end
		set i += 1
	end
	return true
end

proc copy[source:ptr, source_size:i64, dest:ptr, dest_size:i64]
var smallest, i:i64
begin
	set smallest = min[source_size, dest_size]
	set i = 0
	while i < smallest
	begin
		set (dest + i)@i8 = (source + i)@i8
		set i += 1
	end
end
