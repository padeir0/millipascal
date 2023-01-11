from intutil import min
export mapbuff, setbuff, comp, copy

proc mapbuff[p:ptr, size:i64, transform:proc[i8]i8]
var a
begin
	set a = 0
	while a < size
	begin
		set (p+a:ptr)@i8 = transform[ (p+a:ptr)@i8 ]
		set a += 1
	end while
end proc

proc setbuff[p:ptr, size:i64, b:i8]
var a
begin
	set a = 0
	while a < size
	begin
		set (p+a:ptr)@i8 = b
		set a += 1
	end while
end proc

proc comp[p1:ptr, size1:i64, p2:ptr, size2:i64] bool
var i
begin
	if size1 != size2
	begin
		return false
	end if

	set i = 0
	while i < size1
	begin
		if (p1 + i:ptr)@i8 != (p2 + i:ptr)@i8
		begin
			return false
		end if
		set i += 1
	end while
	return true
end proc

proc copy[source:ptr, source_size:i64, dest:ptr, dest_size:i64]
var smallest, i
begin
	set smallest = min[source_size, dest_size]
	set i = 0
	while i < smallest
	begin
		set (dest + i:ptr)@i8 = (source + i:ptr)@i8
		set i += 1
	end while
end proc
