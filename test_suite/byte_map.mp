memory buff "am i uppercase yet?\n"

proc main
begin
	byte_map[buff, buff.size, upper_case]
	write[buff, buff.size]
end proc

proc byte_map[b:ptr, bsize:i64, op:proc[i8]i8]
var i:ptr
begin
	set i = 0p
	while i < bsize:ptr
	begin
		set (b+i)@i8 = op[(b+i)@i8]
		set i += 1p
	end while
end proc

proc upper_case[a:i8] i8
begin
	if a >= 'a' and a <= 'z'
	begin
		return a - 32r
	end if
	return a
end proc
