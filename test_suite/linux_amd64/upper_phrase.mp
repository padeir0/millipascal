from io import write
data buff "am i uppercase yet?\n"

proc main
begin
	byte_map[buff, sizeof[buff]];
	write[buff, sizeof[buff]];
end

proc byte_map[b:ptr, bsize:i64]
var i:i64
begin
	set i = 0;
	while i < bsize
	begin
		set (b+i)@i8 = upper_case[(b+i)@i8];
		set i += 1;
	end
end

proc upper_case[a:i8] i8
begin
	if a >= 'a' and a <= 'z'
	begin
		return a - 32ss;
	end
	return a;
end
