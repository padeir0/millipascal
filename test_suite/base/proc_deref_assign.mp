data buff [512]

proc main
var p:ptr
begin
	set p = middle[];
	set p@i32 = square[2];
	if p@i32 != 4 begin
		exit 1ss;
	end
end

proc middle[] ptr
begin
	return buff + 256;
end

proc square[n:i32] i32
begin
	return n * n;
end
