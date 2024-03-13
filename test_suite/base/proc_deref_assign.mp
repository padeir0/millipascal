data buff [512]

proc main
var p:ptr
begin
	set p = middle[];
	set p@i64 = square[2];
end

proc middle[] ptr
begin
	return buff + 256;
end

proc square[n:i64] i64
begin
	return n * n;
end
