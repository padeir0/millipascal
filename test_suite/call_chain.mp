proc main
var result:i64
begin
	set result = square[square[square[2]]];
end

proc square[n:i64] i64
begin
	return n * n;
end
