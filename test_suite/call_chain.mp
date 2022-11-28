proc main
var result
begin
	set result = square[square[square[2]]];
end proc

proc square[n] i64
begin
	return n * n;
end proc
