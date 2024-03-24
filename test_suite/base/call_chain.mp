proc main
var result:i32
begin
	set result = square[square[square[2]]];
	if result != 256 begin
		exit 1ss;
	end
end

proc square[n:i32] i32
begin
	return n * n;
end
