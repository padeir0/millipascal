proc main
var result:i64
begin
	set result = square[5] + 1;
	if result != 26 begin
		exit 1ss
	end
end

proc square[a:i64] i64
begin
	return a * a;
end
