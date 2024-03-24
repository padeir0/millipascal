proc main
var result:i32
begin
	set result = square[5] + 1;
	if result != 26 begin
		exit 1ss;
	end
end

proc square[a:i32] i32
begin
	return a * a;
end
