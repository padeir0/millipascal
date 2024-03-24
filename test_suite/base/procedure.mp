proc main
var a, b, c:i32
begin
	set a, b, c = P[1, 2, 3];
	if a != 1 or b != 2 or c != 3
	begin
		exit 1ss;
	end
end

proc P
	[arg1, arg2, arg3:i32]
	i32, i32, i32
var a, b, c:i32
begin
	set a = arg1;
	set b = arg2;
	set c = arg3;
	return a, b, c;
end
