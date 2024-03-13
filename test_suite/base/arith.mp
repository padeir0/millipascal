proc main
var a, b:i64
begin
	set a = 1;
	set b = 2;
	set a = b - a;
	if a != 1 begin
		exit 1ss;
	end
end
