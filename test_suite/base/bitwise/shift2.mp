proc main
var i, a, b:i32
begin
	set i = 1;
	while i < 29 begin
		set a = pow[2, i];
		set b = 1<<i;
		if a != b begin
			exit (b):i8;
		end
		set i++;
	end
end

proc pow[a, b:i32] i32
var out:i32
begin
	set out = 1;
	while 0 < b begin
		set out *= a;
		set b--;
	end
	return out;
end
