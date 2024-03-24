proc main
var a, b:i32
begin
	set a = 0;
	set a += 1;
	set a -= 1;
	set a = 2;
	set a *= 2;
	set a /= 2;
	set a = 7;
	set a %= 3;
	f[a];
	set b = a;
end

proc f[a:i32]
begin
	# does nothing UwU
end
