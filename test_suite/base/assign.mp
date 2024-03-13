proc main
var a, b:i64
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

proc f[a:i64]
begin
	# does nothing UwU
end
