proc main
begin
	if fact[5] != 120 begin
		exit 1ss;
	end
end

proc fact[n:i64] i64
var result:i64
begin
	set result = 1;
	while n > 0 begin
		set result *= n;
		set n -= 1;
	end
	return result;
end
