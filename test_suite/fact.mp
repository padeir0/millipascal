proc main
begin
	if fact[5] != 120 begin
		exit 1ss
	end
end

proc fact[n] i64
begin
	if n == 0 begin
		return 1;
	end
	return n * fact[n-1]
end
