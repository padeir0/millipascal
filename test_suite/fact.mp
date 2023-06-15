proc main
begin
	if fact[5] != 120
	begin
		exit 1ss
	end if
end proc

proc fact[n] i64
begin
	if n == 0 
	begin
		return 1;
	end if
	return n * fact[n-1]
end proc
