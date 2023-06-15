proc main
begin
	if fact[5] != 120
	begin
		exit 1ss
	end if
end proc

proc fact[n] i64
var result
begin
	set result = 1
	while n > 0 begin
		set result *= n
		set n -= 1
	end while
	return result
end proc
