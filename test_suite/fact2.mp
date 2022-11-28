proc fibb[n] i64
var result
begin
	set result = 1
	while n > 0 begin
		set result = result * n
		set n = n - 1
	end while
	return result
end proc
