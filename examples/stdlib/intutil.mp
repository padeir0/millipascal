export min, max

proc max[a, b] i64
begin
	if a >= b
	begin
		return a
	end 
	else
	begin
		return b
	end if
end proc

proc min[a, b] i64
begin
	if a <= b
	begin
		return a
	end 
	else
	begin
		return b
	end if
end proc
