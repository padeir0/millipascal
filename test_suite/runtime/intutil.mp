export min, max, abs, abs_i32

proc main
begin
	if max[5, 2] != 5 begin
		exit 1ss
	end
	if max[~1, 1] != 1 begin
		exit 1ss
	end
	if max[0, 10] != 10 begin
		exit 1ss
	end
	if max[~2000000, ~100000] != ~100000 begin
		exit 1ss
	end

	if min[5, 2] != 2 begin
		exit 1ss
	end
	if min[~1, 1] != ~1 begin
		exit 1ss
	end
	if min[0, 10] != 0 begin
		exit 1ss
	end
	if min[~2000000, ~100000] != ~2000000 begin
		exit 1ss
	end
end

proc max[a, b] i64
begin
	if a >= b
	begin
		return a
	end 
	else
	begin
		return b
	end
end

proc min[a:i64, b:i64] i64
begin
	if a <= b
	begin
		return a
	end 
	else
	begin
		return b
	end
end

proc abs_i32[a:i32] i32
begin
	if a >= 0l begin
		return a
	end
	return ~a
end

proc abs[a:i64] i64
begin
	if a >= 0 begin
		return a
	end
	return ~a
end
