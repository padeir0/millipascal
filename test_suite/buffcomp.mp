data first "first"
data second "second"

proc main
begin
	if not equals[first, sizeof first, first, sizeof first]
	begin
		exit 1ss
	end
	if not equals[second, sizeof second, second, sizeof second]
	begin
		exit 1ss
	end
	if equals[first, sizeof first, second, sizeof second]
	begin
		exit 1ss
	end
end

proc equals[p1:ptr, size1:i64, p2:ptr, size2:i64] bool
var i:i64
begin
	if size1 != size2
	begin
		return false
	end

	set i = 0
	while i < size1
	begin
		if (p1 + i:ptr)@i8 != (p2 + i:ptr)@i8
		begin
			return false
		end
		set i += 1
	end
	return true
end

