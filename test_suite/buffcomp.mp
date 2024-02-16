data first "first"
data second "second"

proc main
begin
	if not equals[first, first.size, first, first.size]
	begin
		exit 1ss
	end if
	if not equals[second, second.size, second, second.size]
	begin
		exit 1ss
	end if
	if equals[first, first.size, second, second.size]
	begin
		exit 1ss
	end if
end proc

proc equals[p1:ptr, size1:i64, p2:ptr, size2:i64] bool
var i
begin
	if size1 != size2
	begin
		return false
	end if

	set i = 0
	while i < size1
	begin
		if (p1 + i:ptr)@i8 != (p2 + i:ptr)@i8
		begin
			return false
		end if
		set i += 1
	end while
	return true
end proc

