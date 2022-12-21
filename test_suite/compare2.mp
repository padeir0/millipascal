proc main
var a:i8
begin
	set a = 'a'
	if a != a and a != a
	begin
		exit 1r
	end if

	if a > 'b' and a > 'b'
	begin
		exit 1r
	end if

	if 'b' < a and 'b' < a
	begin
		exit 1r
	end if

	if not(a >= a) and not(a >= a)
	begin
		exit 1r
	end if

	if not(a <= a) and not(a <= a)
	begin
		exit 1r
	end if
end proc
