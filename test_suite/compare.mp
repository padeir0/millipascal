proc main
begin
	if 'a' != 'a'
	begin
		exit 1r
	end if

	if 'a' > 'b'
	begin
		exit 1r
	end if

	if 'b' < 'a'
	begin
		exit 1r
	end if

	if not('a' >= 'a')
	begin
		exit 1r
	end if
end proc
