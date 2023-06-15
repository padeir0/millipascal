proc main
begin
	if 'a' != 'a'
	begin
		exit 1ss
	end if

	if 'a' > 'b'
	begin
		exit 1ss
	end if

	if 'b' < 'a'
	begin
		exit 1ss
	end if

	if not('a' >= 'a')
	begin
		exit 1ss
	end if
end proc
