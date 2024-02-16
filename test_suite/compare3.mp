data hello "Hello"

proc main
begin
	if hello@i8 != 'H'
	begin
		exit 1ss
	end if

	if hello@i8 > 'M'
	begin
		exit 1ss
	end if

	if hello@i8 < 'D'
	begin
		exit 1ss
	end if

	if not(hello@i8 >= hello@i8)
	begin
		exit 1ss
	end if

	if not(hello@i8 <= hello@i8)
	begin
		exit 1ss
	end if
end proc
