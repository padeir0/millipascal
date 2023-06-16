proc main
var a:u16
begin
	set a = 1us;
	if a != 1us
	begin
		exit 1ss;
	end if

	set a -= 1us;
	if a != 0us
	begin
		exit 1ss
	end if

	set a -= 1us;
	if a != 65535us
	begin
		exit 1ss
	end if
end proc
