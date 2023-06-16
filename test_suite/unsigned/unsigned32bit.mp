proc main
var a:u32
begin
	set a = 1ul;
	if a != 1ul
	begin
		exit 1ss;
	end if

	set a -= 1ul;
	if a != 0ul
	begin
		exit 1ss
	end if

	set a -= 1ul;
	if a != 4294967295ul
	begin
		exit 1ss
	end if
end proc
