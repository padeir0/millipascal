proc main
var a:u64
begin
	set a = 1u;
	if a != 1u
	begin
		exit 1ss;
	end if

	set a -= 1u;
	if a != 0u
	begin
		exit 1ss
	end if

	set a -= 1u;
	if a != 18446744073709551615u
	begin
		exit 1ss
	end if
end proc
