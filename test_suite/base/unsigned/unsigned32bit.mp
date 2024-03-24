proc main
var a:u32
begin
	set a = 1u;
	if a != 1u
	begin
		exit 1ss;
	end

	set a -= 1u;
	if a != 0u
	begin
		exit 1ss;
	end

	set a -= 1u;
	if a != 4294967295u
	begin
		exit 1ss;
	end
end
