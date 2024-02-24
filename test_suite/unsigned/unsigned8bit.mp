proc main
var a:u8
begin
	set a = 1uss;
	if a != 1uss
	begin
		exit 1ss;
	end

	set a -= 1uss;
	if a != 0uss
	begin
		exit 1ss
	end

	set a -= 1uss;
	if a != 255uss
	begin
		exit 1ss
	end
end
