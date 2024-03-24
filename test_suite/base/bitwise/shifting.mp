proc main
var a:i32
begin
	set a = 1;
	if a << 1 != 2 begin
		exit 1ss;
	end
	if a << 2 != 4 begin
		exit 1ss;
	end
	if a << 3 != 8 begin
		exit 1ss;
	end
	if a << 4 != 16 begin
		exit 1ss;
	end

	set a = 32;
	if a >> 1 != 16 begin
		exit 1ss;
	end
	if a >> 2 != 8 begin
		exit 1ss;
	end
	if a >> 3 != 4 begin
		exit 1ss;
	end
	if a >> 4 != 2 begin
		exit 1ss;
	end
end
