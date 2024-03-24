proc main
var i:i32
begin
	set i = 0;
	while i < 10 begin
		set i += 1;
	end
	if i != 10 begin
		exit 1ss;
	end
end
