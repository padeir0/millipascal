proc main
var a, b, c:i32
begin
	set a, b, c = P[];
	if a != 1 begin
		exit 1ss;
	end
	if b != 2 begin
		exit 2ss;
	end
	if c != 3 begin
		exit 3ss;
	end
end

proc P[] i32, i32, i32
begin
	return 1, 2, 3;
end
