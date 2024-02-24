proc main
var a, b, c
begin
	set a, b, c = P[]
	if a != 1 begin
		exit 1ss
	end
	if b != 2 begin
		exit 2ss
	end
	if c != 3 begin
		exit 3ss
	end
end

proc P[] i64, i64, i64
begin
	return 1, 2, 3
end
