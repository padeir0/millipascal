proc main
var a:u64
begin
	set a = 1ul;
	if a != 1ul begin
		exit 1ss;
	end

	set a -= 1ul;
	if a != 0ul begin
		exit 1ss;
	end

	set a -= 1ul;
	if a != 18446744073709551615ul begin
		exit 1ss;
	end
end
