proc main
var a:i64, b:u64
begin
	set a = 0b0000_1111;
	if a | 0b1111_0000 != 0b1111_1111 begin
		exit 1ss;
	end
	if a | 0b1111_1111 != 0b1111_1111 begin
		exit 1ss;
	end
	if a | 0b0000_0101 != 0b0000_1111 begin
		exit 1ss;
	end
	if a | 0b0000_1000 != 0b0000_1111 begin
		exit 1ss;
	end

	set b = 0b0000_0000ull;
	if b | 0b0000_0000ull != 0b0000_0000ull begin
		exit 1ss;
	end
	if b | 0b0000_1111ull != 0b0000_1111ull begin
		exit 1ss;
	end
	if b | 0b1010_0000ull != 0b1010_0000ull begin
		exit 1ss;
	end
	if b | 0b0001_1000ull != 0b0001_1000ull begin
		exit 1ss;
	end
end
