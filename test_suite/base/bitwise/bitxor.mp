proc main
var a:i32, b:u64
begin
	set a = 0b0000_1111;
	if a ^ 0b1111_0000 != 0b1111_1111 begin
		exit 1ss;
	end
	if a ^ 0b1111_1111 != 0b1111_0000 begin
		exit 1ss;
	end
	if a ^ 0b0000_0101 != 0b0000_1010 begin
		exit 1ss;
	end
	if a ^ 0b0000_1000 != 0b0000_0111 begin
		exit 1ss;
	end

	set b = 0b0000_0000ul;
	if b ^ 0b0000_0000ul != 0b0000_0000ul begin
		exit 1ss;
	end
	if b ^ 0b0000_1111ul != 0b0000_1111ul begin
		exit 1ss;
	end
	if b ^ 0b1010_0000ul != 0b1010_0000ul begin
		exit 1ss;
	end
	if b ^ 0b0001_1000ul != 0b0001_1000ul begin
		exit 1ss;
	end
end
