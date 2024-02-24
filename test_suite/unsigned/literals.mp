proc main
var a:u8, b:u16, c:u32, d:u64
begin
	set a = 0b11111111uss;
	if a + 1uss != 0uss begin
		exit 1ss
	end
	set a = 0xFFuss;
	if a + 1uss != 0uss begin
		exit 1ss
	end
	set a = 255uss;
	if a + 1uss != 0uss begin
		exit 1ss
	end

	set b = 0b1111111111111111us;
	if b + 1us != 0us begin
		exit 1ss
	end
	set b = 0xFFFFus;
	if b + 1us != 0us begin
		exit 1ss
	end
	set b = 65535us;
	if b + 1us != 0us begin
		exit 1ss
	end

	set c = 0b11111111111111111111111111111111ul;
	if c + 1ul != 0ul begin
		exit 1ss
	end
	set c = 0xFFFFFFFFul;
	if c + 1ul != 0ul begin
		exit 1ss
	end
	set c = 4294967295ul;
	if c + 1ul != 0ul begin
		exit 1ss
	end

	set d = 0b1111111111111111111111111111111111111111111111111111111111111111u
	if d + 1u != 0u begin
		exit 1ss
	end
	set d = 0xFFFFFFFFFFFFFFFFu
	if d + 1u != 0u begin
		exit 1ss
	end
	set d = 18446744073709551615u;
	if d + 1u != 0u begin
		exit 1ss
	end
end
