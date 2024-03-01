proc main
var a:i8, b:i16, c:i32, d:i64, e:ptr, z:bool
begin
	set a = 'a';
	if a+1ss != 'b' begin
		exit 1ss
	end
	set a = '\t';
	set a = '\r';
	set a = '\n';
	if a != 0xAss begin
		exit 1ss
	end
	set a = '0';
	if a-'0' != 0ss begin
		exit 1ss
	end

	set a = 0b01111111ss;
	if a+1ss != ~128ss begin
		exit 1ss
	end
	set a = 0x7Fss;
	if a+1ss != ~128ss begin
		exit 1ss
	end
	set a = 127ss;
	if a+1ss != ~128ss begin
		exit 1ss
	end
	
	set b = 0b0111111111111111s;
	if b+1s != ~32768s begin
		exit 1ss
	end
	set b = 0x7FFFs;
	if b+1s != ~32768s begin
		exit 1ss
	end
	set b = 32767s;
	if b+1s != ~32768s begin
		exit 1ss
	end
	
	set c = 0b01111111111111111111111111111111l;
	if c+1l != ~2147483648l begin
		exit 1ss
	end
	set c = 0x7FFFFFFFl;
	if c+1l != ~2147483648l begin
		exit 1ss
	end
	set c = 2147483647l;
	if c+1l != ~2147483648l begin
		exit 1ss
	end

	set d = 0b0111111111111111111111111111111111111111111111111111111111111111;
	if d+1 != ~9223372036854775808
	begin
		exit 1ss
	end
	set d = 0x7FFFFFFFFFFFFFFF;
	if d+1 != ~9223372036854775808
	begin
		exit 1ss
	end
	set d = 9223372036854775807;
	if d+1 != ~9223372036854775808
	begin
		exit 1ss
	end

	set e = 0b0111111111111111111111111111111111111111111111111111111111111111p
	if e+1 != (1u << 63u):ptr
	begin
		exit 1ss
	end
	set e = 0x7FFFFFFFFFFFFFFFp;
	if e+1 != (1u << 63u):ptr
	begin
		exit 1ss
	end
	set e = 9223372036854775807p;
	if e+1 != (1u << 63u):ptr
	begin
		exit 1ss
	end

	set z = true;
	if not z != false
	begin
		exit 1ss
	end
	set z = false;
	if not z != true
	begin
		exit 1ss
	end
end
