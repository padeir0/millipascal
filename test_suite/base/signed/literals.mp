proc main
var a:i8, b:i16, c:i32, d:i64, e:ptr, z:bool
begin
	set a = 'a';
	if a+1ss != 'b' begin
		exit 1ss;
	end
	set a = '\t';
	set a = '\r';
	set a = '\n';
	if a != 0xAss begin
		exit 2ss;
	end
	set a = '0';
	if a-'0' != 0ss begin
		exit 3ss;
	end

	set a = 0b01111111ss;
	if a+1ss != ~128ss begin
		exit 4ss;
	end
	set a = 0x7Fss;
	if a+1ss != ~128ss begin
		exit 5ss;
	end
	set a = 127ss;
	if a+1ss != ~128ss begin
		exit 6ss;
	end
	
	set b = 0b0111111111111111s;
	if b+1s != ~32768s begin
		exit 7ss;
	end
	set b = 0x7FFFs;
	if b+1s != ~32768s begin
		exit 8ss;
	end
	set b = 32767s;
	if b+1s != ~32768s begin
		exit 9ss;
	end
	
	set c = 0b01111111111111111111111111111111;
	if c+1 != ~2147483648 begin
		exit 10ss;
	end
	set c = 0x7FFFFFFF;
	if c+1 != ~2147483648 begin
		exit 11ss;
	end
	set c = 2147483647;
	if c+1 != ~2147483648 begin
		exit 12ss;
	end

	set d = 0b0111111111111111111111111111111111111111111111111111111111111111l;
	if d+1l != ~9223372036854775808l begin
		exit 13ss;
	end
	set d = 0x7FFFFFFFFFFFFFFFl;
	if d+1l != ~9223372036854775808l begin
		exit 14ss;
	end
	set d = 9223372036854775807l;
	if d+1l != ~9223372036854775808l begin
		exit 15ss;
	end

	set e = 0b0111111111111111111111111111111111111111111111111111111111111111p;
	if e+1 != (1ul << 63ul):ptr
	begin
		exit 16ss;
	end
	set e = 0x7FFFFFFFFFFFFFFFp;
	if e+1 != (1ul << 63ul):ptr
	begin
		exit 17ss;
	end
	set e = 9223372036854775807p;
	if e+1 != (1ul << 63ul):ptr
	begin
		exit 18ss;
	end

	set z = true;
	if not z != false
	begin
		exit 19ss;
	end
	set z = false;
	if not z != true
	begin
		exit 20ss;
	end
end
