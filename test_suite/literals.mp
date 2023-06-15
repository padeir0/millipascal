proc main
var a:i8, b:i16, c:i32, d:i64, e:ptr,
	f:u8, g:u16, h:u32, i:u64,
	z:bool
begin
	set a = 'a';
	set a = '\n';
	set a = '0';
	set a = '\t';
	set a = '\r';

	set a = 0b01111111ss;
	set a = 0x7Fss;
	set a = 127ss;

	set f = 0b01111111uss;
	set f = 0x7Fuss;
	set f = 127uss;
	
	set b = 0b0111111111111111s;
	set b = 0x7FFFs;
	set b = 32767s;

	set g = 0b0111111111111111us;
	set g = 0x7FFFus;
	set g = 32767us;
	
	set c = 0b01111111111111111111111111111111l;
	set c = 0x7FFFFFFFl;
	set c = 2147483647l;

	set h = 0b01111111111111111111111111111111ul;
	set h = 0x7FFFFFFFul;
	set h = 2147483647ul;

	# TODO: fix these lol
	# set d = 0b0111111111111111111111111111111111111111111111111111111111111111;
	# set d = 0x7FFFFFFFFFFFFFFF;
	# set d = 9223372036854775807;

	# set d = 0b0111111111111111111111111111111111111111111111111111111111111111u;
	# set d = 0x7FFFFFFFFFFFFFFFu;
	# set d = 9223372036854775807u;

	# set e = 0b0111111111111111111111111111111111111111111111111111111111111111p
	# set e = 0x7FFFFFFFFFFFFFFFp;
	# set e = 9223372036854775807p;

	set z = true;
	set z = false;
end proc
