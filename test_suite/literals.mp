proc main
var a:i8, b:i16, c:i32, d:i64, e:ptr, f:bool
begin
	set a = 'a';
	set a = '\n';
	set a = '0';
	set a = '\t';
	set a = '\r';
	set a = 0b01111111r;
	set a = 0x7Fr;
	set a = 127r;
	
	set b = 0b0111111111111111t;
	set b = 0x7FFFt;
	set b = 32767t;
	
	set c = 0b01111111111111111111111111111111g;
	set c = 0x7FFFFFFFg;
	set c = 2147483647g;

	set d = 0b0111111111111111111111111111111111111111111111111111111111111111;
	set d = 0x7FFFFFFFFFFFFFFF;
	set d = 9223372036854775807;

	set e = 0b0111111111111111111111111111111111111111111111111111111111111111p
	set e = 0x7FFFFFFFFFFFFFFFp;
	set e = 9223372036854775807p;

	set f = true;
	set f = false;
end proc
