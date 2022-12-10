proc main
var a:i8, b:i16, c:i32, d:i64, e:ptr, f:bool
begin
	# TODO: fix binary and hexadecimal literals
	set a = 'a';
	set a = '\n';
	set a = '0';
	set a = '\t';
	set a = '\r';
	# set a = 0b01100110r;
	#set a = 0xFFFFr;
	set a = 0r;
	
	# set b = 0b0110011001100110t;
	#set b = 0xFFFFFFFFt;
	set b = 1t;
	
	# set c = 0b01100110011001100110011001100110g;
	#set c = 0xFFFFFFFFFFFFFFFFg;
	set c = 2g;

	# set d = 0b0110011001100110011001100110011001100110011001100110011001100110;
	#set d = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF;
	set d = 3;

	# set e = 0b0110011001100110011001100110011001100110011001100110011001100110p
	#set e = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFp;
	set e = 4p;

	set f = true;
	set f = false;
end proc
