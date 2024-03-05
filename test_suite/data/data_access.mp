data buff [512]

proc main
var p:ptr
begin
	set p = buff;
	set p@i8 = 1:i8;

	set p = p + 1;
	set p@i8 = 2:i8;
end
