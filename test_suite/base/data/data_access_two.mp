data buff [512]

proc main
var a:i8
begin
	set a = buff@i8 + (buff+1)@i8;
end
