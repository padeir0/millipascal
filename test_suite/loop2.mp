memory hello "Hello, World!\n"

proc main
var a:i8, p:ptr
begin
	set a = 0r;

	while a < 'a'
	begin
		set a += 1r;
	end while

	set hello@i8 = a:i8
end proc
