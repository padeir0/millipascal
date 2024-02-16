data hello "Hello, World!\n"

proc main
var a:i8, p:ptr
begin
	set a = 0ss;

	while a < 'a'
	begin
		set a += 1ss;
	end while

	set hello@i8 = a:i8
end proc
