data hello "Hello, World!\n"

proc main
var a:i64, p:ptr
begin
	set a = 0;
	set p = hello;
	while a < sizeof[hello] begin
		set (p+a)@i8 = 'A';
		set a += 1;
	end
	write[hello, sizeof[hello]];
end
