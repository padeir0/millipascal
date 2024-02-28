data hello "Hello, World!\n"

proc main
var a:i64, p:ptr
begin
	set a = 0;
	set p = hello;
	while a < hello.size begin
		set (p+a:ptr)@i8 = 'A'
		set a += 1;
	end
	write[hello, hello.size]
end
