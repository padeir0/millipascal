memory hello "Hello\n"

proc main
var p:ptr, c:i8
begin
	set p = hello;
	write[hello, hello.size]
	
	while p < hello + 6p
	begin
		if p@i8 >= 'a' and p@i8 <= 'z'
		begin
			set p@i8 -= 32r
		end if
		set p += 1p;
	end while

	write[hello, hello.size]
end proc
