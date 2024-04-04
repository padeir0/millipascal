from io import write
data hello "Hello\n"

proc main
var p:ptr, c:i8
begin
	set p = hello;
	write[hello, sizeof[hello]];
	
	while p < hello + 6 begin
		if p@i8 >= 'a' and p@i8 <= 'z' begin
			set p@i8 -= 32ss;
		end
		set p += 1;
	end

	write[hello, sizeof[hello]];
end
