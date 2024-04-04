from io import write

data hello "Hello, World!\n"

proc main
begin
	write[hello, sizeof[hello]];
end
