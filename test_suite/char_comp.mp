data hello "Hello\n"

proc main
var p:ptr, res:bool
begin
	set p = hello;

	set res = p@i8 >= 'A' and p@i8 <= 'Z'
	if not res 
	begin
		exit 1ss
	end
end
