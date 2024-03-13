proc main
var a:bool, b:bool, c:bool, d:bool
begin
	set a = 1 == 0;
	set b = 1 != 2;
	set c = 1 == 1;
	set d = a or (b and c);
end
