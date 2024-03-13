proc main
var char:i8
begin
	set char = 'a';
	set char = upper_case[char];
	if char != 'A' begin
		exit 1ss;
	end
end

proc upper_case[a:i8] i8
begin
	if a >= 'a' and a <= 'z' begin
		return a - 32ss;
	end
	return a;
end
