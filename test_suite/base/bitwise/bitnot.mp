proc main
var a:u8
begin
	set a = 0b0000_1111uss;
	if !a != 0b1111_0000uss begin
		exit 1ss;
	end
	if !!a != a begin
		exit 2ss;
	end
	if !0b1111_0000uss != a begin
		exit 3ss;
	end
	if !0b1010_0101uss != 0b0101_1010uss begin
		exit 4ss;
	end
end
