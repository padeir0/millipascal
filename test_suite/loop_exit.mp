proc main
var i
begin
	set i = 0;
	while i < 10
	begin
		set i += 1;
	end while
	if i != 10
	begin
		exit 1r
	end if
end proc
