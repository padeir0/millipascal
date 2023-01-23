proc main
var a
begin
	set a = -1
	set a = -a
	if a != 1 
	begin
		exit 1r
	end if
end proc
