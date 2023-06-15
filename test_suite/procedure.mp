proc main
var a, b, c
begin
	set a, b, c = P[1, 2, 3]
	if a != 1 or b != 2 or c != 3
	begin
		exit 1ss
	end if
end proc

proc P
	[arg1, arg2, arg3]
	i64, i64, i64
var a, b, c
begin
	set a = arg1
	set b = arg2
	set c = arg3
	return a, b, c
end proc
