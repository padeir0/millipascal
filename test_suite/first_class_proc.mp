proc main
var a, b
begin
	set a = 1

	set b = Perform[a, a, Add]
	if b != 2
	begin
		exit 1r
	end if

	set b = Perform[b, b, Sub]
	if b != 0 
	begin
		exit 1r
	end if
end proc

proc Perform[a, b, p:proc[i64, i64] i64] i64
begin
	return p[a, b];
end proc

proc Add[a, b] i64
begin
	return a + b
end proc

proc Sub[a, b] i64
begin
	return a - b
end proc
