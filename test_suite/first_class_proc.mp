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
