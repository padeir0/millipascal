proc main
var a, b:i64
begin
	set a = 1;

	set b = Perform[a, a, Add];
	if b != 2 begin
		exit 1ss;
	end

	set b = Perform[b, b, Sub];
	if b != 0 begin
		exit 1ss;
	end
end

proc Perform[a, b:i64, p:proc[i64, i64][i64] ] i64
begin
	return p[a, b];
end

proc Add[a, b:i64] i64
begin
	return a + b;
end

proc Sub[a, b:i64] i64
begin
	return a - b;
end
