proc main
var a, b:i32
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

proc Perform[a, b:i32, p:proc[i32, i32][i32] ] i32
begin
	return p[a, b];
end

proc Add[a, b:i32] i32
begin
	return a + b;
end

proc Sub[a, b:i32] i32
begin
	return a - b;
end
