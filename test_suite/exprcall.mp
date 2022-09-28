proc main
var result
begin
	set result = square(5) + 1;
end proc

proc square(a) i64
begin
	return a * a;
end proc
