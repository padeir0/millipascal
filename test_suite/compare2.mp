proc main
var a:i8
begin
	set a = 'a'
	if a != a and a != a
	begin
		exit 1ss
	end

	if a > 'b' and a > 'b'
	begin
		exit 1ss
	end

	if 'b' < a and 'b' < a
	begin
		exit 1ss
	end

	if not(a >= a) and not(a >= a)
	begin
		exit 1ss
	end

	if not(a <= a) and not(a <= a)
	begin
		exit 1ss
	end
end
