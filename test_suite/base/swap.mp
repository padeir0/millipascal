data a "A"
data b "B"

proc main
begin
	A[]; B[];
end

proc A
var b1:ptr, b2:ptr, sw:ptr
begin
	set b1 = a;
	set b2 = b;

	set sw = b1;
	set b1 = b2;
	set b2 = sw;

	if b1 == a or b2 == b begin
		exit 1ss;
	end
end

proc B
var b1:ptr, b2:ptr
begin
	set b1 = a;
	set b2 = b;

	set b1 <> b2;

	if b1 == a or b2 == b begin
		exit 2ss;
	end
end
