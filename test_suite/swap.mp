data a "A"
data b "B"
data nl "\n"
proc newline[]
begin
	write[nl, nl.size]
end proc

proc main
var b1:ptr, b2:ptr, sw:ptr
begin
	set b1 = a;
	set b2 = b;

	set sw = b1
	set b1 = b2
	set b2 = sw

	if b1 == a or b2 == b
	begin
		exit 1ss
	end if
end proc
