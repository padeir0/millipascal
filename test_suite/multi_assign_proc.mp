proc main
vars a, b, c
begin
	set a, b, c = P()
end proc

proc P() qword, qword, qword
begin
	return 1, 2, 3
end proc
