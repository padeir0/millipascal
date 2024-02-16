data buff [3]

proc main
begin
	set (buff+1p)@i8 = comp[1]
end proc

proc comp[index] i8
begin
	if index != 1
	begin
		exit index:i8
	end if
	exit 0ss
end proc
