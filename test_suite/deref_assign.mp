data buff [3]

proc main
begin
	set (buff+1)@i8 = comp[1];
end

proc comp[index:i64] i8
begin
	if index != 1
	begin
		exit index:i8;
	end
	exit 0ss;
end
