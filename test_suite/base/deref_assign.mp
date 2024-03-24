data buff [3]

proc main
begin
	set (buff+1)@i8 = comp[1];
	if (buff+1)@i8 != 0ss begin
		exit 1ss;
	end
end

proc comp[index:i32] i8
begin
	if index != 1
	begin
		exit index:i8;
	end
	exit 0ss;
end
