proc main
var a
begin
	set a = sizeof i8
	if a != 1 begin
		exit 1ss
	end

	if sizeof i32 >= sizeof i64 begin
		exit 1ss
	end
	if sizeof i16 >= sizeof i32 begin
		exit 1ss
	end
	if sizeof i8 >= sizeof i16 begin
		exit 1ss
	end

	if sizeof i8 != sizeof u8 begin
		exit 1ss
	end
	if sizeof i16 != sizeof u16 begin
		exit 1ss
	end
	if sizeof i32 != sizeof u32 begin
		exit 1ss
	end
	if sizeof i64 != sizeof u64 begin
		exit 1ss
	end
end
