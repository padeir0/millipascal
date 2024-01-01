proc main[]
var i, size, chunk
begin
	set i = 0;
	set size = 128;
	set chunk = 16

	while i < size
	begin
		set i += chunk
	end while
	exit 0ss
end proc
