proc main
begin
	if 'a' != 'a'
	begin
		exit 1ss
	end

	if 'a' > 'b'
	begin
		exit 1ss
	end

	if 'b' < 'a'
	begin
		exit 1ss
	end

	if not('a' >= 'a')
	begin
		exit 1ss
	end
end
