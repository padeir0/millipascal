export is_letter, is_word, is_number, uppercase, lowercase

proc is_letter[char:i8] bool 
begin
	return (char >= 'a' and char <= 'z')
	        or
	       (char >= 'A' and char <= 'Z')
end proc

proc is_word[char:i8] bool
begin
	return (char >= 'a' and char <= 'z')
        or
	       (char >= 'A' and char <= 'Z')
		or
	       (char >= '0' and char <= '0')
		or
	       (char == '_')
end proc

proc is_number[char:i8] bool
begin
	return char >= '0' and char <= '9'
end proc

proc uppercase[a:i8] i8
begin
	if a >= 'a' and a <= 'z'
	begin
		return a - ('a'-'A')
	end if
	return a
end proc

proc lowercase[a:i8] i8
begin
	if a >= 'A' and a <= 'Z'
	begin
		return a + ('a'-'A')
	end if
	return a
end proc
