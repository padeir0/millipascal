proc main
begin
end proc

proc rule110[buff:ptr, index] i8
var left:i8, curr:i8, right:i8
begin

	set left  =  (buff + index:ptr - 1p)@i8
	set curr  =  (buff + index:ptr)@i8
	set right =  (buff + index:ptr + 1p)@i8

	if left == 'O'
	begin
		if curr == 'O'
		begin
			if right == 'O'
			begin
				return ' '
			end
			else
			begin
				return 'O'
			end if
		end
		else
		begin
			if right == 'O'
			begin
				return 'O'
			end
			else
			begin
				return ' '
			end if
		end if
	end
	else
	begin
		if curr == 'O'
		begin
			if right == 'O'
			begin
				return 'O'
			end
			else
			begin
				return 'O'
			end if
		end
		else
		begin
			if right == 'O'
			begin
				return 'O'
			end
			else
			begin
				return ' '
			end if
		end if
	end if
end proc
