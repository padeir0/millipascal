proc main
begin
end

proc rule110[buff:ptr, index:i64] i8
var left:i8, curr:i8, right:i8
begin
	set left  =  (buff + index - 1)@i8;
	set curr  =  (buff + index)@i8;
	set right =  (buff + index + 1)@i8;

	if left == 'O' begin
		if curr == 'O' begin
			if right == 'O' begin
				return ' ';
			end else begin
				return 'O';
			end
		end else begin
			if right == 'O' begin
				return 'O';
			end else begin
				return ' ';
			end
		end
	end else begin
		if curr == 'O' begin
			if right == 'O' begin
				return 'O';
			end else begin
				return 'O';
			end
		end else begin
			if right == 'O' begin
				return 'O';
			end else begin
				return ' ';
			end
		end
	end
end
