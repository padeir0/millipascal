from io import print
from ioutil import put_int, put_ln

data buff1 [102]
data buff2 [102]
data here "here\n"

proc main
var gen:i32, b1:ptr, b2:ptr, b3:ptr
begin
	clear[buff1, sizeof[buff1]];
	clear[buff2, sizeof[buff2]];
	set gen = 0;
	set b1 = buff1;
	set b2 = buff2;

	set (buff1+(sizeof[buff1])/2)@i8 = 'O';

	while gen < 1024 begin

		generation[b1, b2, sizeof[buff1]];
		set b3 = b2;
		set b2 = b1;
		set b1 = b3;

		set gen += 1;
	end
end

proc clear[p:ptr, size:i32]
var a:i32
begin
	set a = 0;
	while a < size
	begin
		set (p+a)@i8 = ' ';
		set a += 1;
	end
end

proc generation[b1:ptr, b2:ptr, size:i32]
var a:i32, c:i8
begin
	set a = 1;
	while a < size -2
	begin
		set (b2+a)@i8 = rule110[b1, a];
		set a += 1;
	end
end

const begin
	_000 = ' ';
	_001 = 'O';
	_010 = 'O';
	_011 = ' ';

	_100 = 'O';
	_101 = 'O';
	_110 = 'O';
	_111 = ' ';
end

proc rule110[buff:ptr, index:i32] i8
var left, curr, right:i8
begin
	set left  =  (buff + index - 1)@i8;
	set curr  =  (buff + index)@i8;
	set right =  (buff + index + 1)@i8;

	if left == 'O' begin
		if curr == 'O' begin
			if right == 'O' begin
				return _000;
			end else begin
				return _001;
			end
		end else begin
			if right == 'O' begin
				return _010;
			end else begin
				return _011;
			end
		end
	end else begin
		if curr == 'O' begin
			if right == 'O' begin
				return _100;
			end else begin
				return _101;
			end
		end else begin
			if right == 'O' begin
				return _110;
			end else begin
				return _111;
			end
		end
	end
end
