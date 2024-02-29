data buff1 [102]
data buff2 [102]
data here "here\n"

proc main
var gen:i64, b1:ptr, b2:ptr, b3:ptr
begin
	clear[buff1, sizeof buff1]
	clear[buff2, sizeof buff2]
	set gen = 0;
	set b1 = buff1;
	set b2 = buff2;

	set (buff1+((sizeof buff1)/2):ptr)@i8 = 'O';

	while gen < 1024 begin

		generation[b1, b2, sizeof buff1]
		set b3 = b2
		set b2 = b1
		set b1 = b3

		set gen += 1
	end
end

proc clear[p:ptr, size:i64]
var a:i64
begin
	set a = 0
	while a < size
	begin
		set (p+a:ptr)@i8 = ' '
		set a += 1
	end
end

proc generation[b1:ptr, b2:ptr, size:i64]
var a:i64, c:i8
begin
	set a = 1
	while a < size -2
	begin
		set (b2+a:ptr)@i8 = rule110[b1, a]
		set a += 1
	end
end

proc rule110[buff:ptr, index:i64] i8
var left, curr, right:i8
begin

	set left  =  (buff + index:ptr - 1p)@i8
	set curr  =  (buff + index:ptr)@i8
	set right =  (buff + index:ptr + 1p)@i8

	if left == 'O' begin
		if curr == 'O' begin
			if right == 'O' begin
				return ' '
			end else begin
				return 'O'
			end
		end else begin
			if right == 'O' begin
				return 'O'
			end else begin
				return ' '
			end
		end
	end else begin
		if curr == 'O' begin
			if right == 'O' begin
				return 'O'
			end else begin
				return 'O'
			end
		end else begin
			if right == 'O' begin
				return 'O'
			end else begin
				return ' '
			end
		end
	end
end

proc debug
begin
	write[here, sizeof here]
end

data pchar_buff [1]

proc putchar[n:i8]
begin
	set pchar_buff@i8 = n
	write[pchar_buff, sizeof pchar_buff]
end

proc pint[n:i64]
var p:ptr, size:i64
begin
	set p, size = itoa[n]
	write[p, size]
end

data itoa_buff [32]

proc itoa[n:i64] ptr, i64
var p:ptr, signal:i8
begin
	set signal = 0ss
	set p = itoa_buff + (sizeof itoa_buff):ptr - 1p # end of buffer

	if n == 0 begin
		set itoa_buff@i8 = '0'
		return itoa_buff, 1
	end

	if n < 0 begin
		set signal = '-'
		set n = ~n
	end

	while n > 0 begin
		set p@i8 = (n % 10):i8 + '0'
		set n /= 10;
		set p -= 1p
	end

	if signal == '-' begin
		set p@i8 = signal
	end else begin
		set p += 1p;
	end

	return p, (sizeof itoa_buff)-(p-itoa_buff):i64 
end
