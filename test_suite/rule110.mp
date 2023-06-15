memory buff1 102
memory buff2 102
memory here "here\n"

proc main
var gen, b1:ptr, b2:ptr, b3:ptr
begin
	clear[buff1, buff1.size]
	clear[buff2, buff2.size]
	set gen = 0;
	set b1 = buff1;
	set b2 = buff2;

	set (buff1+(buff1.size/2):ptr)@i8 = 'O';

	while gen < 1024
	begin
		write[b1, buff1.size]
		putchar[' ']
		pint[gen]
		putchar['\n']

		generation[b1, b2, buff1.size]
		set b3 = b2
		set b2 = b1
		set b1 = b3

		set gen += 1
	end while
end proc

proc clear[p:ptr, size:i64]
var a
begin
	set a = 0
	while a < size
	begin
		set (p+a:ptr)@i8 = ' '
		set a += 1
	end while
end proc

proc generation[b1:ptr, b2:ptr, size]
var a, c:i8
begin
	set a = 1
	while a < size -2
	begin
		set (b2+a:ptr)@i8 = rule110[b1, a]
		set a += 1
	end while
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

proc debug
begin
	write[here, here.size]
end proc

memory pchar_buff 1

proc putchar[n:i8]
begin
	set pchar_buff@i8 = n
	write[pchar_buff, pchar_buff.size]
end proc

proc pint[n]
var p:ptr, size
begin
	set p, size = itoa[n]
	write[p, size]
end proc

memory itoa_buff 32

proc itoa[n] ptr, i64
var p:ptr, signal:i8
begin
	set signal = 0ss
	set p = itoa_buff + itoa_buff.size:ptr - 1p # end of buffer

	if n == 0
	begin
		set itoa_buff@i8 = '0'
		return itoa_buff, 1
	end if

	if n < 0
	begin
		set signal = '-'
		set n = ~n
	end if

	while n > 0 begin
		set p@i8 = (n % 10):i8 + '0'
		set n /= 10;
		set p -= 1p
	end while

	if signal == '-'
	begin
		set p@i8 = signal
	end
	else
	begin
		set p += 1p;
	end if

	return p, itoa_buff.size-(p-itoa_buff):i64 
end proc
