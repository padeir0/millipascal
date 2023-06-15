memory result0 "32"
memory result1 "-32"
memory result2 "1"
memory result3 "0"
memory result4 "1000"
memory result5 "-1000"

proc main
var p:ptr, size
begin
	set p, size = itoa[32]
	if not equals[p, size, result0, result0.size]
	begin
		exit 1ss
	end if
	set p, size = itoa[~32]
	if not equals[p, size, result1, result1.size]
	begin
		exit 2ss
	end if
	set p, size = itoa[1]
	if not equals[p, size, result2, result2.size]
	begin
		exit 3ss
	end if
	set p, size = itoa[0]
	if not equals[p, size, result3, result3.size]
	begin
		exit 4ss
	end if
	set p, size = itoa[1000]
	if not equals[p, size, result4, result4.size]
	begin
		exit 5ss
	end if
	set p, size = itoa[~1000]
	if not equals[p, size, result5, result5.size]
	begin
		exit 6ss
	end if
end proc

proc equals[p1:ptr, size1:i64, p2:ptr, size2:i64] bool
var i
begin
	if size1 != size2
	begin
		return false
	end if

	set i = 0
	while i < size1
	begin
		if (p1 + i:ptr)@i8 != (p2 + i:ptr)@i8
		begin
			return false
		end if
		set i += 1
	end while
	return true
end proc

memory itoa_buff 32

# do not modify the returned buffer
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

