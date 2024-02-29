data result0 "32"
data result1 "-32"
data result2 "1"
data result3 "0"
data result4 "1000"
data result5 "-1000"

proc main
var p:ptr, size:i64
begin
	set p, size = itoa[32]
	if not equals[p, size, result0, sizeof result0] begin
		exit 1ss
	end
	set p, size = itoa[~32]
	if not equals[p, size, result1, sizeof result1] begin
		exit 2ss
	end
	set p, size = itoa[1]
	if not equals[p, size, result2, sizeof result2] begin
		exit 3ss
	end
	set p, size = itoa[0]
	if not equals[p, size, result3, sizeof result3] begin
		exit 4ss
	end
	set p, size = itoa[1000]
	if not equals[p, size, result4, sizeof result4] begin
		exit 5ss
	end
	set p, size = itoa[~1000]
	if not equals[p, size, result5, sizeof result5] begin
		exit 6ss
	end
end

proc equals[p1:ptr, size1:i64, p2:ptr, size2:i64] bool
var i:i64
begin
	if size1 != size2 begin
		return false
	end

	set i = 0
	while i < size1
	begin
		if (p1 + i:ptr)@i8 != (p2 + i:ptr)@i8 begin
			return false
		end
		set i += 1
	end
	return true
end

data itoa_buff [32]

# do not modify the returned buffer
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

