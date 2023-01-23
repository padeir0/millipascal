export itoa, btoa

memory itoa_buff 32

proc main
var p:ptr, size
begin
	set p, size = itoa[255]
	write[p, size]
end proc

# do not modify the returned buffer
proc itoa[n] ptr, i64
var p:ptr, signal:i8
begin
	set signal = 0r
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

memory _T "true"
memory _F "false"

# do not modify the returned buffer
proc btoa[b:bool] ptr, i64
begin
	if b
	begin
		return _T, _T.size
	end 
	else
	begin
		return _F, _F.size
	end if
end proc
