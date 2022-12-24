memory tst 32
memory ozy "nothing 314159 beside 111 remains, my name is ozymandias king of kings"

proc main
begin
	setbuff[tst, tst.size, 'a']
	writeLn[tst, tst.size]

	mapbuff[tst, tst.size, uppercase]
	writeLn[tst, tst.size]

	mapbuff[tst, tst.size, lowercase]
	writeLn[tst, tst.size]

	putint[-256]
	putchar['\n']
	putint[0]
	putchar['\n']
	putint[255]
	putchar['\n']

	putbool[true]
	putchar['\n']
	putbool[false]
	putchar['\n']

	# finds and prints all words: [a-zA-Z][a-zA-Z0-9_]* 
	find[ozy, ozy.size, isLetter, isWord, writeLn]

	# finds and prints all numbers: [0-9][0-9]*
	find[ozy, ozy.size, isNumber, isNumber, writeLn]

	# changes "ozymandias" to "superking,"
	find[ozy, ozy.size, isLetter, isWord, changeName]
	writeLn[ozy, ozy.size]
end proc

proc mapbuff[p:ptr, size:i64, transform:proc[i8]i8]
var a
begin
	set a = 0
	while a < size
	begin
		set (p+a:ptr)@i8 = transform[ (p+a:ptr)@i8 ]
		set a += 1
	end while
end proc

proc setbuff[p:ptr, size:i64, b:i8]
var a
begin
	set a = 0
	while a < size
	begin
		set (p+a:ptr)@i8 = b
		set a += 1
	end while
end proc

memory here "here\n"

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

proc putint[n]
var p:ptr, size
begin
	set p, size = itoa[n]
	write[p, size]
end proc

memory _T "true"
memory _F "false"

proc putbool[b:bool]
begin
	if b
	begin
		write[_T, _T.size]
	end 
	else
	begin
		write[_F, _F.size]
	end if
end proc

proc writeLn[p:ptr, size:i64]
begin
	write[p, size]
	putchar['\n']
end proc

memory itoa_buff 32

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
		set n = -n
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

proc find[
          p:ptr,
          size:i64,
          head:proc[i8]bool,
          body:proc[i8]bool,
          op:proc[ptr, i64]
         ]
var start:ptr, finish:ptr, curr:ptr, char:i8
begin
	set start = 0p
	set finish = 0p
	set curr = p

	while curr < p + size:ptr
	begin
		set char = curr@i8
		if head[char] and start == 0p
		begin
			set start  = curr
			set finish = curr
		end
		elseif body[char] and start > 0p
		begin
			set finish = curr
		end 
		elseif finish > start
		begin
			op[start, (finish+1p-start):i64]
			set start = 0p
			set finish = 0p
		end if
		set curr += 1p
	end while

	if finish > start
	begin
		op[start, (finish+1p-start):i64]
		set start = 0p
		set finish = 0p
	end if
end proc

proc isLetter[char:i8] bool 
begin
	return (char >= 'a' and char <= 'z')
	        or
	       (char >= 'A' and char <= 'Z')
end proc

proc isWord[char:i8] bool
begin
	return (char >= 'a' and char <= 'z')
	        or
	       (char >= 'A' and char <= 'Z')
		or
	       (char >= '0' and char <= '0')
		or
	       (char == '_')
end proc

proc isNumber[char:i8] bool
begin
	return char >= '0' and char <= '9'
end proc

proc comp[p1:ptr, size1:i64, p2:ptr, size2:i64] bool
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

proc copy[source:ptr, source_size:i64, dest:ptr, dest_size:i64]
var smallest, i
begin
	set smallest = min[source_size, dest_size]
	set i = 0
	while i < smallest
	begin
		set (dest + i:ptr)@i8 = (source + i:ptr)@i8
		set i += 1
	end while
end proc

proc min[a, b] i64
begin
	if a >= b
	begin
		return a
	end 
	else
	begin
		return b
	end if
end proc

memory ozymandias "ozymandias"
proc isOzymandias[p:ptr, size:i64] bool
begin
	return comp[p, size, ozymandias, ozymandias.size]
end proc

memory king "superking,"
proc changeName[p:ptr, size:i64]
begin
	if isOzymandias[p, size]
	begin
		copy[king, king.size, p, size]
	end if
end proc
