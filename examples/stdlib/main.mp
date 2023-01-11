from ioutil import putint, putchar, writeLn, putbool, debug
from buffutil import setbuff, mapbuff, comp, copy
from charutil import isWord, isLetter, isNumber, uppercase, lowercase

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
