from ioutil import put_int, put_char, write_line, put_bool, debug
from buffutil import set_buff, map_buff, comp, copy
from charutil import is_word, is_letter, is_number, uppercase, lowercase

memory tst 32
memory ozy "nothing 314159 beside 111 remains, my name is ozymandias king of kings"

proc main
begin
	set_buff[tst, tst.size, 'a']
	write_line[tst, tst.size]

	map_buff[tst, tst.size, uppercase]
	write_line[tst, tst.size]

	map_buff[tst, tst.size, lowercase]
	write_line[tst, tst.size]

	put_int[~256]
	put_char['\n']
	put_int[0]
	put_char['\n']
	put_int[255]
	put_char['\n']

	put_bool[true]
	put_char['\n']
	put_bool[false]
	put_char['\n']

	# finds and prints all words: [a-zA-Z][a-zA-Z0-9_]* 
	find[ozy, ozy.size, is_letter, is_word, write_line]

	# finds and prints all numbers: [0-9][0-9]*
	find[ozy, ozy.size, is_number, is_number, write_line]

	# changes "ozymandias" to "superking,"
	find[ozy, ozy.size, is_letter, is_word, changeName]
	write_line[ozy, ozy.size]
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
