from charutil import is_word, is_number
from ioutil import write_line

proc main
begin
	set counter@ptr = 0p
	find[text, text.size]
end proc

memory text "my name 999 is not 123 ozy"

# can match:
#	"c" character 'c'
#	"." not newline
#	"*" any number of not newline
#	"\.", "\*", "\\", "\n", "\w", "\d" escapes
memory regex "\\d\\d\\d"
memory counter 8

proc regexp_match[char:i8] bool
var re_char:i8
begin
	set re_char = (regex + counter@ptr)@i8
	set counter@ptr += 1p
	if re_char == '.' begin
		return char != '\n'
	end
	elseif re_char == '*' begin
		if char != '\n' begin
			set counter@ptr -= 1p
			return true
		end if
		return false
	end
	elseif re_char == '\\' begin
		set re_char = (regex + counter@ptr)@i8
		set counter@ptr += 1p
		if re_char == '.' or re_char == '*' or re_char == '\\' or re_char == ':' begin
			return char == re_char
		end
		elseif re_char == 'n' begin
			return char == '\n'
		end
		elseif re_char == 'w' begin
			return is_word[char]
		end
		elseif re_char == 'd' begin
			return is_number[char]
		end if
	end if
	return re_char == char
end proc

proc find[p:ptr, size:i64]
var start:ptr, finish:ptr, curr:ptr, matched:bool
begin
	set start = 0p
	set finish = 0p
	set curr = p

	while curr < p + size:ptr begin
		set matched = regexp_match[curr@i8]
		if matched and start == 0p begin
			set start  = curr
			set finish = curr
		end
		elseif matched and start > 0p begin
			set finish = curr
		end if

		if counter@i64 == regex.size begin
			set counter@i64 = 0
			write_line[start, (finish+1p-start):i64]
			set start = 0p
			set finish = 0p
		end if

		if not matched begin
			set counter@i64 = 0
			set start = 0p
			set finish = 0p
		end if
		set curr += 1p
	end while

	if counter@i64 == regex.size begin
		write_line[start, (finish+1p-start):i64]
	end if

	set counter@ptr = 0p
end proc
