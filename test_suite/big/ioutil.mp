from conversion import itoa, btoa

export put_int, put_int_ln, put_2int, put_3int, put_4int, put_5int,
       put_bool, put_char, put_ln, write_line, fatal, debug

proc main
begin
	exit 0ss
end proc

memory _debug "debug: "

proc debug[c:i8]
begin
	write[_debug, _debug.size]
	put_char[c]
	put_char['\n']
end proc

proc fatal[p:ptr, size]
begin
	write_line[p, size]
	exit 1ss
end proc

memory pchar_buff 1

proc put_char[n:i8]
begin
	set pchar_buff@i8 = n
	write[pchar_buff, pchar_buff.size]
end proc

proc put_ln[]
begin
	put_char['\n']
end proc

proc put_5int[a, b, c, d, e]
begin
	put_int[a]
	put_char[',']
	put_char[' ']
	put_int[b]
	put_char[',']
	put_char[' ']
	put_int[c]
	put_char[',']
	put_char[' ']
	put_int[d]
	put_char[',']
	put_char[' ']
	put_int[e]
end proc

proc put_4int[a, b, c, d]
begin
	put_int[a]
	put_char[',']
	put_char[' ']
	put_int[b]
	put_char[',']
	put_char[' ']
	put_int[c]
	put_char[',']
	put_char[' ']
	put_int[d]
end proc

proc put_3int[a, b, c]
begin
	put_int[a]
	put_char[',']
	put_char[' ']
	put_int[b]
	put_char[',']
	put_char[' ']
	put_int[c]
end proc

proc put_2int[a, b]
begin
	put_int[a]
	put_char[',']
	put_char[' ']
	put_int[b]
end proc

proc put_int_ln[n]
begin
	put_int[n]
	put_char['\n']
end proc

proc put_int[n]
var p:ptr, size
begin
	set p, size = itoa[n]
	write[p, size]
end proc

proc put_bool[b:bool]
var p:ptr, size
begin
	set p, size = btoa[b]
	write[p, size]
end proc

proc write_line[p:ptr, size:i64]
begin
	write[p, size]
	put_char['\n']
end proc
