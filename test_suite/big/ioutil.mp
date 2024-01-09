from conversion import itoa, btoa

export put_int, put_bool, put_char, write_line, fatal, debug

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
