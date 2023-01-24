from conversion import itoa, btoa

export put_int, put_bool, put_char, write_line, fatal, debug

memory here "here\n"

proc debug
begin
	write[here, here.size]
end proc

proc fatal[p:ptr, size]
begin
	write_line[p, size]
	exit 1r
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
