from conversion import itoa, btoa

export putint, putbool, putchar, writeLn, fatal, debug

memory here "here\n"

proc debug
begin
	write[here, here.size]
end proc

proc fatal[p:ptr, size]
begin
	writeLn[p, size]
	exit 1r
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

proc putbool[b:bool]
var p:ptr, size
begin
	set p, size = btoa[b]
	write[p, size]
end proc

proc writeLn[p:ptr, size:i64]
begin
	write[p, size]
	putchar['\n']
end proc
