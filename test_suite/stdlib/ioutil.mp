from io import print, fatal

from utf8
import utf8_encode, MAX_RUNE_SIZE

from conv
import all

export
    put_int, put_i32, put_uint, put_u8, put_u16,
    put_bool, put_char, put_ln, put_rune,
    println, debug, oom

data DEBUG_MSG "debug: "
data Debug [sizeof[i64]] # inits as zero

proc debug[]
begin
	print[DEBUG_MSG, sizeof[DEBUG_MSG]];
	put_int[Debug@i64];
	put_char['\n'];
	set Debug@i64 += 1l;
end

data ERR_OOM "out of memory!\n"

proc oom[]
begin
	print[ERR_OOM, sizeof[ERR_OOM]];
	exit 123ss;
end

data begin
	PutCharBuff [sizeof[i8]];
	ConvBuff [128];
	PutRuneBuff [MAX_RUNE_SIZE];
end

proc put_rune[r:i32]
var size:i32
begin
	set size = utf8_encode[r, PutRuneBuff];
	print[PutRuneBuff, size];
end

proc put_char[n:i8]
begin
	set PutCharBuff@i8 = n;
	print[PutCharBuff, sizeof[PutCharBuff]];
end

proc put_ln[]
begin
	put_char['\n'];
end

proc put_uint[n:u64]
var p:ptr, size:i32
begin
	set p, size = u64_to_dec[n, ConvBuff, sizeof[ConvBuff]];
	print[p, size];
end

proc put_int[n:i64]
var p:ptr, size:i32
begin
	set p, size = i64_to_dec[n, ConvBuff, sizeof[ConvBuff]];
	print[p, size];
end

proc put_i32[n:i32]
begin
	put_int[n:i64];
end

proc put_u8[b:u8]
var p:ptr, size:i32
begin
	set p, size = u8_to_bin[b, ConvBuff, sizeof[ConvBuff]];
	print[p, size];
end

proc put_u16[b:u16]
var p:ptr, size:i32
begin
	set p, size = u16_to_bin[b, ConvBuff, sizeof[ConvBuff]];
	print[p, size];
end

proc put_bool[b:bool]
var p:ptr, size:i32
begin
	set p, size = bool_to_text[b];
	print[p, size];
end

proc println[p:ptr, size:i32]
begin
	print[p, size];
	put_char['\n'];
end
