from utf8
import utf8_encode, MAX_RUNE_SIZE

export
    put_int, put_uint, put_u8, put_u16,
    put_bool, put_char, put_ln, put_rune,
    write_line,
    fatal, debug, oom

proc main
begin
end

data DEBUG_MSG "debug: "
data Debug [sizeof i64] # inits as zero

proc debug[]
begin
	write[DEBUG_MSG, sizeof DEBUG_MSG]
	put_int[Debug@i64]
	put_char['\n']
	set Debug@i64 += 1
end

proc fatal[p:ptr, size:i64]
begin
	write_line[p, size]
	exit 1ss
end

data ERR_OOM "out of memory!\n"

proc oom[]
begin
	write_line[ERR_OOM, sizeof ERR_OOM]
	exit 123ss
end

data PutRuneBuff [MAX_RUNE_SIZE]

proc put_rune[r:i32]
var size:i64
begin
	set size = utf8_encode[r, PutRuneBuff]
	write[PutRuneBuff, size]
end

data PutCharBuff [sizeof i8]

proc put_char[n:i8]
begin
	set PutCharBuff@i8 = n
	write[PutCharBuff, sizeof PutCharBuff]
end

proc put_ln[]
begin
	put_char['\n']
end

proc put_uint[n:u64]
var p:ptr, size:i64
begin
	set p, size = u64_to_dec[n, ConvBuff, sizeof ConvBuff]
	write[p, size]
end

proc put_int[n:i64]
var p:ptr, size:i64
begin
	set p, size = i64_to_dec[n, ConvBuff, sizeof ConvBuff]
	write[p, size]
end

proc put_u8[b:u8]
var p:ptr, size:i64
begin
	set p, size = u8_to_bin[b, ConvBuff, sizeof ConvBuff]
	write[p, size]
end

proc put_u16[b:u16]
var p:ptr, size:i64
begin
	set p, size = u16_to_bin[b, ConvBuff, sizeof ConvBuff]
	write[p, size]
end

proc put_bool[b:bool]
var p:ptr, size:i64
begin
	set p, size = bool_to_text[b]
	write[p, size]
end

proc write_line[p:ptr, size:i64]
begin
	write[p, size]
	put_char['\n']
end

####################### conversion functions

data BUFF_TOO_SMALL "Destination buffer is too small\n"
data ConvBuff [80]

proc u64_to_dec[n:u64, buff:ptr, buffSize:i64] ptr, i64
var p:ptr
begin
	if n == 0u
	begin
		set ConvBuff@i8 = '0'
		return ConvBuff, 1
	end

	set p = buff + buffSize:ptr

	while n > 0u begin
		set p -= 1p
		set p@i8 = (n % 10u):i8 + '0'
		set n /= 10u;
	end

	return p, buffSize-(p-buff):i64 
end

# do not modify the returned buffer
proc i64_to_dec[n:i64, buff:ptr, buffSize:i64] ptr, i64
var sign:i8
begin
	set sign = 0ss

	if n < 0 begin
		set sign = '~'
		set n = ~n
	end

	set buff, buffSize = u64_to_dec[n:u64, buff, buffSize]

	if sign == '~' begin
		set (buff-1p)@i8 = sign
		return buff-1p, buffSize+1
	end
	return buff, buffSize
end

data _T "true"
data _F "false"

# do not modify the returned buffer
proc bool_to_text[b:bool] ptr, i64
begin
	if b
	begin
		return _T, sizeof _T
	end 
	else
	begin
		return _F, sizeof _F
	end
end

proc u8_to_bin[b:u8, buff:ptr, buffSize:i64] ptr, i64
begin
	if buffSize < 9 begin
		fatal[BUFF_TOO_SMALL, sizeof BUFF_TOO_SMALL]
	end
	# loop unroll LOL
	set (buff+0p)@u8 = '0':u8 + ((b && 0b1000_0000uss) != 0uss):u8
	set (buff+1p)@u8 = '0':u8 + ((b && 0b0100_0000uss) != 0uss):u8
	set (buff+2p)@u8 = '0':u8 + ((b && 0b0010_0000uss) != 0uss):u8
	set (buff+3p)@u8 = '0':u8 + ((b && 0b0001_0000uss) != 0uss):u8

	set (buff+4p)@i8 = '_'

	set (buff+5p)@u8 = '0':u8 + ((b && 0b0000_1000uss) != 0uss):u8
	set (buff+6p)@u8 = '0':u8 + ((b && 0b0000_0100uss) != 0uss):u8
	set (buff+7p)@u8 = '0':u8 + ((b && 0b0000_0010uss) != 0uss):u8
	set (buff+8p)@u8 = '0':u8 + ((b && 0b0000_0001uss) != 0uss):u8
	return buff, 9
end

proc u16_to_bin[b:u16, buff:ptr, buffSize:i64] ptr, i64
var i, j:i64,
	mask:u16
begin
	set i = 0
	set j = 0
	set mask = 0b1000_0000_0000_0000us
	while i < 19 begin
		if i >= buffSize begin
			fatal[BUFF_TOO_SMALL, sizeof BUFF_TOO_SMALL]
		end
		set (buff+i:ptr)@u8 = '0':u8 + ((b && mask) != 0us):u8

		if j == 3 begin
			set i += 1
			set (buff+i:ptr)@i8 = '_'
		end

		set mask = mask >> 1us
		set i += 1
		set j = (j+1)%4
	end
	return buff, 19
end
