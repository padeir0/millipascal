from io import print, fatal

export
	OK, ERR_OVERFLOW, ERR_UNDERFLOW, ERR_NOTNUM,
	
	i64_to_i32, i32_to_i16,
	ptr_to_i64, ptr_to_i32,
	u64_to_i32,

	I64MAX,
	I32MAX, I32MIN,
	I16MAX, I16MIN,
	
    u64_to_dec, dec_to_u64,
    i64_to_dec, dec_to_i64,

	u16_to_bin, u8_to_bin,
	bool_to_text,

data begin
	NEG_DIS "distance was negative\n";
	BUFF_TOO_SMALL "buffer is too small\n";
end

const begin
	OK = 0ss;
	ERR_OVERFLOW = 1ss;
	ERR_UNDERFLOW = 2ss;
	ERR_NOTNUM = 4ss;
end

const begin
    # const evaluation saturates instead of overflowing
    I64MAX = (1l<<65l):i64;
    I64MIN = (~(1l<<65l)):i64;

    I32MAX = (1<<33):i32;
    I32MIN = (~(1<<33)):i32;

    I16MAX = (1<<17):i16;
    I16MIN = (~(1<<17)):i16;

	I8MAX = (1<<9):i8;
	I8MIN = (~(1<<9)):i8;
end

proc i64_to_i32[a:i64] i32, i8
begin
    if a < I32MIN:i64 begin
		return 0, ERR_OVERFLOW;
    end
    if a > I32MAX:i64 begin
		return 0, ERR_UNDERFLOW;
    end
    if a < 0l begin
        return ~( (~a):i32 ), OK;
    end
    return a:i32, OK;
end

proc i32_to_i16[a:i32] i16, i8
begin
    if a < I16MIN:i32 begin
		return 0s, ERR_OVERFLOW;
    end
    if a > I16MAX:i32 begin
		return 0s, ERR_UNDERFLOW;
    end
    if a < 0 begin
        return ~( (~a):i16 ), OK;
    end
    return a:i16, OK;
end

proc u64_to_i64[n:u64] i64, i8
begin
    if n > I64MAX:u64 begin
		return 0l, ERR_OVERFLOW;
    end
    return n:i64, OK;
end

proc ptr_to_i64[p:ptr] i64, i8
begin
    if p > I64MAX:ptr begin
		return 0l, ERR_OVERFLOW;
    end
    return p:i64, OK;
end

proc ptr_to_i32[p:ptr] i32, i8
begin
    if p > I32MAX:ptr begin
		return 0, ERR_OVERFLOW;
    end
    return p:i32, OK;
end

proc u64_to_i32[a:u64] i32, i8
begin
    if a > I32MAX:u64 begin
		return 0, ERR_OVERFLOW;
    end
    return a:i32, OK;
end

# [0-9]+
proc dec_to_u64[buff:ptr, buffSize:i32] u64, i8
var prev, out:u64, i:i32, c:i8
begin
	set i = 0;
	set out = 0ul;
	set prev = 0ul;
	while i < buffSize begin
		set c = (buff+i)@i8;
		if '0' <= c and c <= '9' begin
			set out *= 10ul;
			if out < prev begin
				return 0ul, ERR_OVERFLOW;
			end
			set out += (c-'0'):u64;

			set prev = out;
		end else begin
			return c:u64, ERR_NOTNUM;
		end
		set i += 1;
	end
	return out, OK;
end

proc u64_to_dec[n:u64, buff:ptr, buffSize:i32] ptr, i32
var p:ptr, size:i64
begin
	if n == 0ul begin
		set buff@i8 = '0';
		return buff, 1;
	end

	set p = buff + buffSize-1;

	while n > 0ul and p > buff begin
		set p -= 1;
		set p@i8 = (n % 10ul):i8 + '0';
		set n /= 10ul;
	end

	set size = p:i64-buff:i64 + 1l;
	if size < 0l begin # sanity check: this should never happen
		fatal[NEG_DIS, sizeof[NEG_DIS]];
	end

	# downcasting i64 -> i32 should be ok as long as i64 is positive
	return p, buffSize-(size:i32);
end

# ~?[0-9]+
proc dec_to_i64[buff:ptr, buffSize:i32] i64, i8
var i:i32, r:u64, neg:bool, status:i8
begin
	set i = 0;
	set neg = false;
	if buff@i8 == '~' begin
		set neg = true;
		set i += 1;
	end
	set r, status = dec_to_u64[buff+i, buffSize-i];
	if status != OK begin
		return 0l, status;
	end
	if r > I64MAX:u64 begin
		return 0l, ERR_OVERFLOW;
	end
	if neg begin
		return ~(r:i64), OK;
	end
	return r:i64, OK;
end

proc i64_to_dec[n:i64, buff:ptr, buffSize:i32] ptr, i32
var sign:i8
begin
	set sign = 0ss;

	if n < 0l begin
		set sign = '~';
		set n = ~n;
	end

	set buff, buffSize = u64_to_dec[n:u64, buff, buffSize];

	if sign == '~' begin
		set (buff-1)@i8 = sign;
		return buff-1, buffSize+1;
	end
	return buff, buffSize;
end

data _T "true"
data _F "false"

# do not modify the returned buffer
proc bool_to_text[b:bool] ptr, i32
begin
	if b
	begin
		return _T, sizeof[_T];
	end 
	else
	begin
		return _F, sizeof[_F];
	end
end

proc u8_to_bin[b:u8, buff:ptr, buffSize:i32] ptr, i32
begin
	if buffSize < 9 begin
		fatal[BUFF_TOO_SMALL, sizeof[BUFF_TOO_SMALL]];
	end
	# loop unroll LOL
	set (buff+0)@u8 = '0':u8 + ((b & 0b1000_0000uss) != 0uss):u8;
	set (buff+1)@u8 = '0':u8 + ((b & 0b0100_0000uss) != 0uss):u8;
	set (buff+2)@u8 = '0':u8 + ((b & 0b0010_0000uss) != 0uss):u8;
	set (buff+3)@u8 = '0':u8 + ((b & 0b0001_0000uss) != 0uss):u8;

	set (buff+4)@i8 = '_';

	set (buff+5)@u8 = '0':u8 + ((b & 0b0000_1000uss) != 0uss):u8;
	set (buff+6)@u8 = '0':u8 + ((b & 0b0000_0100uss) != 0uss):u8;
	set (buff+7)@u8 = '0':u8 + ((b & 0b0000_0010uss) != 0uss):u8;
	set (buff+8)@u8 = '0':u8 + ((b & 0b0000_0001uss) != 0uss):u8;
	return buff, 9;
end

proc u16_to_bin[b:u16, buff:ptr, buffSize:i32] ptr, i32
var i, j:i32,
	mask:u16
begin
	set i = 0;
	set j = 0;
	set mask = 0b1000_0000_0000_0000us;
	if buffSize < 19 begin
		fatal[BUFF_TOO_SMALL, sizeof[BUFF_TOO_SMALL]];
	end
	while i < 19 begin
		set (buff+i)@u8 = '0':u8 + ((b & mask) != 0us):u8;

		if j == 3 begin
			set i += 1;
			set (buff+i)@i8 = '_';
		end

		set mask = mask >> 1us;
		set i += 1;
		set j = (j+1)%4;
	end
	return buff, 19;
end
