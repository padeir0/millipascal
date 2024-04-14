data begin
    ONE "1";
    TWENTYSEVEN "27";
    NINENINENINE "999";
end

const begin
	OK = 0ss;
	ERR_OVERFLOW = 1ss;
	ERR_UNDERFLOW = 2ss;
	ERR_NOTNUM = 4ss;
end

proc main
var res:u64, status:i8
begin
    set res, status = conv1[ONE, sizeof[ONE]];
    if status != OK begin
        exit 30ss + status;
    end
    if res != 1ul begin
        exit 12ss;
    end

    set res, status = conv1[TWENTYSEVEN, sizeof[TWENTYSEVEN]];
    if status != OK begin
        exit 30ss + status;
    end
    if res != 27ul begin
        exit 13ss;
    end

    set res, status = conv1[NINENINENINE, sizeof[NINENINENINE]];
    if status != OK begin
        exit 30ss + status;
    end
    if res != 999ul begin
        exit 14ss;
    end

    set res, status = conv2[ONE, sizeof[ONE]];
    if status != OK begin
        exit 30ss + status;
    end
    if res != 1ul begin
        exit 15ss;
    end

    set res, status = conv2[TWENTYSEVEN, sizeof[TWENTYSEVEN]];
    if status != OK begin
        exit 30ss + status;
    end
    if res != 27ul begin
        exit 16ss;
    end

    set res, status = conv2[NINENINENINE, sizeof[NINENINENINE]];
    if status != OK begin
        exit 30ss + status;
    end
    if res != 999ul begin
        exit 17ss;
    end
end

proc p[]
begin
	return;
end

proc conv1[buff:ptr, buffSize:i32] u64, i8
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
			p[];

			set prev = out;
		end else begin
			return c:u64, ERR_NOTNUM;
		end
		set i += 1;
	end
	return out, OK;
end

proc conv2[buff:ptr, buffSize:i32] u64, i8
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
