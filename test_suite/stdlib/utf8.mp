# naïve utf-8 implementation
# does not protect against encoding/decoding invalid sequences
from io import print

export
    utf8_encode, utf8_decode,
    rune_size,
    MAX_RUNE_SIZE

const MAX_RUNE_SIZE = 4

const LOWEST_7_BITS = 0b0111_1111uss
const LOWEST_6_BITS = 0b0011_1111uss
const LOWEST_5_BITS = 0b0001_1111uss
const LOWEST_4_BITS = 0b0000_1111uss
const LOWEST_3_BITS = 0b0000_0111uss

const TOP_BIT = 0b1000_0000uss
const TOP_2_BITS = 0b1100_0000uss

proc utf8_decode[p:ptr] i32, i32
begin
    if p@u8 & 0b1000_0000uss == 0uss begin # ascii
        return (p@u8 & LOWEST_7_BITS):i32, 1;
    end elseif p@u8 & 0b1110_0000uss == 0b1100_0000uss begin # 2 byte sequence
        if (p+1)@u8 & TOP_2_BITS != TOP_BIT begin
            return ~1, 0;
        end
        return ((p@u8    & LOWEST_5_BITS):i32 << 6) |
               ((p+1)@u8 & LOWEST_6_BITS):i32, 2;
    end elseif p@u8 & 0b1111_0000uss == 0b1110_0000uss begin # 3 byte sequence
        if (p+1)@u8 & TOP_2_BITS != TOP_BIT or
           (p+2)@u8 & TOP_2_BITS != TOP_BIT begin
            return ~1, 0;
        end
        return ((p@u8     & LOWEST_4_BITS):i32 << 12) |
               (((p+1)@u8 & LOWEST_6_BITS):i32 << 6) |
               ((p+2)@u8  & LOWEST_6_BITS):i32, 3;
    end elseif p@u8 & 0b1111_1000uss == 0b1111_0000uss begin # 4 byte sequence
        if (p+1)@u8 & TOP_2_BITS != TOP_BIT or
           (p+2)@u8 & TOP_2_BITS != TOP_BIT or
           (p+3)@u8 & TOP_2_BITS != TOP_BIT begin
            return ~1, 0;
        end
        return ((p@u8     & LOWEST_3_BITS):i32 << 18) |
               (((p+1)@u8 & LOWEST_6_BITS):i32 << 12) |
               (((p+2)@u8 & LOWEST_6_BITS):i32 << 6) |
               ((p+3)@u8  & LOWEST_6_BITS):i32, 4;
    end
    fatal[ERR_OUT_OF_RANGE, sizeof[ERR_OUT_OF_RANGE]];
    return ~1, 0;
end

const RANGE_S_1 = 0x0000_0000
const RANGE_E_1 = 0x0000_007F

const RANGE_S_2 = 0x0000_0080
const RANGE_E_2 = 0x0000_07FF

const RANGE_S_3 = 0x0000_0800
const RANGE_E_3 = 0x0000_FFFF

const RANGE_S_4 = 0x0001_0800
const RANGE_E_4 = 0x0010_FFFF

data ERR_OUT_OF_RANGE "rune out of range\n"

# 0x0080
proc utf8_encode[rune:i32, p:ptr] i32
begin
    if RANGE_S_1 <= rune and rune <= RANGE_E_1 begin # ascii
        set p@u8 = (rune & 0b0111_1111):u8;
        return 1;
    end elseif RANGE_S_2 <= rune and rune <= RANGE_E_2 begin # 2 byte sequence
        set (p+1)@u8 = (rune & LOWEST_6_BITS:i32):u8 | 0b1000_0000uss;
        set rune = rune >> 6;
        set (p)@u8 = (rune & LOWEST_5_BITS:i32):u8 | 0b1100_0000uss;
        return 2;
    end elseif RANGE_S_3 <= rune and rune <= RANGE_E_3 begin # 3 byte sequence
        set (p+2)@u8 = (rune & LOWEST_6_BITS:i32):u8 | 0b1000_0000uss;
        set rune = rune >> 6;
        set (p+1)@u8 = (rune & LOWEST_6_BITS:i32):u8 | 0b1000_0000uss;
        set rune = rune >> 6;
        set (p)@u8 = (rune & LOWEST_4_BITS:i32):u8 | 0b1110_0000uss;
        return 3;
    end elseif RANGE_S_4 <= rune and rune <= RANGE_E_4 begin # 4 byte sequence
        set (p+3)@u8 = (rune & LOWEST_6_BITS:i32):u8 | 0b1000_0000uss;
        set rune = rune >> 6;
        set (p+2)@u8 = (rune & LOWEST_6_BITS:i32):u8 | 0b1000_0000uss;
        set rune = rune >> 6;
        set (p+1)@u8 = (rune & LOWEST_6_BITS:i32):u8 | 0b1000_0000uss;
        set rune = rune >> 6;
        set (p)@u8 = (rune & LOWEST_3_BITS:i32):u8 | 0b1111_0000uss;
        return 4;
    end else begin
        fatal[ERR_OUT_OF_RANGE, sizeof[ERR_OUT_OF_RANGE]];
    end
    return ~1;
end

# returns the size of a rune in bytes
# if that rune were to be encoded
proc rune_size[rune:i32] i32
begin
    if RANGE_S_1 <= rune and rune <= RANGE_E_1 begin # ascii
        return 1;
    end elseif RANGE_S_2 <= rune and rune <= RANGE_E_2 begin # 2 byte sequence
        return 2;
    end elseif RANGE_S_3 <= rune and rune <= RANGE_E_3 begin # 3 byte sequence
        return 3;
    end elseif RANGE_S_4 <= rune and rune <= RANGE_E_4 begin # 4 byte sequence
        return 4;
    end else begin
        fatal[ERR_OUT_OF_RANGE, sizeof[ERR_OUT_OF_RANGE]];
    end
    return ~1;
end

proc fatal[p:ptr, size:i32]
begin
	print[p, size];
	exit 1ss;
end
