# naïve utf-8 implementation
# does not protect against encoding/decoding invalid sequences

export
    utf8_encode, utf8_decode,
    rune_size,
    MAX_RUNE_SIZE

proc main
begin
end proc

const MAX_RUNE_SIZE 4

const LOWEST_7_BITS 0b0111_1111uss
const LOWEST_6_BITS 0b0011_1111uss
const LOWEST_5_BITS 0b0001_1111uss
const LOWEST_4_BITS 0b0000_1111uss
const LOWEST_3_BITS 0b0000_0111uss

const TOP_BIT 0b1000_0000uss
const TOP_2_BITS 0b1100_0000uss

proc utf8_decode[p:ptr] i32, i64
begin
    if p@u8 && 0b1000_0000uss == 0uss begin # ascii
        return (p@u8 && LOWEST_7_BITS):i32, 1
    end elseif p@u8 && 0b1110_0000uss == 0b1100_0000uss begin # 2 byte sequence
        if (p+1p)@u8 && TOP_2_BITS != TOP_BIT begin
            return ~1l, 0
        end if
        return ((p@u8 && LOWEST_5_BITS):i32 << 6l) ||
               ((p+1p)@u8 && LOWEST_6_BITS):i32, 2
    end elseif p@u8 && 0b1111_0000uss == 0b1110_0000uss begin # 3 byte sequence
        if (p+1p)@u8 && TOP_2_BITS != TOP_BIT or
           (p+2p)@u8 && TOP_2_BITS != TOP_BIT begin
            return ~1l, 0
        end if
        return ((p@u8 && LOWEST_4_BITS):i32 << 12l) ||
               (((p+1p)@u8 && LOWEST_6_BITS):i32 << 6l) ||
               ((p+2p)@u8 && LOWEST_6_BITS):i32, 3
    end elseif p@u8 && 0b1111_1000uss == 0b1111_0000uss begin # 4 byte sequence
        if (p+1p)@u8 && TOP_2_BITS != TOP_BIT or
           (p+2p)@u8 && TOP_2_BITS != TOP_BIT or
           (p+3p)@u8 && TOP_2_BITS != TOP_BIT begin
            return ~1l, 0
        end if
        return ((p@u8 && LOWEST_4_BITS):i32 << 18l) ||
               (((p+1p)@u8 && LOWEST_6_BITS):i32 << 12l) ||
               (((p+2p)@u8 && LOWEST_6_BITS):i32 << 6l) ||
               ((p+3p)@u8 && LOWEST_6_BITS):i32, 4
    end if
    fatal[ERR_OUT_OF_RANGE,ERR_OUT_OF_RANGE.size]
    return ~1l, 0
end proc

const RANGE_S_1 0x0000_0000l
const RANGE_E_1 0x0000_007Fl

const RANGE_S_2 0x0000_0080l
const RANGE_E_2 0x0000_07FFl

const RANGE_S_3 0x0000_0800l
const RANGE_E_3 0x0000_FFFFl

const RANGE_S_4 0x0001_0800l
const RANGE_E_4 0x0010_FFFFl

memory ERR_OUT_OF_RANGE "rune out of range\n"

# 0x0080
proc utf8_encode[rune:i32, p:ptr] i64
begin
    if RANGE_S_1 <= rune and rune <= RANGE_E_1 begin # ascii
        set p@u8 = (rune && 0b0111_1111l):u8
        return 1
    end elseif RANGE_S_2 <= rune and rune <= RANGE_E_2 begin # 2 byte sequence
        set (p+1p)@u8 = (rune && LOWEST_6_BITS:i32):u8 || 0b1000_0000uss
        set rune = rune >> 6l
        set (p)@u8 = (rune && LOWEST_5_BITS:i32):u8 || 0b1100_0000uss
        return 2
    end elseif RANGE_S_3 <= rune and rune <= RANGE_E_3 begin # 3 byte sequence
        set (p+2p)@u8 = (rune && LOWEST_6_BITS:i32):u8 || 0b1000_0000uss
        set rune = rune >> 6l
        set (p+1p)@u8 = (rune && LOWEST_6_BITS:i32):u8 || 0b1000_0000uss
        set rune = rune >> 6l
        set (p)@u8 = (rune && LOWEST_4_BITS:i32):u8 || 0b1110_0000uss
        return 3
    end elseif RANGE_S_4 <= rune and rune <= RANGE_E_4 begin # 4 byte sequence
        set (p+3p)@u8 = (rune && LOWEST_6_BITS:i32):u8 || 0b1000_0000uss
        set rune = rune >> 6l
        set (p+2p)@u8 = (rune && LOWEST_6_BITS:i32):u8 || 0b1000_0000uss
        set rune = rune >> 6l
        set (p+1p)@u8 = (rune && LOWEST_6_BITS:i32):u8 || 0b1000_0000uss
        set rune = rune >> 6l
        set (p)@u8 = (rune && LOWEST_3_BITS:i32):u8 || 0b1111_0000uss
        return 4
    end else begin
        fatal[ERR_OUT_OF_RANGE,ERR_OUT_OF_RANGE.size]
    end if
    return ~1;
end proc

# returns the size of a rune in bytes
# it that rune were to be encoded
proc rune_size[rune:i32] i64
begin
    if RANGE_S_1 <= rune and rune <= RANGE_E_1 begin # ascii
        return 1
    end elseif RANGE_S_2 <= rune and rune <= RANGE_E_2 begin # 2 byte sequence
        return 2
    end elseif RANGE_S_3 <= rune and rune <= RANGE_E_3 begin # 3 byte sequence
        return 3
    end elseif RANGE_S_4 <= rune and rune <= RANGE_E_4 begin # 4 byte sequence
        return 4
    end else begin
        fatal[ERR_OUT_OF_RANGE,ERR_OUT_OF_RANGE.size]
    end if
    return ~1;
end proc

proc fatal[p:ptr, size]
begin
	write[p, size]
	exit 1ss
end proc
