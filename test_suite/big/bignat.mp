# Aritmética de tamanho "arbitrário" para números naturais (inteiros >= 0)
from buffutil import set_buff, copy
from ioutil import fatal, put_int, put_char
from intutil import max

memory num_A 40 # [length:i64, i32...]
memory num_B 40 # 8 * i32
memory num_C 40

const NAT_SIZE 40
const CAP 8

const BN_SIZE_OFFSET 0p
const BN_FIRST_OFFSET 8p

const BASE 1_000_000_000l # 10^9, 9 digitos para cada i32
const DIGITS_PER_INT 9

proc main
var i, a:ptr, b:ptr, c:ptr, d:ptr
begin
    set a = num_A
    set b = num_B
    set c = num_C

    setNat[a, 5l, 0l, 0l]
    putBigNat[a]
    put_char['\n']

    setNat[b, 5l, 0l, 0l]
    putBigNat[b]
    put_char['\n']

    SetZero[c]

    # set i = 0
    # while i < 50 begin
    #     putBigNat[a]
    #     put_char['*']
    #     putBigNat[b]
    #     Mult[a, b, c]
    #     put_char['=']
    #     putBigNat[c]
    #     put_char['\n']

    #     set d = a
    #     set a = c

    #     set_buff[d, NAT_SIZE, 0ss]
    #     set c = d

    #     set i += 1
    # end while
end proc

proc printComp[natA:ptr, natB:ptr]
var i
begin
    set i = Comp[num_A, num_B]
    put_char['A']
    if i == ~1 begin
        put_char['>']
    end elseif i == 1 begin
        put_char['<']
    end else begin
        put_char['=']
    end if
    put_char['B']
    put_char['\n']
end proc

proc setNat[nat:ptr, a:i32, b:i32, c:i32]
var size, first:ptr
begin
    set size = 0
    set_buff[nat, NAT_SIZE, 0ss]
    set first = nat + BN_FIRST_OFFSET
    if a > 0l begin
        set first@i32 = a
        set size = 1
    end if
    if b > 0l begin
        set (first+4p)@i32 = b
        set size = 2
    end if
    if c > 0l begin
        set (first+8p)@i32 = c
        set size = 3
    end if

    set (nat+BN_SIZE_OFFSET)@i64 = size
end proc

memory ERR_NAT_OVERFLOW "number is too big (max 72 digits)\n"
memory ERR_NAT_NEGATIVE "number subtraction went negative\n"

proc Mult[natA:ptr, natB:ptr, natC:ptr]
var i, j, sizeA, sizeB, sizeC, carry:i32, res,
    currdigitA:ptr, currdigitB:ptr, currdigitC:ptr,
    sizePtr:ptr
begin
    set_buff[natC, NAT_SIZE, 0ss]
    set sizeA = (natA+BN_SIZE_OFFSET)@i64
    set sizeB = (natB+BN_SIZE_OFFSET)@i64
    set sizeC = sizeA+sizeB
    if CAP < sizeC begin
        fatal[ERR_NAT_OVERFLOW, ERR_NAT_OVERFLOW.size]
    end if
    set sizePtr = natC+BN_SIZE_OFFSET

    set natA = natA + BN_FIRST_OFFSET
    set natB = natB + BN_FIRST_OFFSET
    set natC = natC + BN_FIRST_OFFSET

    set i = 0
    while i < sizeA begin
        set currdigitA = natA+(i*sizeof i32):ptr
        set j = 0
        set carry = 0l
        while j < sizeB or carry > 0l begin
            set currdigitB = natB+(j*sizeof i32):ptr
            set currdigitC = natC+((i+j)*sizeof i32):ptr
            if j < sizeB begin
                set res = currdigitC@i32:i64 + carry:i64 + currdigitA@i32:i64*currdigitB@i32:i64
            end else begin
                set res = currdigitC@i32:i64 + carry:i64
            end if

            set currdigitC@i32 = (res % BASE:i64):i32
            set carry = (res / BASE:i64):i32
            
            set j += 1
        end while
        set i += 1
    end while

    set i = sizeC-1
    set currdigitC = natC+(i*sizeof i32):ptr
    while currdigitC@i32 == 0l and 0 <= i begin
        set sizeC -= 1
        set i -= 1
        set currdigitC = natC+(i*sizeof i32):ptr
    end while

    set (sizePtr)@i64 = sizeC
end proc

proc Sub[natA:ptr, natB:ptr]
var i, sizeA, sizeB, carry:i32, currdigitA:ptr, currdigitB:ptr,
    sizePtr:ptr
begin
    set i = Comp[natA, natB]

    if i == 1 begin
        fatal[ERR_NAT_NEGATIVE, ERR_NAT_NEGATIVE.size]
    end elseif i == 0 begin
        SetZero[natA]
        return;
    end if

    # por causa da verificação anterior,
    # sabemos que sizeB <= sizeA
    set sizeA = (natA+BN_SIZE_OFFSET)@i64
    set sizeB = (natB+BN_SIZE_OFFSET)@i64
    set sizePtr = natA+BN_SIZE_OFFSET

    set natA = natA + BN_FIRST_OFFSET
    set natB = natB + BN_FIRST_OFFSET

    set carry = 0l
    set i = 0
    while i < sizeB or carry > 0l begin
        set currdigitA = natA+(i*sizeof i32):ptr
        set currdigitB = natB+(i*sizeof i32):ptr

        if i < sizeB begin
            set currdigitA@i32 -= carry + currdigitB@i32
        end else begin
            set currdigitA@i32 -= carry
        end if

        if currdigitA@i32 < 0l begin
            set carry = 1l
            set currdigitA@i32 += BASE
        end else begin
            set carry = 0l
        end if

        set i += 1
    end while

    set i = sizeA-1
    set currdigitA = natA+(i*sizeof i32):ptr
    while currdigitA@i32 == 0l and 0 <= i begin
        set sizeA -= 1
        set i -= 1
        set currdigitA = natA+(i*sizeof i32):ptr
    end while

    set (sizePtr)@i64 = sizeA
end proc

proc SetZero[nat:ptr]
begin
    set (nat+BN_SIZE_OFFSET)@i64 = 0
    set (nat+BN_FIRST_OFFSET)@i32 = 0l
end proc

# ~1 if B < A (A is bigger)
# 0 if A == B
# 1 if A < B (B is bigger)
proc Comp[natA:ptr, natB:ptr] i64
var sizeA, sizeB, i, currdigitA:ptr, currdigitB:ptr
begin
    set sizeA = (natA+BN_SIZE_OFFSET)@i64
    set sizeB = (natB+BN_SIZE_OFFSET)@i64

    if sizeB < sizeA begin
        return ~1
    end elseif sizeA < sizeB begin
        return 1
    end if

    set natA = natA + BN_FIRST_OFFSET
    set natB = natB + BN_FIRST_OFFSET

    set i = sizeA-1 # vamos de trás pra frente (MSD -> LSD)
    while 0 <= i begin
        set currdigitA = natA+(i*sizeof i32):ptr
        set currdigitB = natB+(i*sizeof i32):ptr
        
        if currdigitB@i32 < currdigitA@i32 begin
            return ~1
        end elseif currdigitA@i32 < currdigitB@i32 begin
            return 1
        end if
        
        set i -= 1
    end while

    return 0
end proc

# adds nat1 to nat2 and stores in nat1
proc Add[natA:ptr, natB:ptr]
var carry:i32, i, sizeA, sizeB, maxsize,
    sizePtr:ptr, currdigitA:ptr
begin
    set sizeA = (natA+BN_SIZE_OFFSET)@i64
    set sizeB = (natB+BN_SIZE_OFFSET)@i64
    set maxsize = max[sizeA, sizeB]

    set sizePtr = natA + BN_SIZE_OFFSET
    set natA = natA + BN_FIRST_OFFSET
    set natB = natB + BN_FIRST_OFFSET

    set i = 0
    set carry = 0l
    while (i < maxsize or carry > 0l) and i < CAP begin
        set currdigitA = natA+(i*sizeof i32):ptr
        if i == sizeA begin
            set sizeA += 1
            set currdigitA@i32 = 0l
        end if
        if i < sizeB begin
            set currdigitA@i32 += carry + (natB+(i*sizeof i32):ptr)@i32
        end else begin
            set currdigitA@i32 += carry
        end if
        if BASE <= currdigitA@i32 begin
            set carry = 1l
            set currdigitA@i32 -= BASE
        end else begin
            set carry = 0l
        end if
        set i += 1
    end while
    if i >= CAP begin
        fatal[ERR_NAT_OVERFLOW, ERR_NAT_OVERFLOW.size]
    end if
    set (sizePtr)@i64 = sizeA
end proc


memory ERR_STR_OVERFLOW "overflow of string buffer\n"

memory strbuff 72 # 8 * 9 = 72 digitos

proc putBigNat[nat:ptr]
var size
begin
    set size = writeBigNat[nat, strbuff, strbuff.size]
    write[strbuff, size]
end proc

proc writeBigNat[nat:ptr, buff:ptr, buff_size] i64
var size, first:ptr, back:ptr, block:ptr
begin
    set_buff[buff, buff_size, '0']
    set size = (nat+BN_SIZE_OFFSET)@i64
    if size == 0 begin
        return 1;
    end if
    set first = (nat+BN_FIRST_OFFSET)
    set back = first+((size-1)*sizeof i32):ptr

    set block = buff
    while first <= back begin
        if block < buff begin
            fatal[ERR_STR_OVERFLOW, ERR_STR_OVERFLOW.size]
        end if
        puti32[back@i32, block]
        set block += DIGITS_PER_INT:ptr
        set back -= (sizeof i32):ptr
    end while
    return size * DIGITS_PER_INT
end proc

proc puti32[n:i32, digits:ptr]
var p:ptr
begin
	if n == 0l
	begin
        return; # already set to zero
	end if
	set p = digits + DIGITS_PER_INT:ptr - 1p # end of buffer
	while n > 0l begin
		set p@i8 = (n % 10l):i8 + '0'
		set n /= 10l;
		set p -= 1p
	end while
end proc
