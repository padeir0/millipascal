# Long Arithmetic for natural numbers (integers >= 0)
# Aritmética de tamanho "arbitrário" para números naturais (inteiros >= 0)
from buffutil import set_buff, copy
from ioutil import fatal, put_int, put_char
from intutil import max

export
    BASE, NAT_SIZE, CAP, EQ, MORE, LESS,
    div, slow_div, # div_digit, div_base, #TODO
    comp, comp_digit,
    mult, mult_digit, mult_base,
    add, add_digit,
    sub, # sub_digit, #TODO
    put_bnat,
    set_bnat, set_zero,
    less, less_eq, more, more_eq, eq, neq,
    put_res

const NAT_SIZE 40
const CAP 8

const BN_SIZE_OFFSET 0p
const BN_FIRST_OFFSET 8p

const BASE 1_000_000_000l # 10^9, 9 digits for each i32
const DIGITS_PER_INT 9

proc main
begin
end proc

proc set_bnat[nat:ptr, a:i32, b:i32, c:i32]
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
memory ERR_DIVISION_BY_ZERO "division by zero\n"
memory WTF "wtf\n"

memory NatIDD 40
memory Scratch 40

const MAXITER 33

# Computes the solution to A = Q*B + Rem
# (where A, B are given and Q, Rem are output pointers)
# by long division
proc div[
    natA:ptr,
    natB:ptr,
    natQ:ptr,
    natRem:ptr
]
var sizeA, sizeB, i, j,
    low:i32, high:i32, guess:i32,
    res
begin
    if comp_digit[natB, 0l] == 0 begin
        fatal[ERR_DIVISION_BY_ZERO, ERR_DIVISION_BY_ZERO.size]
    end if

    set_buff[NatIDD, NAT_SIZE, 0ss]
    set_buff[Scratch, NAT_SIZE, 0ss]

    set sizeA = (natA+BN_SIZE_OFFSET)@i64

    set natA = natA + BN_FIRST_OFFSET

    set (NatIDD+BN_SIZE_OFFSET)@i64 = 0
    set (NatIDD+BN_FIRST_OFFSET)@i32 = 0l

    set i = sizeA-1

    while 0 <= i begin
        mult_base[NatIDD] # multiplies by Base
        add_digit[NatIDD, (natA+(i*sizeof i32):ptr)@i32]

        # find quotient digit
        if more[NatIDD, natB] begin
            set low = 1l
            set guess = BASE/2l
            set high = BASE
            set j = 0
            set res = test_guess[NatIDD, Scratch, natB, guess]
            while res != EQ and j < MAXITER begin
                if res == LESS begin
                    set low = guess
                end elseif res == MORE begin 
                    set high = guess
                end if
                set guess = (low + high)/2l
                set res = test_guess[NatIDD, Scratch, natB, guess]
                set j += 1
            end while
            if j >= MAXITER begin
                exit 4ss
            end if

            copy_bnat[Scratch, natB]
            mult_digit[Scratch, guess]
            sub[NatIDD, Scratch]
            mult_base[natQ]
            add_digit[natQ, guess]
        end elseif eq[NatIDD, natB] begin
            set_zero[NatIDD]
            mult_base[natQ]
            add_digit[natQ, 1l]
        end else begin
            if comp_digit[natQ, 0l] != EQ begin
                mult_base[natQ]
            end if
        end if
        set i -= 1
    end while
    copy_bnat[natRem, NatIDD]
end proc

proc test_guess[
    NatIDD:ptr,
    Scratch:ptr,
    natB:ptr,
    guess:i32
] i64
var res
begin
    copy_bnat[Scratch, natB]
    mult_digit[Scratch, guess]
    set res = comp[Scratch, NatIDD]
    if (res == EQ) begin
        return EQ
    end if
    if res == LESS begin 
        add[Scratch, natB]
        set res = comp[Scratch, NatIDD]
        if res == MORE begin # guess*B < IDD and (guess+1)*B > IDD
            return EQ
        end if
        return LESS # too low
    end if
    if res == MORE begin
        return MORE
    end if
    exit 5ss
end proc

# Computes the solution to A = Q*B + Rem
# (where A, B are given and Q, Rem are output pointers)
# by repeated subtraction
proc slow_div[
    natA:ptr,
    natB:ptr,
    natQ:ptr,
    natRem:ptr
]
begin
    if comp_digit[natB, 0l] == 0 begin
        fatal[ERR_DIVISION_BY_ZERO, ERR_DIVISION_BY_ZERO.size]
    end if
    copy_bnat[natRem, natA]
    while more_eq[natRem, natB] begin
        sub[natRem, natB]
        add_digit[natQ, 1l]
    end while
end proc

proc copy_bnat[natDest:ptr, natSource:ptr] 
var size, i
begin
    set size = (natSource+BN_SIZE_OFFSET)@i64
    set (natDest+BN_SIZE_OFFSET)@i64 = size

    set natDest = natDest + BN_FIRST_OFFSET
    set natSource = natSource + BN_FIRST_OFFSET

    set i = 0
    while i < size begin
        set (natDest+(i*sizeof i32):ptr)@i32 = (natSource+(i*sizeof i32):ptr)@i32
        set i += 1
    end while
end proc

# we will have to mark the pointers that point to objetcs
# so the GC can ignore derived pointers.
#
# proc mult[natA*:ptr, natB*:ptr, natC*:ptr]
# var i, j, sizeA, sizeB, sizeC, carry:i32, res,
#     currdigitA:ptr, currdigitB:ptr, currdigitC:ptr,
#     sizePtr:ptr
# begin

proc mult[natA:ptr, natB:ptr, natC:ptr]
var i, j, sizeA, sizeB, sizeC, carry:i32, res,
    currDigitA:ptr, currDigitB:ptr, currDigitC:ptr,
    sizePtr:ptr
begin
    set_buff[natC, NAT_SIZE, 0ss]
    set sizeA = (natA+BN_SIZE_OFFSET)@i64
    set sizeB = (natB+BN_SIZE_OFFSET)@i64
    set sizeC = sizeA+sizeB
    set sizePtr = natC+BN_SIZE_OFFSET

    set natA = natA + BN_FIRST_OFFSET
    set natB = natB + BN_FIRST_OFFSET
    set natC = natC + BN_FIRST_OFFSET

    set i = 0
    while i < sizeA begin
        set currDigitA = natA+(i*sizeof i32):ptr
        set j = 0
        set carry = 0l
        while (j < sizeB or carry > 0l) and i+j < CAP begin
            set currDigitB = natB+(j*sizeof i32):ptr
            set currDigitC = natC+((i+j)*sizeof i32):ptr
            if j < sizeB begin
                set res = currDigitC@i32:i64 + carry:i64 + currDigitA@i32:i64*currDigitB@i32:i64
            end else begin
                set res = currDigitC@i32:i64 + carry:i64
            end if

            set currDigitC@i32 = (res % BASE:i64):i32
            set carry = (res / BASE:i64):i32
            
            set j += 1
        end while
        if i+j >= CAP begin
            fatal[ERR_NAT_OVERFLOW, ERR_NAT_OVERFLOW.size]
        end if
        set i += 1
    end while

    set i = sizeC-1
    set currDigitC = natC+(i*sizeof i32):ptr
    while currDigitC@i32 == 0l and 0 <= i begin
        set sizeC -= 1
        set i -= 1
        set currDigitC = natC+(i*sizeof i32):ptr
    end while

    set (sizePtr)@i64 = sizeC
end proc

proc mult_base[natA:ptr]
var sizeA, i
begin
    set sizeA = (natA+BN_SIZE_OFFSET)@i64
    set sizeA += 1
    set (natA+BN_SIZE_OFFSET)@i64 = sizeA
    if sizeA > CAP begin
        fatal[ERR_NAT_OVERFLOW, ERR_NAT_OVERFLOW.size]
    end if

    set natA = natA+BN_FIRST_OFFSET
    set i = sizeA-1
    while 0 < i begin
        set (natA+(i*sizeof i32):ptr)@i32 = (natA+((i-1)*sizeof i32):ptr)@i32
        set i -= 1
    end while
    set natA@i32 = 0l # zeroes the first one
end proc

proc mult_digit[natA:ptr, digit:i32]
var i, j, sizeA, carry:i32, res,
    currDigitA:ptr, sizePtr:ptr
begin
    set sizeA = (natA+BN_SIZE_OFFSET)@i64
    set sizePtr = natA+BN_SIZE_OFFSET
    set natA = natA+BN_FIRST_OFFSET

    set i = 0
    set carry = 0l
    while (i < sizeA or carry > 0l) and i < CAP begin
        set currDigitA = natA+(i*sizeof i32):ptr
        if i == sizeA begin
            set sizeA += 1
            set currDigitA@i32 = 0l
        end if

        set res = carry:i64 + currDigitA@i32:i64 * digit:i64

        set currDigitA@i32 = (res % BASE:i64):i32
        set carry = (res / BASE:i64):i32
        
        set i += 1
    end while

    if i >= CAP begin
        fatal[ERR_NAT_OVERFLOW, ERR_NAT_OVERFLOW.size]
    end if
    set (sizePtr)@i64 = sizeA
end proc

proc sub[natA:ptr, natB:ptr]
var i, sizeA, sizeB, carry:i32, currDigitA:ptr, currDigitB:ptr,
    sizePtr:ptr
begin
    set i = comp[natA, natB]

    if i == LESS begin
        fatal[ERR_NAT_NEGATIVE, ERR_NAT_NEGATIVE.size]
    end elseif i == EQ begin
        set_zero[natA]
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
        set currDigitA = natA+(i*sizeof i32):ptr
        set currDigitB = natB+(i*sizeof i32):ptr

        if i < sizeB begin
            set currDigitA@i32 -= carry + currDigitB@i32
        end else begin
            set currDigitA@i32 -= carry
        end if

        if currDigitA@i32 < 0l begin
            set carry = 1l
            set currDigitA@i32 += BASE
        end else begin
            set carry = 0l
        end if

        set i += 1
    end while

    set i = sizeA-1
    set currDigitA = natA+(i*sizeof i32):ptr
    while currDigitA@i32 == 0l and 0 <= i begin
        set sizeA -= 1
        set i -= 1
        set currDigitA = natA+(i*sizeof i32):ptr
    end while

    set (sizePtr)@i64 = sizeA
end proc

proc set_zero[nat:ptr]
begin
    set (nat+BN_SIZE_OFFSET)@i64 = 0
    set (nat+BN_FIRST_OFFSET)@i32 = 0l
end proc

# natA < natB
proc less[natA:ptr, natB:ptr] bool
begin
    return comp[natA, natB] == LESS
end proc

# natA <= natB
proc less_eq[natA:ptr, natB:ptr] bool
var res
begin
    set res = comp[natA, natB]
    return res == EQ or res == LESS
end proc

# natA > natB
proc more[natA:ptr, natB:ptr] bool
begin
    return comp[natA, natB] == MORE
end proc

# natA >= natB
proc more_eq[natA:ptr, natB:ptr] bool
var res
begin
    set res = comp[natA, natB]
    return res == EQ or res == MORE
end proc

# natA != natB
proc neq[natA:ptr, natB:ptr] bool
begin
    return comp[natA, natB] != EQ
end proc

# natA == natB
proc eq[natA:ptr, natB:ptr] bool
begin
    return comp[natA, natB] == EQ
end proc

const EQ 0
const MORE 1
const LESS 2

# ~1 if B < A (A is bigger)
# 0 if A == B
# 1 if A < B (B is bigger)
proc comp[natA:ptr, natB:ptr] i64
var sizeA, sizeB, i, currDigitA:ptr, currDigitB:ptr
begin
    set sizeA = (natA+BN_SIZE_OFFSET)@i64
    set sizeB = (natB+BN_SIZE_OFFSET)@i64

    if sizeB < sizeA begin
        return MORE
    end elseif sizeA < sizeB begin
        return LESS
    end if

    set natA = natA + BN_FIRST_OFFSET
    set natB = natB + BN_FIRST_OFFSET

    set i = sizeA-1 # we go backwards (MSD -> LSD)
    while 0 <= i begin
        set currDigitA = natA+(i*sizeof i32):ptr
        set currDigitB = natB+(i*sizeof i32):ptr
        
        if currDigitB@i32 < currDigitA@i32 begin
            return MORE
        end elseif currDigitA@i32 < currDigitB@i32 begin
            return LESS
        end if
        
        set i -= 1
    end while

    return EQ
end proc

# Compares two numbers, just like comp,
# with the same outputs,
# but the second number is < BASE,
# that is, it's a single digit number.
proc comp_digit[natA:ptr, digit:i32] i64
var sizeA, i, digitA:i32
begin
    set sizeA = (natA+BN_SIZE_OFFSET)@i64

    if digit == 0l and sizeA == 0 begin
        return EQ # both are zero
    end if

    if sizeA > 1 begin
        return MORE
    end if

    set digitA = (natA+BN_FIRST_OFFSET)@i32

    if digit < digitA begin
        return MORE
    end elseif digitA < digit begin
        return LESS
    end if
    return EQ
end proc

# adds nat1 to nat2 and stores in nat1
proc add[natA:ptr, natB:ptr]
var carry:i32, i, sizeA, sizeB, maxSize,
    sizePtr:ptr, currDigitA:ptr
begin
    set sizeA = (natA+BN_SIZE_OFFSET)@i64
    set sizeB = (natB+BN_SIZE_OFFSET)@i64
    set maxSize = max[sizeA, sizeB]

    set sizePtr = natA + BN_SIZE_OFFSET
    set natA = natA + BN_FIRST_OFFSET
    set natB = natB + BN_FIRST_OFFSET

    set i = 0
    set carry = 0l
    while (i < maxSize or carry > 0l) and i < CAP begin
        set currDigitA = natA+(i*sizeof i32):ptr
        if i == sizeA begin
            set sizeA += 1
            set currDigitA@i32 = 0l
        end if
        if i < sizeB begin
            set currDigitA@i32 += carry + (natB+(i*sizeof i32):ptr)@i32
        end else begin
            set currDigitA@i32 += carry
        end if
        if BASE <= currDigitA@i32 begin
            set carry = 1l
            set currDigitA@i32 -= BASE
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

# Adds two numbers, just like add
# but the second number is < BASE,
# that is, it's a single digit number.
proc add_digit[natA:ptr, digit:i32]
var carry:i32, i, sizeA,
    sizePtr:ptr, currDigitA:ptr
begin
    set sizeA = (natA+BN_SIZE_OFFSET)@i64

    set sizePtr = natA + BN_SIZE_OFFSET
    set natA = natA + BN_FIRST_OFFSET

    set i = 0
    set currDigitA = natA+(i*sizeof i32):ptr
    if i == sizeA begin
        set sizeA += 1
        set currDigitA@i32 = 0l
    end if
    set carry = 0l
    set currDigitA@i32 += digit
    if BASE <= currDigitA@i32 begin
        set carry = 1l
        set currDigitA@i32 -= BASE
    end if
    set i += 1
    while carry > 0l and i < CAP begin
        set currDigitA = natA+(i*sizeof i32):ptr
        if i == sizeA begin
            set sizeA += 1
            set currDigitA@i32 = 0l
        end if
        set currDigitA@i32 += carry
        if BASE <= currDigitA@i32 begin
            set carry = 1l
            set currDigitA@i32 -= BASE
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

memory StrBuff 72 # 8 * 9 = 72 digits

proc put_bnat[nat:ptr]
var size
begin
    set size = write_bnat[nat, StrBuff, StrBuff.size]
    write[StrBuff, size]
end proc

proc write_bnat[nat:ptr, buff:ptr, buffSize] i64
var size, first:ptr, back:ptr, block:ptr
begin
    set_buff[buff, buffSize, '0']
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
        put_i32[back@i32, block]
        set block += DIGITS_PER_INT:ptr
        set back -= (sizeof i32):ptr
    end while
    return size * DIGITS_PER_INT
end proc

proc put_i32[n:i32, digits:ptr]
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

memory M "more"
memory L "less"
memory E "eq"

proc put_res[res]
begin
    if res == EQ begin
        write[E, E.size]
        return
    end elseif res == MORE begin
        write[M, M.size]
        return
    end elseif res == LESS begin
        write[L, L.size]
        return
    end if

    write[WTF, WTF.size]
    exit 3ss
end proc
