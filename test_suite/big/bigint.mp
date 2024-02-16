# Here be dragons.
#
# Long Arithmetic for integers
# Aritmética de tamanho "arbitrário" para números inteiros 

# nomeclature:
#     we call x a "digit" in procedures only if 0 <= x < BASE
#     when we refer to x as "i32", we expect it to be ~BASE < x < BASE
#
#     in procedures with a single big int argument, we simply call
#     that argument "bint", while if multiple big ints are used, we
#     prefix de variable name with "i", but never only "i" (eg. "iA")
#
#     procedures that have "abs" prefix deal only with numbers >= 0,
#     while exported procedures will deal only with whole integers.

from constants import NULL, WORD
from buffutil import set_buff, copy
from ioutil import fatal, put_int, put_char, put_bool, oom
from intutil import min, max, abs_i32
from dfl_alloc
import alloc_f, alloc_b, free, init, print_list, print_map,
       validate_init

export
    new_bint, set_zero, set_bint,
    put_bint, copy_bint, free_bint,

    BASE, EQ, MORE, LESS,
    put_res,
    put_bint_header,

    comp, comp_i32,
    div,  div_i32,
    mult, mult_i32,
    add,  add_i32,
    sub,  sub_i32,
    neg

proc main
begin
end proc

data ERR_SET_INVALID_DIGIT "setting bigint to invalid digit\n"
data ERR_SET_INVALID_I32 "setting bigint to invalid i32\n"
data ERR_DIVISION_BY_ZERO "division by zero\n"
data WTF "wtf\n"

const BASE = 1_000_000_000l # 10^9, 9 digits for each i32
const DIGITS_PER_INT = 9

# Vector Header (negative offset)
#  ├── 4 bytes, offset -8: object size in words (for the allocator)
#  └── 1 byte, offset -1: integer sign (bool)
# we can do this because there's a 4 byte padding in the object header
# that we're allowed to use for other stuff

const ARRAY_NEG_OFFSET = 1p

# BigInt Header (positive offset)
#  ├── 4 bytes, offset 0: capacity
#  ├── 4 bytes, offset 4: length
#  └── 8 bytes, offset 8: pointer to array (on front heap)

const BINT_HEADERSIZE = 16

const BINT_CAP_OFFSET = 0p
const BINT_LEN_OFFSET = 4p
const BINT_PTR_OFFSET = 8p

# pointer in second word
const BINT_BITMAP = 0b01000000_00000000_00000000_00000000ul

######################################## $UTILITARIES

data ERR_STR_OVERFLOW "overflow of string buffer\n"

data StrBuff [1_000]

proc put_bint[bint:ptr]
var size
begin
    set size = write_bint[bint, StrBuff, StrBuff.size]
    write[StrBuff, size]
    set_buff[StrBuff, size, 0ss]
end proc

proc write_bint[bint:ptr, buff:ptr, buffSize] i64
var len, i, array:ptr, currDigit:ptr,
    block:ptr, first:bool, neg:bool,
    size
begin
    set len = (bint+BINT_LEN_OFFSET)@i32:i64
    set array = (bint+BINT_PTR_OFFSET)@ptr
    set neg = (array-ARRAY_NEG_OFFSET)@bool

    if len == 0 begin
        set buff@i8 = '0'
        return 1;
    end if

    set size = 0
    set i = len-1
    set block = buff
    set first = true
    if neg begin
        set block@i8 = '~'
        set block += 1p
        set size += 1
    end if
    while 0 <= i begin
        set currDigit = array + (i*sizeof i32):ptr
        if (block - buff):i64 >= buffSize-DIGITS_PER_INT begin
            fatal[ERR_STR_OVERFLOW, ERR_STR_OVERFLOW.size]
        end if
        put_i32[currDigit@i32, block, first]
        set first = false
        set block += DIGITS_PER_INT:ptr
        set size += DIGITS_PER_INT
        set i -= 1
    end while
    return size
end proc

proc put_i32[n:i32, digits:ptr, first:bool]
var p:ptr, i
begin
	if n == 0l
	begin
        set i = 0
        while i <= 9 begin
    		set (digits + i:ptr)@i8 = '0'
            set i += 1
        end while
        return
	end if
	set p = digits + DIGITS_PER_INT:ptr - 1p # end of buffer
	while n > 0l begin
		set p@i8 = (n % 10l):i8 + '0'
		set n /= 10l;
		set p -= 1p
	end while

    # padding
    if not first begin
        while p >= digits begin
    		set p@i8 = '0'
            set p -= 1p
        end while
    end if
end proc

proc put_res[res]
begin
    if res == EQ begin
        put_char['=']
        return
    end elseif res == MORE begin
        put_char['>']
        return
    end elseif res == LESS begin
        put_char['<']
        return
    end if

    write[WTF, WTF.size]
    exit 3ss
end proc

proc verify_digit[digit:i32]
begin
    if digit < 0l or digit >= BASE begin
        fatal[ERR_SET_INVALID_DIGIT, ERR_SET_INVALID_DIGIT.size]
    end if
end proc

proc verify_i32[digit:i32]
begin
    if digit <= ~BASE or digit >= BASE begin
        fatal[ERR_SET_INVALID_I32, ERR_SET_INVALID_I32.size]
    end if
end proc

proc set_bint[bint:ptr, first:i32]
var len:i32, array:ptr
begin
    verify_i32[first]
    if first == 0l begin
        set len = 0l
    end else begin
        set len = 1l
    end if

    set array = (bint+BINT_PTR_OFFSET)@ptr
    set (array-ARRAY_NEG_OFFSET)@bool = first < 0l
    set (array)@i32 = abs_i32[first]
    set (bint+BINT_LEN_OFFSET)@i32 = len
end proc

proc set_abs_bint[bint:ptr, first:i32]
var len:i32, array:ptr
begin
    verify_digit[first]
    if first == 0l begin
        set len = 0l
    end else begin
        set len = 1l
    end if

    set array = (bint+BINT_PTR_OFFSET)@ptr
    set (array)@i32 = first
    set (bint+BINT_LEN_OFFSET)@i32 = len
end proc

proc is_neg[bint:ptr] bool
begin
    if is_zero[bint] begin
        return false
    end if
    return ((bint+BINT_PTR_OFFSET)@ptr-ARRAY_NEG_OFFSET)@bool
end proc

const INIT_ALLOC = 1

proc new_bint[first:i32] ptr
var obj:ptr, array:ptr, len:i32
begin
    verify_i32[first]
    set obj = alloc_b[BINT_HEADERSIZE, 0uss, 0us, BINT_BITMAP]
    set array = alloc_f[INIT_ALLOC * sizeof i32]
    if obj == NULL or array == NULL begin
        put_char['1']
        put_char[' ']
        put_int[INIT_ALLOC*sizeof i32]
        put_char[' ']
        oom[]
    end if

    if first == 0l begin
        set len = 0l
    end else begin
        set len = 1l
    end if

    set (array-ARRAY_NEG_OFFSET)@bool = first < 0l

    set (obj+BINT_CAP_OFFSET)@i32 = (INIT_ALLOC):i32
    set (obj+BINT_LEN_OFFSET)@i32 = len
    set (obj+BINT_PTR_OFFSET)@ptr = array

    set array@i32 = abs_i32[first]

    return obj
end proc

proc free_bint[bint:ptr]
begin
    free[(bint+BINT_PTR_OFFSET)@ptr]
    free[bint]
end proc

# pushes a digit at the end of the number,
# increasing length by 1
proc push_digit[bint:ptr, digit:i32]
var cap:i32, len:i32,
    array:ptr, new_array:ptr, new_cap
begin
    verify_digit[digit]
    set cap = (bint+BINT_CAP_OFFSET)@i32
    set len = (bint+BINT_LEN_OFFSET)@i32
    set array = (bint+BINT_PTR_OFFSET)@ptr

    if cap == len begin
        set new_cap = cap:i64*2
        set new_array = alloc_f[new_cap*sizeof i32]
        if new_array == NULL begin
            put_char['2']
            put_char[' ']
            put_int[new_cap*sizeof i32]
            put_char[' ']
            put_bint_header[bint]
            put_char[' ']
            oom[]
        end if

        copy_i32[new_array, array, len:i64]
        free[array]

        set (bint+BINT_CAP_OFFSET)@i32 = new_cap:i32
        set (bint+BINT_PTR_OFFSET)@ptr = new_array
        set array = new_array
    end if
    set (array+(len:i64*sizeof i32):ptr)@i32 = digit
    set (bint+BINT_LEN_OFFSET)@i32 = len + 1l
end proc

proc put_bint_header[bint:ptr]
var cap:i32, len:i32, array:ptr, neg:bool
begin
    set cap = (bint+BINT_CAP_OFFSET)@i32
    set len = (bint+BINT_LEN_OFFSET)@i32
    set array = (bint+BINT_PTR_OFFSET)@ptr
    set neg = (array+ARRAY_NEG_OFFSET)@bool

    put_char['{']
    put_char['l']
    put_char[':']
    put_int[len:i64]
    put_char[',']
    put_char['c']
    put_char[':']
    put_int[cap:i64]
    put_char[',']
    put_char['a']
    put_char[':']
    put_int[array:i64]
    put_char[',']
    put_char['n']
    put_char[':']
    put_bool[neg]
    put_char['}']
end proc

proc copy_bint[iDest:ptr, iSource:ptr] 
var source_cap:i32, source_len:i32, dest_cap:i32,
    dest_array:ptr, source_array:ptr
begin
    set source_cap = (iSource+BINT_CAP_OFFSET)@i32
    set source_len = (iSource+BINT_LEN_OFFSET)@i32
    set source_array = (iSource+BINT_PTR_OFFSET)@ptr

    set dest_cap = (iDest+BINT_CAP_OFFSET)@i32
    set dest_array = (iDest+BINT_PTR_OFFSET)@ptr

    if dest_cap < source_cap begin
        free[dest_array]
        set dest_array = alloc_f[source_cap:i64*sizeof i32]

        set (iDest+BINT_CAP_OFFSET)@i32 = source_cap
        set (iDest+BINT_PTR_OFFSET)@ptr = dest_array 
    end if

    copy_i32[dest_array, source_array, source_len:i64]
    set (dest_array-ARRAY_NEG_OFFSET)@bool = (source_array-ARRAY_NEG_OFFSET)@bool
    set (iDest+BINT_LEN_OFFSET)@i32 = source_len
end proc

proc copy_i32[dest:ptr, source:ptr, size]
var i
begin
    set i = 0
    while i < size begin
        set (dest+(i*sizeof i32):ptr)@i32 = (source+(i*sizeof i32):ptr)@i32
        set i += 1
    end while
end proc

proc remove_trailing_zeroes[bint:ptr]
var len, i, arraySlot:ptr, currDigit:ptr
begin
    set arraySlot = bint+BINT_PTR_OFFSET
    set len = (bint+BINT_LEN_OFFSET)@i32:i64
    set i = len-1
    set currDigit = arraySlot@ptr+(i*sizeof i32):ptr
    while currDigit@i32 == 0l and 0 <= i begin
        set len -= 1
        set i -= 1
        set currDigit = arraySlot@ptr+(i*sizeof i32):ptr
    end while

    set (bint+BINT_LEN_OFFSET)@i32 = len:i32
end proc

######################################## $ABSOLUTE ARITHMETIC

const MAXITER = 33

# abs_div computes the solution to A = Q*B + Rem
# (where A, B are given and Q, Rem are output pointers)
# by long division
#
# bintQ, bintRem must be different objects from bintA and bintB, although
# bintA may be the same as bintB
proc abs_div[
    iA:ptr,
    iB:ptr,
    iQ:ptr,
    iRem:ptr
]
var lenA, lenB, i, j, arrayA:ptr,
    low:i32, high:i32, guess:i32,
    res, iIDD:ptr, iScratch:ptr
begin
    if abs_comp_digit[iB, 0l] == 0 begin
        fatal[ERR_DIVISION_BY_ZERO, ERR_DIVISION_BY_ZERO.size]
    end if
    set_zero[iQ]
    set_zero[iRem]
    set iIDD = new_bint[0l] # TODO: remove these allocations
    set iScratch = new_bint[0l]

    set lenA = (iA+BINT_LEN_OFFSET)@i32:i64
    set arrayA = (iA+BINT_PTR_OFFSET)@ptr

    set i = lenA-1

    while 0 <= i begin
        abs_mult_base[iIDD]
        abs_add_digit[iIDD, (arrayA+(i*sizeof i32):ptr)@i32, iIDD]

        # find quotient digit
        if abs_comp[iIDD, iB] == MORE begin
            set low = 1l
            set guess = BASE/2l
            set high = BASE
            set j = 0
            set res = test_guess[iIDD, iScratch, iB, guess]
            while res != EQ and j < MAXITER begin
                if res == LESS begin
                    set low = guess
                end elseif res == MORE begin 
                    set high = guess
                end if
                set guess = (low + high)/2l
                set res = test_guess[iIDD, iScratch, iB, guess]
                set j += 1
            end while
            if j >= MAXITER begin
                exit 4ss
            end if

            copy_bint[iScratch, iB]
            abs_mult_digit[iScratch, guess, iScratch]
            abs_sub[iIDD, iScratch, iIDD]
            abs_mult_base[iQ]
            abs_add_digit[iQ, guess, iQ]
        end elseif abs_comp[iIDD, iB] == EQ begin
            set_zero[iIDD]
            abs_mult_base[iQ]
            abs_add_digit[iQ, 1l, iQ]
        end else begin
            if abs_comp_digit[iQ, 0l] != EQ begin
                abs_mult_base[iQ]
            end if
        end if
        set i -= 1
    end while
    copy_bint[iRem, iIDD]
    free_bint[iIDD] # TODO: consider how the GC will look at these frees
    free_bint[iScratch]
end proc

proc test_guess[
    iIDD:ptr,
    iScratch:ptr,
    iB:ptr,
    guess:i32
] i64
var res
begin
    copy_bint[iScratch, iB]
    abs_mult_digit[iScratch, guess, iScratch]
    set res = abs_comp[iScratch, iIDD]
    if (res == EQ) begin
        return EQ
    end if
    if res == LESS begin 
        abs_add[iScratch, iB, iScratch]
        set res = abs_comp[iScratch, iIDD]
        if res == MORE begin # guess*B < IDD and (guess+1)*B > IDD
            return EQ
        end if
        return LESS # too low
    end if
    if res == MORE begin
        return MORE
    end if
    exit 99ss
end proc

# since 0 <= rem < B
# we can store rem in a i32
# and return it from the procedure
#
# furthermore,
# IDD will have at most 2 digits, because B has only 1
# so we can store it in an i64
proc abs_div_digit[
    iA:ptr,
    b:i32,
    iQ:ptr
] i32
var idd, q, b64,
    lenA, arrayA:ptr, i
begin
    if b == 0l begin
        fatal[ERR_DIVISION_BY_ZERO, ERR_DIVISION_BY_ZERO.size]
    end if
    set_zero[iQ]

    set lenA = (iA+BINT_LEN_OFFSET)@i32:i64
    set arrayA = (iA+BINT_PTR_OFFSET)@ptr
    set b64 = b:i64

    set idd = 0
    set i = lenA-1
    while 0 <= i begin
        set idd *= BASE:i64
        set idd += (arrayA+(i*sizeof i32):ptr)@i32:i64

        set q = idd / b64
        set idd -= q*b64

        abs_mult_base[iQ]
        abs_add_digit[iQ, q:i32, iQ]
        
        set i -= 1
    end while
    # at the end of the loop, idd will be less than BASE,
    # so we can safely cast it
    return idd:i32
end proc

# abs_slow_div computes the solution to A = Q*B + Rem
# (where A, B are given and Q, Rem are output pointers)
# by repeated abs_subtraction
#
# bintQ, bintRem must be different objects from bintA and bintB, although
# bintA may be the same as bintB
proc abs_slow_div[
    iA:ptr,
    iB:ptr,
    iQ:ptr,
    iRem:ptr
]
var res
begin
    set_zero[iQ]
    set_zero[iRem]
    if abs_comp_digit[iB, 0l] == 0 begin
        fatal[ERR_DIVISION_BY_ZERO, ERR_DIVISION_BY_ZERO.size]
    end if
    copy_bint[iRem, iA]
    set res = abs_comp[iRem, iB]
    while res == MORE or res == EQ begin
        abs_sub[iRem, iB, iRem]
        abs_add_digit[iQ, 1l, iQ]
        set res = abs_comp[iRem, iB]
    end while
end proc

# we will have to mark the pointers that point to objetcs
# so the GC can ignore derived pointers.
#
# proc abs_mult[natA*:ptr, natB*:ptr, natC*:ptr]
# var i, j, sizeA, sizeB, sizeC, carry:i32, res,
#     currdigitA:ptr, currdigitB:ptr, currdigitC:ptr,
#     sizePtr:ptr
# begin

# iC must be a different object from iA and iB
proc abs_mult[iA:ptr, iB:ptr, iC:ptr]
var i, j,
    lenA, lenB, lenC,
    carry:i32, res,
    arrayA:ptr, arrayB:ptr, arraySlotC:ptr,
    currDigitA:ptr, currDigitB:ptr, currDigitC:ptr,
begin
    set_zero[iC]
    set lenA = (iA+BINT_LEN_OFFSET)@i32:i64
    set lenB = (iB+BINT_LEN_OFFSET)@i32:i64
    set lenC = (iC+BINT_LEN_OFFSET)@i32:i64

    set arrayA = (iA+BINT_PTR_OFFSET)@ptr
    set arrayB = (iB+BINT_PTR_OFFSET)@ptr

    # we might realloc the array
    set arraySlotC = iC+BINT_PTR_OFFSET

    set i = 0
    while i < lenA begin
        set currDigitA = arrayA+(i*sizeof i32):ptr
        set j = 0
        set carry = 0l
        while j < lenB or carry > 0l begin
            if i+j == lenC begin
                push_digit[iC, 0l]
                set lenC += 1
            end if
            set currDigitB = arrayB+(j*sizeof i32):ptr
            set currDigitC = arraySlotC@ptr+((i+j)*sizeof i32):ptr
            if j < lenB begin
                set res = currDigitC@i32:i64 + carry:i64 + currDigitA@i32:i64*currDigitB@i32:i64
            end else begin
                set res = currDigitC@i32:i64 + carry:i64
            end if

            set currDigitC@i32 = (res % BASE:i64):i32
            set carry = (res / BASE:i64):i32
            
            set j += 1
        end while
        set i += 1
    end while

    remove_trailing_zeroes[iC]
end proc

proc abs_mult_base[bint:ptr]
var len, i, array:ptr
begin
    push_digit[bint, 0l]
    set len = (bint+BINT_LEN_OFFSET)@i32:i64
    set array = (bint+BINT_PTR_OFFSET)@ptr
    set i = len-1
    while 0 < i begin
        set (array+(i*sizeof i32):ptr)@i32 = (array+((i-1)*sizeof i32):ptr)@i32
        set i -= 1
    end while
    set array@i32 = 0l # zeroes the first one
end proc

# iA may be the same object as iC
proc abs_mult_digit[iA:ptr, digit:i32, iC:ptr]
var i,
    carry:i32, res,
    lenA, lenC,
    arraySlotA:ptr, arraySlotC:ptr,
    currDigitA:ptr, currDigitC:ptr
begin
    verify_digit[digit]

    set lenA = (iA+BINT_LEN_OFFSET)@i32:i64
    # lenC must be smaller or equal to lenA
    # if iC == iA then lenC == lenA too
    set lenC = min[(iC+BINT_LEN_OFFSET)@i32:i64, lenA]
    set (iC+BINT_LEN_OFFSET)@i32 = lenC:i32

    set arraySlotA = iA+BINT_PTR_OFFSET
    set arraySlotC = iC+BINT_PTR_OFFSET

    set i = 0
    set carry = 0l
    while i < lenA or carry > 0l begin
        if i == lenC begin
            push_digit[iC, 0l]
            set lenC += 1
        end if
        set currDigitA = arraySlotA@ptr+(i*sizeof i32):ptr
        set currDigitC = arraySlotC@ptr+(i*sizeof i32):ptr

        set res = carry:i64
        if i < lenA begin
            set res += currDigitA@i32:i64 * digit:i64
        end if
        set currDigitC@i32 = (res % BASE:i64):i32
        set carry = (res / BASE:i64):i32
       
        set i += 1
    end while

    remove_trailing_zeroes[iC]
end proc

# iA, iB and iC need not to be different objects
proc abs_sub[iA:ptr, iB:ptr, iC:ptr]
var i, lenA, lenB, lenC,
    carry:i32, res:i32,
    arraySlotA:ptr, arraySlotB:ptr, arraySlotC:ptr,
    currDigitA:ptr, currDigitB:ptr, currDigitC:ptr,
    hold:ptr
begin
    set i = abs_comp[iA, iB]
    if i == LESS begin
        set hold = iA
        set iA = iB
        set iB = hold
    end elseif i == EQ begin
        set_zero[iC]
        return;
    end if
    # for now on, we know that lenB <= lenA

    # if iB and iC are the same object set to zero
    # then iC will not be set to iA, so we do a separate check
    if abs_comp_digit[iB, 0l] == EQ begin
        copy_bint[iC, iA]
        return
    end if

    set lenA = (iA+BINT_LEN_OFFSET)@i32:i64
    set lenB = (iB+BINT_LEN_OFFSET)@i32:i64
    set lenC = (iC+BINT_LEN_OFFSET)@i32:i64

    set arraySlotA = iA+BINT_PTR_OFFSET
    set arraySlotB = iB+BINT_PTR_OFFSET
    set arraySlotC = iC+BINT_PTR_OFFSET

    set carry = 0l
    set i = 0
    while i < lenB or carry > 0l begin
        if i == lenC begin
            push_digit[iC, 0l]
            set lenC += 1
        end if
        set currDigitA = arraySlotA@ptr+(i*sizeof i32):ptr
        set currDigitB = arraySlotB@ptr+(i*sizeof i32):ptr
        set currDigitC = arraySlotC@ptr+(i*sizeof i32):ptr

        set res = ~carry
        if i < lenA begin
            set res += currDigitA@i32
        end if
        if i < lenB begin
            set res -= currDigitB@i32
        end if
        set currDigitC@i32 = res

        if currDigitC@i32 < 0l begin
            set carry = 1l
            set currDigitC@i32 += BASE
        end else begin
            set carry = 0l
        end if

        set i += 1
    end while
    
    remove_trailing_zeroes[iC]
end proc

# iA and iC need not to be different objects
proc abs_sub_digit[iA:ptr, digit:i32, iC:ptr]
var i, lenA, lenC,
    carry:i32, res:i32,
    arraySlotA:ptr, arraySlotC:ptr,
    currDigitA:ptr, currDigitC:ptr,
    a_digit:i32
begin
    set i = abs_comp_digit[iA, digit]

    if i == LESS begin
        # in this case, lenA <= 1
        set a_digit = ((iA+BINT_PTR_OFFSET)@ptr)@i32
        # and we do sub_digit[iC, iA] -> iC instead
        set_abs_bint[iC, digit-a_digit]
        return;
    end elseif i == EQ begin
        set_zero[iC]
        return;
    end if
    # for now on, we know that 1 <= lenA

    set lenA = (iA+BINT_LEN_OFFSET)@i32:i64
    set lenC = (iC+BINT_LEN_OFFSET)@i32:i64

    set arraySlotA = iA+BINT_PTR_OFFSET
    set arraySlotC = iC+BINT_PTR_OFFSET

    set carry = 0l
    set i = 0
    if i == lenC begin
        push_digit[iC, 0l]
        set lenC += 1
    end if

    set currDigitA = arraySlotA@ptr+(i*sizeof i32):ptr
    set currDigitC = arraySlotC@ptr+(i*sizeof i32):ptr

    set currDigitC@i32 = currDigitA@i32 - digit

    if currDigitC@i32 < 0l begin
        set carry = 1l
        set currDigitC@i32 += BASE
    end if
    set i += 1

    while carry > 0l begin
        if i == lenC begin
            push_digit[iC, 0l]
            set lenC += 1
        end if
        set currDigitA = arraySlotA@ptr+(i*sizeof i32):ptr
        set currDigitC = arraySlotC@ptr+(i*sizeof i32):ptr

        if i < lenA begin
            set res = currDigitA@i32
        end else begin
            set res = 0l
        end if
        set currDigitC@i32 = res - carry

        if currDigitC@i32 < 0l begin
            set carry = 1l
            set currDigitC@i32 += BASE
        end else begin
            set carry = 0l
        end if

        set i += 1
    end while
    
    remove_trailing_zeroes[iC]
end proc

proc is_zero[bint:ptr] bool
begin
    return (bint+BINT_LEN_OFFSET)@i32 <= 0l
end proc

proc set_zero[bint:ptr]
var array:ptr
begin
    set array = (bint+BINT_PTR_OFFSET)@ptr

    set (bint+BINT_LEN_OFFSET)@i32 = 0l
    set (array)@i32 = 0l
    set (array-ARRAY_NEG_OFFSET)@bool = false
end proc

const EQ = 0
const MORE = 1
const LESS = 2

# iA and iB need not to be different objects
proc abs_comp[iA:ptr, iB:ptr] i64
var lenA, lenB,
    arrayA:ptr, arrayB:ptr,
    i, currDigitA:ptr, currDigitB:ptr
begin
    set lenA = (iA+BINT_LEN_OFFSET)@i32:i64
    set lenB = (iB+BINT_LEN_OFFSET)@i32:i64

    if lenB < lenA begin
        return MORE
    end elseif lenA < lenB begin
        return LESS
    end if

    # we won't update the size of the array, thus we can keep
    # a pointer directly to the vector
    set arrayA = (iA+BINT_PTR_OFFSET)@ptr
    set arrayB = (iB+BINT_PTR_OFFSET)@ptr

    set i = lenA-1 # we go backwards (MSD -> LSD)
    while 0 <= i begin
        set currDigitA = arrayA+(i*sizeof i32):ptr
        set currDigitB = arrayB+(i*sizeof i32):ptr
        
        if currDigitB@i32 < currDigitA@i32 begin
            return MORE
        end elseif currDigitA@i32 < currDigitB@i32 begin
            return LESS
        end if
        
        set i -= 1
    end while

    return EQ
end proc

# abs_compares two numbers, just like abs_comp,
# with the same outputs,
# but the second number is < BASE,
# that is, it's a single digit number.
proc abs_comp_digit[iA:ptr, digit:i32] i64
var lenA, i, digitA:i32
begin
    verify_digit[digit]
    set lenA = (iA+BINT_LEN_OFFSET)@i32:i64

    if digit == 0l and lenA == 0 begin
        return EQ # both are zero
    end if

    if lenA > 1 begin
        return MORE
    end if

    set digitA = ((iA+BINT_PTR_OFFSET)@ptr)@i32

    if digit < digitA begin
        return MORE
    end elseif digitA < digit begin
        return LESS
    end if
    return EQ
end proc

# abs_adds iA to iB and stores in iC
#
# iA, iB and iC need not to be different objects
proc abs_add[iA:ptr, iB:ptr, iC:ptr]
var lenA, lenB, lenC, maxLen,
    arraySlotA:ptr, arraySlotB:ptr, arraySlotC:ptr,
    carry:i32, i, currDigitC:ptr,
    res:i32
begin
    set lenA = (iA+BINT_LEN_OFFSET)@i32:i64
    set lenB = (iB+BINT_LEN_OFFSET)@i32:i64
    set lenC = (iC+BINT_LEN_OFFSET)@i32:i64
    set arraySlotA = iA+BINT_PTR_OFFSET
    set arraySlotB = iB+BINT_PTR_OFFSET
    set arraySlotC = iC+BINT_PTR_OFFSET
    set maxLen = max[lenA, lenB]

    set i = 0
    set carry = 0l
    while i < maxLen or carry > 0l begin
        if i == lenC begin
            push_digit[iC, 0l]
            set lenC += 1
        end if
        set currDigitC = arraySlotC@ptr+(i*sizeof i32):ptr
        set res = carry
        if i < lenA begin
            set res += (arraySlotA@ptr+(i*sizeof i32):ptr)@i32
        end if
        if i < lenB begin
            set res += (arraySlotB@ptr+(i*sizeof i32):ptr)@i32
        end if
        set currDigitC@i32 = res
        if BASE <= currDigitC@i32 begin
            set carry = 1l
            set currDigitC@i32 -= BASE
        end else begin
            set carry = 0l
        end if
        set i += 1
    end while
end proc

# abs_adds two numbers, just like abs_add
# but the second number is < BASE,
# that is, it's a single digit number.
#
# iA and iC need not to be different objects
proc abs_add_digit[iA:ptr, digit:i32, iC:ptr]
var lenA, lenC,
    arraySlotA:ptr, arraySlotC:ptr,
    currDigitC:ptr,
    carry:i32, i, res:i32
begin
    verify_digit[digit]
    set lenA = (iA+BINT_LEN_OFFSET)@i32:i64
    set lenC = (iC+BINT_LEN_OFFSET)@i32:i64
    set arraySlotA = iA+BINT_PTR_OFFSET
    set arraySlotC = iC+BINT_PTR_OFFSET

    set i = 0
    if i == lenC begin
        push_digit[iC, 0l]
        set lenC += 1
    end if
    set currDigitC = arraySlotC@ptr+(i*sizeof i32):ptr

    set carry = 0l
    set res = digit
    if i < lenA begin
         set res += (arraySlotA@ptr + (i*sizeof i32):ptr)@i32
    end if
    set currDigitC@i32 = res

    if BASE <= currDigitC@i32 begin
        set carry = 1l
        set currDigitC@i32 -= BASE
    end if
    set i += 1
    while carry > 0l begin
        if i == lenC begin
            # push_digit may alter the pointer at arraySlot
            push_digit[iC, 0l]
            set lenC += 1
        end if
        set currDigitC = arraySlotC@ptr+(i*sizeof i32):ptr
        set res = carry
        if i < lenA begin
            set res += (arraySlotA@ptr + (i*sizeof i32):ptr)@i32
        end if
        set currDigitC@i32 = res
        if BASE <= currDigitC@i32 begin
            set carry = 1l
            set currDigitC@i32 -= BASE
        end else begin
            set carry = 0l
        end if
        set i += 1
    end while
end proc
######################################## $INTEGER ARITHMETIC

proc comp[iA:ptr, iB:ptr] i64
var negA:bool, negB:bool, res
begin
    set negA = is_neg[iA]
    set negB = is_neg[iB]
    if not negA and not negB begin
        return abs_comp[iA, iB]
    end elseif negA and negB begin
        set res = abs_comp[iA, iB]
        # invert the result
        if res == MORE begin
            return LESS
        end elseif res == LESS begin
            return MORE
        end else begin
            return EQ
        end if
    end elseif not negA and negB begin
        return MORE
    end elseif negA and not negB begin
        return LESS
    end if
    fatal[WTF, WTF.size]
    return ~1
end proc

proc comp_i32[iA:ptr, b:i32] i64
var negA:bool, negB:bool, res
begin
    set negA = is_neg[iA]
    set negB = b < 0l
    set b = abs_i32[b]

    if not negA and not negB begin
        return abs_comp_digit[iA, b]
    end elseif negA and negB begin
        set res = abs_comp_digit[iA, b]
        # invert the result
        if res == MORE begin
            return LESS
        end elseif res == LESS begin
            return MORE
        end else begin
            return EQ
        end if
    end elseif not negA and negB begin
        return MORE
    end elseif negA and not negB begin
        return LESS
    end if
    fatal[WTF, WTF.size]
    return ~1
end proc

proc set_neg[bint:ptr, neg:bool]
var array:ptr
begin
    set array = (bint+BINT_PTR_OFFSET)@ptr
    set (array-ARRAY_NEG_OFFSET)@bool = neg
end proc

proc neg[bint:ptr]
var array:ptr
begin
    set array = (bint+BINT_PTR_OFFSET)@ptr
    set (array-ARRAY_NEG_OFFSET)@bool = not (array-ARRAY_NEG_OFFSET)@bool
end proc

proc sub[iA:ptr, iB:ptr, iC:ptr]
var negA:bool, negB:bool, res:bool
begin
    set negA = is_neg[iA]
    set negB = is_neg[iB]
    if not negA and not negB begin
    	# (1 * a) - (1 * b) = a-b
        set res = pos_sub_sign[iA, iB, iC]
        set_neg[iC, res]
    end elseif negA and negB begin
    	# (-1 * a) - (-1 * b) = (-1)*(a-b)
        set res = pos_sub_sign[iA, iB, iC]
        set_neg[iC, not res]
    end elseif negA and not negB begin
	    # (-1 * a) - (1 * b)  = (-1)*((1*b) - (-1*a)) = (-1)*(b+a)
        abs_add[iA, iB, iC]
        set_neg[iC, true]
    end elseif not negA and negB begin
    	# (1 * a) - (-1 * b)  = a+b
        abs_add[iA, iB, iC]
        set_neg[iC, false]
    end if
end proc

proc sub_i32[iA:ptr, b:i32, iC:ptr]
var negA:bool, negB:bool, res:bool
begin
    set negA = is_neg[iA]
    set negB = b < 0l
    set b = abs_i32[b]

    if not negA and not negB begin
    	# (1 * a) - (1 * b) = a-b
        set res = pos_sub_digit_sign[iA, b, iC]
        set_neg[iC, res]
    end elseif negA and negB begin
    	# (-1 * a) - (-1 * b) = (-1)*(a-b)
        set res = pos_sub_digit_sign[iA, b, iC]
        set_neg[iC, not res]
    end elseif negA and not negB begin
	    # (-1 * a) - (1 * b)  = (-1)*((1*b) - (-1*a)) = (-1)*(b+a)
        abs_add_digit[iA, b, iC]
        set_neg[iC, true]
    end elseif not negA and negB begin
    	# (1 * a) - (-1 * b)  = a+b
        abs_add_digit[iA, b, iC]
        set_neg[iC, false]
    end if
end proc

proc add[iA:ptr, iB:ptr, iC:ptr]
var negA:bool, negB:bool, res:bool
begin
    set negA = is_neg[iA]
    set negB = is_neg[iB]

    if not negA and not negB begin
        # (1 * a) + (1 * b)   = 1*(a+b)
        abs_add[iA, iB, iC]
        set_neg[iC, false]
    end elseif negA and negB begin
    	# (-1 * a) + (-1 * b) = -1*(a+b)
        abs_add[iA, iB, iC]
        set_neg[iC, true]
    end elseif negA and not negB begin
    	# (-1 * a) + (1 * b)  = b-a
        set res = pos_sub_sign[iB, iA, iC]
        set_neg[iC, res]
    end elseif not negA and negB begin
    	# (1 * a) + (-1 * b)  = a-b
        set res = pos_sub_sign[iA, iB, iC]
        set_neg[iC, res]
    end if
end proc

proc add_i32[iA:ptr, b:i32, iC:ptr]
var negA:bool, negB:bool, res:bool
begin
    set negA = is_neg[iA]
    set negB = b < 0l
    set b = abs_i32[b]

    if not negA and not negB begin
        # (1 * a) + (1 * b)   = a+b
        abs_add_digit[iA, b, iC]
        set_neg[iC, false]
    end elseif negA and negB begin
    	# (-1 * a) + (-1 * b) = (-1)*(a+b)
        abs_add_digit[iA, b, iC]
        set_neg[iC, true]
    end elseif negA and not negB begin
    	# (-1 * a) + (1 * b)  = b-a = (-1)*(a-b)
        set res = pos_sub_digit_sign[iA, b, iC]
        set_neg[iC, not res]
    end elseif not negA and negB begin
    	# (1 * a) + (-1 * b)  = a-b
        set res = pos_sub_digit_sign[iA, b, iC]
        set_neg[iC, res]
    end if
end proc

proc mult[iA:ptr, iB:ptr, iC:ptr]
var neg:bool, negA:bool, negB:bool
begin
    set negA = is_neg[iA]
    set negB = is_neg[iB]
    set neg = mult_signal[negA, negB]
    abs_mult[iA, iB, iC]
    set_neg[iC, neg]
end proc

proc mult_i32[iA:ptr, b:i32, iC:ptr]
var neg:bool, negA:bool, negB:bool
begin
    set negA = is_neg[iA]
    set negB = b < 0l
    set neg = mult_signal[negA, negB]
    set b = abs_i32[b]
    abs_mult_digit[iA, b, iC]
    set_neg[iC, neg]
end proc

proc div[iA:ptr, iB:ptr, iQ:ptr, iRem:ptr]
var neg:bool, negA:bool, negB:bool
begin
    set negA = is_neg[iA]
    set negB = is_neg[iB]
    set neg = mult_signal[negA, negB]
    abs_div[iA, iB, iQ, iRem]
    if abs_comp_digit[iQ, 0l] != EQ begin
        set_neg[iQ, neg]
    end if
    if abs_comp_digit[iRem, 0l] != EQ begin
        set_neg[iRem, negA]
    end if
end proc

proc div_i32[iA:ptr, b:i32, iQ:ptr] i32
var neg:bool, negA:bool, negB:bool, rem:i32
begin
    set negA = is_neg[iA]
    set negB = b < 0l
    set neg = mult_signal[negA, negB]
    set b = abs_i32[b]

    set rem = abs_div_digit[iA, b, iQ]
    if abs_comp_digit[iQ, 0l] != EQ begin
        set_neg[iQ, neg]
    end if

    if rem != 0l and negA != (rem < 0l) begin
        set rem = ~rem
    end if
    return rem
end proc

proc mult_signal[a:bool, b:bool] bool
begin
    # 1*1 = (-1)*(-1) = 1
    if (not a and not b) or (a and b) begin
        return false
    # (-1)*1 = 1*(-1) = -1
    end elseif (not a and b) or (a and not b) begin
        return true
    end if
    fatal[WTF, WTF.size]
    return false
end proc

# subtracts two positive numbers
# then returns a sign based on magnitude
proc pos_sub_sign[iA:ptr, iB:ptr, iC:ptr] bool
var res
begin
    set res = abs_comp[iA, iB]
    abs_sub[iA, iB, iC]
    if res == MORE begin
        # if a > b then a-b > 0
        return false
    end elseif res == LESS begin
        # if a < b then a-b < 0
        return true
    end if
    return false
end proc

# same as above, but second number may be an i32
proc pos_sub_digit_sign[iA:ptr, b:i32, iC:ptr] bool
var res
begin
    set res = abs_comp_digit[iA, b]
    abs_sub_digit[iA, b, iC]
    if res == MORE begin
        return false
    end elseif res == LESS begin
        return true
    end if
    return false
end proc
