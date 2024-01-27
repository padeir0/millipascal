# Long Arithmetic for natural numbers (integers >= 0)
# Aritmética de tamanho "arbitrário" para números naturais (inteiros >= 0)
from constants import NULL, WORD
from buffutil import set_buff, copy
from ioutil import fatal, put_int, put_char
from intutil import max
from dfl_alloc
import alloc_f, alloc_b, free, init, print_list, print_map,
       validate_init

export
    BASE, EQ, MORE, LESS,
    div, slow_div, # div_digit, div_base, #TODO
    comp, comp_digit,
    mult, mult_digit, mult_base,
    add, add_digit,
    sub, # sub_digit, #TODO
    put_bnat,
    new_bnat, set_zero,
    less, less_eq, more, more_eq, eq, neq,
    put_res

memory ERR_OOM "OUT OF MEMORY\n"
memory ERR_NAT_NEGATIVE "number subtraction went negative\n"
memory ERR_DIVISION_BY_ZERO "division by zero\n"
memory WTF "wtf\n"

const BASE 1_000_000_000l # 10^9, 9 digits for each i32
const DIGITS_PER_INT 9

# BigNat Header (positive offset)
#  ├── 4 bytes, offset 0: capacity
#  ├── 4 bytes, offset 4: length
#  └── 8 bytes, offset 8: pointer to array (on front heap)

const BNAT_HEADERSIZE 16

const BNAT_CAP_OFFSET 0p
const BNAT_LEN_OFFSET 4p
const BNAT_PTR_OFFSET 8p

# pointer in second word
const BNAT_BITMAP 0b01000000_00000000_00000000_00000000ul
const INIT_ALLOC 2

proc set_bnat[nat:ptr, first:i32]
var len:i32
begin
    if first == 0l begin
        set len = 0l
    end else begin
        set len = 1l
    end if

    set ((nat+BNAT_PTR_OFFSET)@ptr)@i32 = first
    set (nat+BNAT_LEN_OFFSET)@i32 = len
end proc

proc new_bnat[first:i32] ptr
var obj:ptr, array:ptr, len:i32
begin
    set obj = alloc_b[BNAT_HEADERSIZE, 0uss, 0us, BNAT_BITMAP]
    set array = alloc_f[INIT_ALLOC * sizeof i32]
    if obj == NULL or array == NULL begin
        put_char['1']
        put_char[' ']
        put_int[INIT_ALLOC*sizeof i32]
        put_char[' ']
        fatal[ERR_OOM, ERR_OOM.size]
    end if

    if first == 0l begin
        set len = 0l
    end else begin
        set len = 1l
    end if

    set (obj+BNAT_CAP_OFFSET)@i32 = (INIT_ALLOC):i32
    set (obj+BNAT_LEN_OFFSET)@i32 = len
    set (obj+BNAT_PTR_OFFSET)@ptr = array

    set array@i32 = first

    return obj
end proc

proc free_bnat[nat:ptr]
var array:ptr
begin
    set array = (nat+BNAT_PTR_OFFSET)@ptr
    free[array]
    free[nat]
end proc

# pushes a digit at the end of the number,
# increasing length by 1
proc push_bnat[nat:ptr, digit:i32]
var cap:i32, len:i32,
    array:ptr, new_array:ptr, new_cap
begin
    set cap = (nat+BNAT_CAP_OFFSET)@i32
    set len = (nat+BNAT_LEN_OFFSET)@i32
    set array = (nat+BNAT_PTR_OFFSET)@ptr

    if cap == len begin
        set new_cap = cap:i64*2
        set new_array = alloc_f[new_cap*sizeof i32]
        if new_array == NULL begin
            put_char['2']
            put_char[' ']
            put_int[new_cap*sizeof i32]
            put_char[' ']
            fatal[ERR_OOM, ERR_OOM.size]
        end if

        copy_i32[new_array, array, len:i64]
        free[array]

        set (nat+BNAT_CAP_OFFSET)@i32 = new_cap:i32
        set (nat+BNAT_PTR_OFFSET)@ptr = new_array
        set array = new_array
    end if
    set (array+(len:i64*sizeof i32):ptr)@i32 = digit
    set (nat+BNAT_LEN_OFFSET)@i32 = len + 1l
end proc

proc put_bnat_header[nat:ptr]
var cap:i32, len:i32, array:ptr
begin
    set cap = (nat+BNAT_CAP_OFFSET)@i32
    set len = (nat+BNAT_LEN_OFFSET)@i32
    set array = (nat+BNAT_PTR_OFFSET)@ptr

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
    put_char['}']
end proc

proc copy_bnat[natDest:ptr, natSource:ptr] 
var source_cap:i32, source_len:i32, dest_cap:i32,
    dest_array:ptr, source_array:ptr
begin
    set source_cap = (natSource+BNAT_CAP_OFFSET)@i32
    set source_len = (natSource+BNAT_LEN_OFFSET)@i32
    set source_array = (natSource+BNAT_PTR_OFFSET)@ptr

    set dest_cap = (natDest+BNAT_CAP_OFFSET)@i32
    set dest_array = (natDest+BNAT_PTR_OFFSET)@ptr

    if dest_cap < source_cap begin
        free[dest_array]
        set dest_array = alloc_f[source_cap:i64*sizeof i32]

        set (natDest+BNAT_CAP_OFFSET)@i32 = source_cap
        set (natDest+BNAT_PTR_OFFSET)@ptr = dest_array 
    end if

    copy_i32[dest_array, source_array, source_len:i64]
    set (natDest+BNAT_LEN_OFFSET)@i32 = source_len
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
var lenA, lenB, i, j, arrayA:ptr,
    low:i32, high:i32, guess:i32,
    res, natIDD:ptr, scratch:ptr
begin
    if comp_digit[natB, 0l] == 0 begin
        fatal[ERR_DIVISION_BY_ZERO, ERR_DIVISION_BY_ZERO.size]
    end if
    set_zero[natQ]
    set_zero[natRem]
    set natIDD = new_bnat[0l] # TODO: remove these allocations
    set scratch = new_bnat[0l]

    set lenA = (natA+BNAT_LEN_OFFSET)@i32:i64
    set arrayA = (natA+BNAT_PTR_OFFSET)@ptr

    set i = lenA-1

    while 0 <= i begin
        mult_base[natIDD]
        add_digit[natIDD, (arrayA+(i*sizeof i32):ptr)@i32]

        # find quotient digit
        if more[natIDD, natB] begin
            set low = 1l
            set guess = BASE/2l
            set high = BASE
            set j = 0
            set res = test_guess[natIDD, scratch, natB, guess]
            while res != EQ and j < MAXITER begin
                if res == LESS begin
                    set low = guess
                end elseif res == MORE begin 
                    set high = guess
                end if
                set guess = (low + high)/2l
                set res = test_guess[natIDD, scratch, natB, guess]
                set j += 1
            end while
            if j >= MAXITER begin
                exit 4ss
            end if

            copy_bnat[scratch, natB]
            mult_digit[scratch, guess]
            sub[natIDD, scratch]
            mult_base[natQ]
            add_digit[natQ, guess]
        end elseif eq[natIDD, natB] begin
            set_zero[natIDD]
            mult_base[natQ]
            add_digit[natQ, 1l]
        end else begin
            if comp_digit[natQ, 0l] != EQ begin
                mult_base[natQ]
            end if
        end if
        set i -= 1
    end while
    copy_bnat[natRem, natIDD]
    free_bnat[natIDD]
    free_bnat[scratch]
end proc

proc test_guess[
    natIDD:ptr,
    scratch:ptr,
    natB:ptr,
    guess:i32
] i64
var res
begin
    copy_bnat[scratch, natB]
    mult_digit[scratch, guess]
    set res = comp[scratch, natIDD]
    if (res == EQ) begin
        return EQ
    end if
    if res == LESS begin 
        add[scratch, natB]
        set res = comp[scratch, natIDD]
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
    set_zero[natQ]
    set_zero[natRem]
    if comp_digit[natB, 0l] == 0 begin
        fatal[ERR_DIVISION_BY_ZERO, ERR_DIVISION_BY_ZERO.size]
    end if
    copy_bnat[natRem, natA]
    while more_eq[natRem, natB] begin
        sub[natRem, natB]
        add_digit[natQ, 1l]
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
var i, j,
    lenA, lenB, lenC,
    carry:i32, res,
    arrayA:ptr, arrayB:ptr, arraySlotC:ptr,
    currDigitA:ptr, currDigitB:ptr, currDigitC:ptr,
begin
    set_zero[natC]
    set lenA = (natA+BNAT_LEN_OFFSET)@i32:i64
    set lenB = (natB+BNAT_LEN_OFFSET)@i32:i64
    set lenC = (natC+BNAT_LEN_OFFSET)@i32:i64

    set arrayA = (natA+BNAT_PTR_OFFSET)@ptr
    set arrayB = (natB+BNAT_PTR_OFFSET)@ptr

    # we might realloc the array
    set arraySlotC = natC+BNAT_PTR_OFFSET

    set i = 0
    while i < lenA begin
        set currDigitA = arrayA+(i*sizeof i32):ptr
        set j = 0
        set carry = 0l
        while j < lenB or carry > 0l begin
            if i+j == lenC begin
                push_bnat[natC, 0l]
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

    set i = lenC-1
    set currDigitC = arraySlotC@ptr+(i*sizeof i32):ptr
    while currDigitC@i32 == 0l and 0 <= i begin
        set lenC -= 1
        set i -= 1
        set currDigitC = arraySlotC@ptr+(i*sizeof i32):ptr
    end while

    set (natC+BNAT_LEN_OFFSET)@i32 = lenC:i32
end proc

proc mult_base[nat:ptr]
var len, i, array:ptr
begin
    push_bnat[nat, 0l]
    set len = (nat+BNAT_LEN_OFFSET)@i32:i64
    set array = (nat+BNAT_PTR_OFFSET)@ptr
    set i = len-1
    while 0 < i begin
        set (array+(i*sizeof i32):ptr)@i32 = (array+((i-1)*sizeof i32):ptr)@i32
        set i -= 1
    end while
    set array@i32 = 0l # zeroes the first one
end proc

proc mult_digit[nat:ptr, digit:i32]
var i, len, arraySlot:ptr,
    carry:i32, res,
    currDigit:ptr
begin
    set len = (nat+BNAT_LEN_OFFSET)@i32:i64
    set arraySlot = nat+BNAT_PTR_OFFSET

    set i = 0
    set carry = 0l
    while i < len or carry > 0l begin
        if i == len begin
            push_bnat[nat, 0l]
            set len += 1
        end if
        set currDigit = arraySlot@ptr+(i*sizeof i32):ptr
        set res = carry:i64 + currDigit@i32:i64 * digit:i64
        set currDigit@i32 = (res % BASE:i64):i32
        set carry = (res / BASE:i64):i32
       
        set i += 1
    end while
end proc

proc sub[natA:ptr, natB:ptr]
var i, lenA, lenB,
    carry:i32,
    arraySlotA:ptr, arraySlotB:ptr,
    currDigitA:ptr, currDigitB:ptr
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
    set lenA = (natA+BNAT_LEN_OFFSET)@i32:i64
    set lenB = (natB+BNAT_LEN_OFFSET)@i32:i64

    set arraySlotA = natA+BNAT_PTR_OFFSET
    set arraySlotB = natB+BNAT_PTR_OFFSET

    set carry = 0l
    set i = 0
    while i < lenB or carry > 0l begin
        set currDigitA = arraySlotA@ptr+(i*sizeof i32):ptr
        set currDigitB = arraySlotB@ptr+(i*sizeof i32):ptr

        if i < lenB begin
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
    
    # remove trailing zeroes
    set i = lenA-1
    set currDigitA = arraySlotA@ptr+(i*sizeof i32):ptr
    while currDigitA@i32 == 0l and 0 <= i begin
        set lenA -= 1
        set i -= 1
        set currDigitA = arraySlotA@ptr+(i*sizeof i32):ptr
    end while
    set (natA+BNAT_LEN_OFFSET)@i32 = lenA:i32
end proc

proc set_zero[nat:ptr]
begin
    set (nat+BNAT_LEN_OFFSET)@i32 = 0l
    set ((nat+BNAT_PTR_OFFSET)@ptr)@i32 = 0l
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
var lenA, lenB,
    arrayA:ptr, arrayB:ptr,
    i, currDigitA:ptr, currDigitB:ptr
begin
    set lenA = (natA+BNAT_LEN_OFFSET)@i32:i64
    set lenB = (natB+BNAT_LEN_OFFSET)@i32:i64

    if lenB < lenA begin
        return MORE
    end elseif lenA < lenB begin
        return LESS
    end if

    # we won't update the size of the array, thus we can keep
    # a pointer directly to the vector
    set arrayA = (natA+BNAT_PTR_OFFSET)@ptr
    set arrayB = (natB+BNAT_PTR_OFFSET)@ptr

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

# Compares two numbers, just like comp,
# with the same outputs,
# but the second number is < BASE,
# that is, it's a single digit number.
proc comp_digit[natA:ptr, digit:i32] i64
var lenA, i, digitA:i32
begin
    set lenA = (natA+BNAT_LEN_OFFSET)@i32:i64

    if digit == 0l and lenA == 0 begin
        return EQ # both are zero
    end if

    if lenA > 1 begin
        return MORE
    end if

    set digitA = ((natA+BNAT_PTR_OFFSET)@ptr)@i32

    if digit < digitA begin
        return MORE
    end elseif digitA < digit begin
        return LESS
    end if
    return EQ
end proc

# # adds nat1 to nat2 and stores in nat1
proc add[natA:ptr, natB:ptr]
var lenA, lenB, maxLen,
    arraySlotA:ptr, arraySlotB:ptr,
    carry:i32, i, currDigitA:ptr
begin
    set lenA = (natA+BNAT_LEN_OFFSET)@i32:i64
    set lenB = (natB+BNAT_LEN_OFFSET)@i32:i64
    set arraySlotA = natA+BNAT_PTR_OFFSET
    set arraySlotB = natB+BNAT_PTR_OFFSET
    set maxLen = max[lenA, lenB]

    set i = 0
    set carry = 0l
    while i < maxLen or carry > 0l begin
        set currDigitA = arraySlotA@ptr+(i*sizeof i32):ptr
        if i == lenA begin
            push_bnat[natA, 0l]
            set lenA += 1
        end if
        if i < lenB begin
            set currDigitA@i32 += carry + (arraySlotB@ptr+(i*sizeof i32):ptr)@i32
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
end proc

# Adds two numbers, just like add
# but the second number is < BASE,
# that is, it's a single digit number.
proc add_digit[nat:ptr, digit:i32]
var len, arraySlot:ptr,
    carry:i32, i, currDigit:ptr
begin
    set len = (nat+BNAT_LEN_OFFSET)@i32:i64
    set arraySlot = nat+BNAT_PTR_OFFSET

    set i = 0
    if i == len begin
        push_bnat[nat, 0l]
        set len += 1
    end if
    set currDigit = arraySlot@ptr+(i*sizeof i32):ptr
    set carry = 0l
    set currDigit@i32 += digit
    if BASE <= currDigit@i32 begin
        set carry = 1l
        set currDigit@i32 -= BASE
    end if
    set i += 1
    while carry > 0l begin
        if i == len begin
            # push_bnat may alter the pointer at arraySlot
            push_bnat[nat, 0l] 
            set len += 1
        end if
        set currDigit = arraySlot@ptr + (i*sizeof i32):ptr
        set currDigit@i32 += carry
        if BASE <= currDigit@i32 begin
            set carry = 1l
            set currDigit@i32 -= BASE
        end else begin
            set carry = 0l
        end if
        set i += 1
    end while
end proc

memory ERR_STR_OVERFLOW "overflow of string buffer\n"

memory StrBuff 1_000

proc put_bnat[nat:ptr]
var size
begin
    set size = write_bnat[nat, StrBuff, StrBuff.size]
    write[StrBuff, size]
    set_buff[StrBuff, size, 0ss]
end proc

proc write_bnat[nat:ptr, buff:ptr, buffSize] i64
var len, i, arraySlot:ptr, currDigit:ptr,
    block:ptr, first:bool
begin
    set len = (nat+BNAT_LEN_OFFSET)@i32:i64
    set arraySlot = nat+BNAT_PTR_OFFSET

    if len == 0 begin
        put_i32[0l, buff, true]
        return 1;
    end if

    set i = len-1
    set block = buff
    set first = true
    while 0 <= i begin
        set currDigit = arraySlot@ptr + (i*sizeof i32):ptr
        if (block - buff):i64 >= buffSize-DIGITS_PER_INT begin
            fatal[ERR_STR_OVERFLOW, ERR_STR_OVERFLOW.size]
        end if
        put_i32[currDigit@i32, block, first]
        set first = false
        set block += DIGITS_PER_INT:ptr
        set i -= 1
    end while
    return len * DIGITS_PER_INT
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

proc main
begin
    init[]
    test_add_vs_sub[]
    test_mult_digit_vs_mult_base[]
    test_copy[]
    test_slow_div_vs_builtin[]
    test_mult_vs_div[]
end proc

proc test_copy
var a:ptr, b:ptr, c:ptr, d:ptr, e:ptr
begin
    set a = new_bnat[99l]
    mult_base[a]
    set b = new_bnat[0l]
    copy_bnat[b, a]
    if comp[a, b] != EQ begin
        exit 14ss
    end if
    free_bnat[a]
    free_bnat[b]
    validate_init[]
end proc

proc test_mult_digit_vs_mult_base
var a:ptr, b:ptr, c:ptr, d:ptr, e:ptr, i
begin
    set a = new_bnat[99l]
    set b = new_bnat[99l]

    set c = new_bnat[99l]
    set d = new_bnat[1l]
    mult_base[d]
    set e = new_bnat[0l]

    set i = 0
    while i < 100 begin
        mult_digit[a, 100_000_000l]
        mult_digit[a, 10l]

        mult_base[b]

        mult[c, d, e]
        copy_bnat[c, e]

        if comp[a, b] != EQ begin
            exit 12ss
        end if
        if comp[a, c] != EQ begin
            exit 13ss
        end if
        set i += 1
    end while

    free_bnat[a]
    free_bnat[b]
    free_bnat[c]
    free_bnat[d]
    free_bnat[e]
    validate_init[]
end proc

proc test_add_vs_sub
var a:ptr, b:ptr, c:ptr,
    i
begin
    set a = new_bnat[64l]
    set b = new_bnat[512l]
    mult_digit[a, 100_000_000l]
    mult_digit[a, 100_000_000l]
    mult_digit[b, 10_000_000l]
    set c = new_bnat[0l]
    copy_bnat[c, a]

    set i = 0
    while i < 1_000_000 begin
        sub[a, b]
        set i += 1
    end while
    set i = 0
    while i < 1_000_000 begin
        add[a, b]
        set i += 1
    end while

    if comp[a, c] != EQ begin
        put_char['!']
        put_bnat[a]
        put_char['\n']
        put_bnat[c]
        put_char['\n']
        put_bnat[b]
        put_char['\n']
        exit 5ss
    end if

    free_bnat[a]
    free_bnat[b]
    free_bnat[c]
    validate_init[]
end proc

proc test_mult_vs_div
var a:ptr, b:ptr, c:ptr,
    q:ptr, r:ptr,
    i
begin
    set a = new_bnat[64l]
    set b = new_bnat[512l]
    mult_digit[a, 100_000_000l]
    mult_digit[a, 100_000_000l]
    set c = new_bnat[0l]

    set q = new_bnat[0l]
    set r = new_bnat[0l]

    set i = 0
    while i < 1_000 begin
        div[a, b, q, r]
        mult[b, q, c]
        add[c, r]

        if comp[a, c] != EQ begin
            exit 21ss
        end if
        add_digit[a, 9l]
        set i += 1
    end while

    free_bnat[a]
    free_bnat[b]
    free_bnat[c]
    free_bnat[q]
    free_bnat[r]
    validate_init[]
end proc

proc test_slow_div_vs_builtin
var i:i32, j:i32,
    a:ptr, b:ptr, c:ptr, d:ptr
begin
    set a = new_bnat[1l]
    set b = new_bnat[1l]
    set c = new_bnat[1l]
    set d = new_bnat[1l]

    set i = 1l
    while i < 2048l begin
        set_bnat[a, i]

        set j = 1l
        while j < 1024l begin
            set_bnat[b, j]

            slow_div[a, b, c, d]

            if comp_digit[c, i/j] != EQ begin
                put_bnat[c]
                put_char['!']
                put_char['=']
                put_int[(i/j):i64]
                put_char['\n']
                exit 3ss
            end if
            if comp_digit[d, i%j] != EQ begin
                put_bnat[d]
                put_char['!']
                put_char['=']
                put_int[(i%j):i64]
                put_char['\n']
                exit 4ss
            end if

            if j > i begin
                set j = 2048l
            end if
            set j += 1l
        end while
        set i += 1l
    end while
    free_bnat[a]
    free_bnat[b]
    free_bnat[c]
    free_bnat[d]
    validate_init[]
end proc
