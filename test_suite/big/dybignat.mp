# Long Arithmetic for natural numbers (integers >= 0)
# Aritmética de tamanho "arbitrário" para números naturais (inteiros >= 0)
from constants import NULL, WORD
from buffutil import set_buff, copy
from ioutil import fatal, put_int, put_char
from intutil import max
from dfl_alloc import alloc_f, alloc_b, free, init, print_list, print_map

export
    BASE, EQ, MORE, LESS,
    # div, slow_div, # div_digit, div_base, #TODO
    # comp, comp_digit,
    # mult, mult_digit, mult_base,
    # add,
    add_digit
    # sub, # sub_digit, #TODO
    # put_bnat,
    # new_bnat, set_zero,
    # less, less_eq, more, more_eq, eq, neq,
    # put_res

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
const INIT_ALLOC 8

proc new_bnat[first:i32] ptr
var obj:ptr, array:ptr, len:i32
begin
    set obj = alloc_b[BNAT_HEADERSIZE, 0uss, 0us, BNAT_BITMAP]
    set array = alloc_f[INIT_ALLOC]
    if obj == NULL or array == NULL begin
        put_char['1']
        fatal[ERR_OOM, ERR_OOM.size]
    end if

    if first == 0l begin
        set len = 0l
    end else begin
        set len = 1l
    end if

    set (obj+BNAT_CAP_OFFSET)@i32 = (INIT_ALLOC/sizeof i32):i32
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

# tries to increase length by 1
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
var cap:i32, len:i32,
    array:ptr, new_array:ptr
begin
    set cap = (natSource+BNAT_CAP_OFFSET)@i32
    set len = (natSource+BNAT_LEN_OFFSET)@i32
    set array = (natSource+BNAT_PTR_OFFSET)@ptr

    set new_array = alloc_f[cap:i64]
    copy_i32[new_array, array, len:i64]

    set (natDest+BNAT_CAP_OFFSET)@i32 = cap 
    set (natDest+BNAT_LEN_OFFSET)@i32 = len 
    set (natDest+BNAT_PTR_OFFSET)@ptr = new_array 
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
# proc div[
#     natA:ptr,
#     natB:ptr,
#     natQ:ptr,
#     natRem:ptr
# ]
# var sizeA, sizeB, i, j,
#     low:i32, high:i32, guess:i32,
#     res
# begin
#     if comp_digit[natB, 0l] == 0 begin
#         fatal[ERR_DIVISION_BY_ZERO, ERR_DIVISION_BY_ZERO.size]
#     end if

#     set_buff[natIDD, NAT_SIZE, 0ss]
#     set_buff[scratch, NAT_SIZE, 0ss]

#     set sizeA = (natA+BN_SIZE_OFFSET)@i64
#     set sizeB = (natB+BN_SIZE_OFFSET)@i64

#     set natA = natA + BN_FIRST_OFFSET

#     set (natIDD+BN_SIZE_OFFSET)@i64 = 0
#     set (natIDD+BN_FIRST_OFFSET)@i32 = 0l
#     set i = sizeA-1


#     while 0 <= i begin
#         mult_base[natIDD] # multiplies by Base
#         add_digit[natIDD, (natA+(i*sizeof i32):ptr)@i32]

#         # find quotient digit
#         if more[natIDD, natB] begin
#             set low = 1l
#             set guess = BASE/2l
#             set high = BASE
#             set j = 0
#             set res = test_guess[natIDD, scratch, natB, guess]
#             while res != EQ and j < MAXITER begin
#                 if res == LESS begin
#                     set low = guess
#                 end elseif res == MORE begin 
#                     set high = guess
#                 end if
#                 set guess = (low + high)/2l
#                 set res = test_guess[natIDD, scratch, natB, guess]
#                 set j += 1
#             end while
#             if j >= MAXITER begin
#                 exit 4ss
#             end if

#             copy_bnat[scratch, natB]
#             mult_digit[scratch, guess]
#             sub[natIDD, scratch]
#             mult_base[natQ]
#             add_digit[natQ, guess]
#         end elseif eq[natIDD, natB] begin
#             set_zero[natIDD]
#             mult_base[natQ]
#             add_digit[natQ, 1l]
#         end else begin
#             if comp_digit[natQ, 0l] != EQ begin
#                 mult_base[natQ]
#             end if
#         end if
#         set i -= 1
#     end while
#     copy_bnat[natRem, natIDD]
# end proc

# proc test_guess[
#     natIDD:ptr,
#     scratch:ptr,
#     natB:ptr,
#     guess:i32
# ] i64
# var res
# begin
#     copy_bnat[scratch, natB]
#     mult_digit[scratch, guess]
#     set res = comp[scratch, natIDD]
#     if (res == EQ) begin
#         return EQ
#     end if
#     if res == LESS begin 
#         add[scratch, natB]
#         set res = comp[scratch, natIDD]
#         if res == MORE begin # guess*B < IDD and (guess+1)*B > IDD
#             return EQ
#         end if
#         return LESS # too low
#     end if
#     if res == MORE begin
#         return MORE
#     end if
#     exit 5ss
# end proc

# # Computes the solution to A = Q*B + Rem
# # (where A, B are given and Q, Rem are output pointers)
# # by repeated subtraction
# proc slow_div[
#     natA:ptr,
#     natB:ptr,
#     natQ:ptr,
#     natRem:ptr
# ]
# begin
#     if comp_digit[natB, 0l] == 0 begin
#         fatal[ERR_DIVISION_BY_ZERO, ERR_DIVISION_BY_ZERO.size]
#     end if
#     copy_bnat[natRem, natA]
#     while more_eq[natRem, natB] begin
#         sub[natRem, natB]
#         add_digit[natQ, 1l]
#     end while
# end proc

# # we will have to mark the pointers that point to objetcs
# # so the GC can ignore derived pointers.
# #
# # proc mult[natA*:ptr, natB*:ptr, natC*:ptr]
# # var i, j, sizeA, sizeB, sizeC, carry:i32, res,
# #     currdigitA:ptr, currdigitB:ptr, currdigitC:ptr,
# #     sizePtr:ptr
# # begin

# proc mult[natA:ptr, natB:ptr, natC:ptr]
# var i, j, sizeA, sizeB, sizeC, carry:i32, res,
#     currDigitA:ptr, currDigitB:ptr, currDigitC:ptr,
#     sizePtr:ptr
# begin
#     set_buff[natC, NAT_SIZE, 0ss]
#     set sizeA = (natA+BN_SIZE_OFFSET)@i64
#     set sizeB = (natB+BN_SIZE_OFFSET)@i64
#     set sizeC = sizeA+sizeB
#     set sizePtr = natC+BN_SIZE_OFFSET

#     set natA = natA + BN_FIRST_OFFSET
#     set natB = natB + BN_FIRST_OFFSET
#     set natC = natC + BN_FIRST_OFFSET

#     set i = 0
#     while i < sizeA begin
#         set currDigitA = natA+(i*sizeof i32):ptr
#         set j = 0
#         set carry = 0l
#         while (j < sizeB or carry > 0l) and i+j < CAP begin
#             set currDigitB = natB+(j*sizeof i32):ptr
#             set currDigitC = natC+((i+j)*sizeof i32):ptr
#             if j < sizeB begin
#                 set res = currDigitC@i32:i64 + carry:i64 + currDigitA@i32:i64*currDigitB@i32:i64
#             end else begin
#                 set res = currDigitC@i32:i64 + carry:i64
#             end if

#             set currDigitC@i32 = (res % BASE:i64):i32
#             set carry = (res / BASE:i64):i32
            
#             set j += 1
#         end while
#         if i+j >= CAP begin
#             fatal[ERR_NAT_OVERFLOW, ERR_NAT_OVERFLOW.size]
#         end if
#         set i += 1
#     end while

#     set i = sizeC-1
#     set currDigitC = natC+(i*sizeof i32):ptr
#     while currDigitC@i32 == 0l and 0 <= i begin
#         set sizeC -= 1
#         set i -= 1
#         set currDigitC = natC+(i*sizeof i32):ptr
#     end while

#     set (sizePtr)@i64 = sizeC
# end proc

# proc mult_base[natA:ptr]
# var sizeA, i
# begin
#     set sizeA = (natA+BN_SIZE_OFFSET)@i64
#     set sizeA += 1
#     set (natA+BN_SIZE_OFFSET)@i64 = sizeA
#     if sizeA > CAP begin
#         fatal[ERR_NAT_OVERFLOW, ERR_NAT_OVERFLOW.size]
#     end if
#     set natA = natA+BN_FIRST_OFFSET
#     set i = sizeA-1
#     while 0 < i begin
#         set (natA+(i*sizeof i32):ptr)@i32 = (natA+((i-1)*sizeof i32):ptr)@i32
#         set i -= 1
#     end while
#     set natA@i32 = 0l # zeroes the first one
# end proc

proc mult_digit[natA:ptr, digit:i32]
var i, lenA, arraySlotA:ptr,
    carry:i32, res,
    currDigitA:ptr
begin
    set lenA = (natA+BNAT_LEN_OFFSET)@u32:i64
    set arraySlotA = natA+BNAT_PTR_OFFSET

    set i = 0
    set carry = 0l
    while i < lenA or carry > 0l begin
        if i == lenA begin
            push_bnat[natA, 0l]
        end if
        set currDigitA = arraySlotA@ptr+(i*sizeof i32):ptr
        set res = carry:i64 + currDigitA@i32:i64 * digit:i64
        set currDigitA@i32 = (res % BASE:i64):i32
        set carry = (res / BASE:i64):i32
       
        set i += 1
    end while
end proc

# proc sub[natA:ptr, natB:ptr]
# var i, sizeA, sizeB, carry:i32, currDigitA:ptr, currDigitB:ptr,
#     sizePtr:ptr
# begin
#     set i = comp[natA, natB]

#     if i == LESS begin
#         fatal[ERR_NAT_NEGATIVE, ERR_NAT_NEGATIVE.size]
#     end elseif i == EQ begin
#         set_zero[natA]
#         return;
#     end if

#     # por causa da verificação anterior,
#     # sabemos que sizeB <= sizeA
#     set sizeA = (natA+BN_SIZE_OFFSET)@i64
#     set sizeB = (natB+BN_SIZE_OFFSET)@i64
#     set sizePtr = natA+BN_SIZE_OFFSET

#     set natA = natA + BN_FIRST_OFFSET
#     set natB = natB + BN_FIRST_OFFSET

#     set carry = 0l
#     set i = 0
#     while i < sizeB or carry > 0l begin
#         set currDigitA = natA+(i*sizeof i32):ptr
#         set currDigitB = natB+(i*sizeof i32):ptr

#         if i < sizeB begin
#             set currDigitA@i32 -= carry + currDigitB@i32
#         end else begin
#             set currDigitA@i32 -= carry
#         end if

#         if currDigitA@i32 < 0l begin
#             set carry = 1l
#             set currDigitA@i32 += BASE
#         end else begin
#             set carry = 0l
#         end if

#         set i += 1
#     end while

#     set i = sizeA-1
#     set currDigitA = natA+(i*sizeof i32):ptr
#     while currDigitA@i32 == 0l and 0 <= i begin
#         set sizeA -= 1
#         set i -= 1
#         set currDigitA = natA+(i*sizeof i32):ptr
#     end while

#     set (sizePtr)@i64 = sizeA
# end proc

# proc set_zero[nat:ptr]
# begin
#     set (nat+BN_SIZE_OFFSET)@i64 = 0
#     set (nat+BN_FIRST_OFFSET)@i32 = 0l
# end proc

# # natA < natB
# proc less[natA:ptr, natB:ptr] bool
# begin
#     return comp[natA, natB] == LESS
# end proc

# # natA <= natB
# proc less_eq[natA:ptr, natB:ptr] bool
# var res
# begin
#     set res = comp[natA, natB]
#     return res == EQ or res == LESS
# end proc

# # natA > natB
# proc more[natA:ptr, natB:ptr] bool
# begin
#     return comp[natA, natB] == MORE
# end proc

# # natA >= natB
# proc more_eq[natA:ptr, natB:ptr] bool
# var res
# begin
#     set res = comp[natA, natB]
#     return res == EQ or res == MORE
# end proc

# # natA != natB
# proc neq[natA:ptr, natB:ptr] bool
# begin
#     return comp[natA, natB] != EQ
# end proc

# # natA == natB
# proc eq[natA:ptr, natB:ptr] bool
# begin
#     return comp[natA, natB] == EQ
# end proc

const EQ 0
const MORE 1
const LESS 2

# # ~1 if B < A (A is bigger)
# # 0 if A == B
# # 1 if A < B (B is bigger)
# proc comp[natA:ptr, natB:ptr] i64
# var sizeA, sizeB, i, currDigitA:ptr, currDigitB:ptr
# begin
#     set sizeA = (natA+BN_SIZE_OFFSET)@i64
#     set sizeB = (natB+BN_SIZE_OFFSET)@i64

#     if sizeB < sizeA begin
#         return MORE
#     end elseif sizeA < sizeB begin
#         return LESS
#     end if

#     set natA = natA + BN_FIRST_OFFSET
#     set natB = natB + BN_FIRST_OFFSET

#     set i = sizeA-1 # we go backwards (MSD -> LSD)
#     while 0 <= i begin
#         set currDigitA = natA+(i*sizeof i32):ptr
#         set currDigitB = natB+(i*sizeof i32):ptr
        
#         if currDigitB@i32 < currDigitA@i32 begin
#             return MORE
#         end elseif currDigitA@i32 < currDigitB@i32 begin
#             return LESS
#         end if
        
#         set i -= 1
#     end while

#     return EQ
# end proc

# # Compares two numbers, just like comp,
# # with the same outputs,
# # but the second number is < BASE,
# # that is, it's a single digit number.
# proc comp_digit[natA:ptr, digit:i32] i64
# var sizeA, i, digitA:i32
# begin
#     set sizeA = (natA+BN_SIZE_OFFSET)@i64

#     if digit == 0l and sizeA == 0 begin
#         return EQ # both are zero
#     end if

#     if sizeA > 1 begin
#         return MORE
#     end if

#     set digitA = (natA+BN_FIRST_OFFSET)@i32

#     if digit < digitA begin
#         return MORE
#     end elseif digitA < digit begin
#         return LESS
#     end if
#     return EQ
# end proc

# # adds nat1 to nat2 and stores in nat1
proc add[natA:ptr, natB:ptr]
var lenA, lenB, maxLen,
    arraySlotA:ptr, arraySlotB:ptr,
    carry:i32, i, currDigitA:ptr
begin
    set lenA = (natA+BNAT_LEN_OFFSET)@u32:i64
    set lenB = (natB+BNAT_LEN_OFFSET)@u32:i64
    set arraySlotA = natA+BNAT_PTR_OFFSET
    set arraySlotB = natB+BNAT_PTR_OFFSET
    set maxLen = max[lenA, lenB]

    set i = 0
    set carry = 0l
    while i < maxLen or carry > 0l begin
        set currDigitA = arraySlotA@ptr+(i*sizeof i32):ptr
        if i == lenA begin
            push_bnat[natA, 0l]
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
    set len = (nat+BNAT_LEN_OFFSET)@u32:i64
    set arraySlot = nat+BNAT_PTR_OFFSET

    set i = 0
    if i == len begin
        push_bnat[nat, 0l]
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

memory StrBuff 9000 # 9000 digits

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
    set len = (nat+BNAT_LEN_OFFSET)@u32:i64
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

proc main
var a:ptr, b:ptr, i
begin
    init[]
    set a = new_bnat[1l]
    # set b = new_bnat[999_999_999l]
    put_bnat[a]
    put_char['\n']
    #put_bnat[b]
    #put_char['\n']

    put_char['\n']
    set i = 0
    while i < 1_000_000_000 begin
        put_char['$']
        mult_digit[a, 2l]
        put_int[2]
        put_char['^']
        put_int[i+1]
        put_char['=']
        put_bnat[a]
        put_char['\n']
        set i += 1
    end while
end proc
