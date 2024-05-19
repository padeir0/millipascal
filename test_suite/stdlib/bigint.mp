# Here be dragons.
# BigInt arguments are usually uppercase here: A, B, C, ...
# instead of following the usual style for arguments. While
# i32 arguments are normally lowercase.
from io import print, fatal
from arrays import I32A, copy_I32A
from malloc import malloc_panic as malloc, free as _free
from buffutil import set_buff
from ptrutil import distance
from intutil import abs, min_i16, max_i16
from ioutil import put_char, put_int, put_uint, put_bool, oom
from constants import EQ, MORE, LESS

import debug_util as debug

export
    BigInt, BigIntArray,
    new_BigInt, set_zero, set_BigInt,
    put_BigInt, _put_BigInt,
    copy_BigInt, free_BigInt,
    push_digit, is_zero,

    put_BigInt_header,
    BASE,
    put_res,

    abs_comp_digit,
    abs_add_digit, abs_sub_digit, abs_mult_digit, abs_div_digit,

    abs_comp,
    abs_add, abs_sub, abs_mult, abs_div, _abs_div,

    comp_i32,
    add_i32, sub_i32, mult_i32, div_i32,

    comp,
    add, sub, mult, div, _div,

    mult_base, neg, gcd, pow, pow_i32, fact,

data begin
    ERR_SET_INVALID_DIGIT "setting bigint to invalid digit\n";
    ERR_SET_INVALID_I32 "setting bigint to invalid i32\n";
    ERR_TOO_BIG "bigint got too big\n";
    ERR_DIVISION_BY_ZERO "division by zero\n";
    ERR_SCRATCH_TOO_SMALL "scratch space too small\n";
    WTF "wtf\n";
end

const begin
    BASE = 1_000_000_000; # 10^9, 9 digits for each i32
    DIGITS_PER_INT = 9;
end

struct BigIntArray begin
    Int:BigInt;
end

# maximum of I16MAX * 9 = 294903 digits
struct BigInt begin
    Array:I32A;
    Cap,Len:i16;
    Neg:bool;

    _pad1:u8; _pad2:u16;
end

######################################## $ALLOCATION

proc alloc_header[] BigInt
begin
    return malloc[sizeof[BigInt]]:BigInt;
end

proc alloc_I32A[size:i16] I32A
begin
    return malloc[size:i32 * sizeof[i32]]:I32A;
end

######################################## $UTILITARIES

const INIT_ALLOC = 1s

proc new_BigInt[first:i32] BigInt
var obj:BigInt, array:I32A, len:i16
begin
    verify_i32[first];
    set obj = alloc_header[];
    set array = alloc_I32A[INIT_ALLOC];

    if first == 0 begin
        set len = 0s;
    end else begin
        set len = 1s;
    end

    set obj->Neg = first < 0;

    set obj->Cap = INIT_ALLOC;
    set obj->Len = len;
    set obj->Array = array;

    set array->Num = abs[first];

    return obj;
end

proc free_BigInt[bint:BigInt]
begin
    _free[(bint->Array):ptr];
    _free[(bint):ptr];
end

proc put_BigInt_header[bint:BigInt]
begin
    put_char['{'];
    put_char['l'];
    put_char[':'];
    put_int[bint->Len:i64];
    put_char[','];
    put_char['c'];
    put_char[':'];
    put_int[bint->Cap:i64];
    put_char[','];
    put_char['a'];
    put_char[':'];
    put_uint[bint->Array:u64];
    put_char[','];
    put_char['n'];
    put_char[':'];
    put_bool[bint->Neg];
    put_char['}'];
end

proc copy_BigInt[dest:BigInt, source:BigInt] 
begin
    if dest->Cap < source->Cap begin
        _free[dest->Array:ptr];
        set dest->Array = alloc_I32A[source->Cap];
        set dest->Cap = source->Cap;
    end

    set dest->Neg = source->Neg;
    set dest->Len = source->Len;
    copy_I32A[dest->Array, source->Array, source->Len:i32];
    remove_trailing_zeros[dest];
end

proc remove_trailing_zeros[bint:BigInt]
var len, i:i16, curr_digit:I32A
begin
    set len = bint->Len;
    set i = len-1s;
    set curr_digit = bint->Array[i];
    while curr_digit->Num == 0 and 0s <= i begin
        set len -= 1s;
        set i -= 1s;
        set curr_digit = bint->Array[i];
    end

    set bint->Len = len;
end

data ERR_STR_OVERFLOW "overflow of string buffer\n"

# this can be improved :)
data str_buff [2_048]

proc put_BigInt[bint:BigInt]
begin
    _put_BigInt[bint, false, true];
end

proc _put_BigInt[bint:BigInt, pad_left, pad_right:bool]
var size:i32,
begin
    set size = write_BigInt[bint, str_buff, sizeof[str_buff], pad_left, pad_right];
    print[str_buff, size];
    set_buff[str_buff, size, 0ss];
end

proc write_BigInt[bint:BigInt, buff:ptr, buff_size:i32, pad_left, pad_right:bool] i32
var i:i32, curr_digit:I32A,
    block:ptr, first:bool,
    size:i32,
begin
    if bint->Len == 0s begin
        set buff@i8 = '0';
        return 1;
    end

    set size = 0;
    set i = bint->Len:i32 -1;
    set block = buff;
    set first = not pad_left;
    if bint->Neg begin
        set block@i8 = '~';
        set block += 1;
        set size += 1;
    end
    while 0 <= i begin
        set curr_digit = bint->Array[i];
        if distance[block, buff] >= buff_size-DIGITS_PER_INT begin
            fatal[ERR_STR_OVERFLOW, sizeof[ERR_STR_OVERFLOW]];
        end
        _put_i32[curr_digit->Num, block, first];
        set first = false;
        set block += DIGITS_PER_INT;
        set size += DIGITS_PER_INT;
        set i -= 1;
    end
    if not pad_right begin
        while 0 < size and (buff+size-1)@i8 == '0' begin
            set size--;
        end
    end
    return size;
end

proc _put_i32[n:i32, digits:ptr, first:bool]
var p:ptr, i:i32,
begin
	if n == 0 begin
        set i = 0;
        while i <= DIGITS_PER_INT begin
    		set (digits + i)@i8 = '0';
            set i += 1;
        end
        return;
	end
	set p = digits + DIGITS_PER_INT - 1; # end of buffer
	while n > 0 begin
		set p@i8 = (n % 10):i8 + '0';
		set n /= 10;
		set p -= 1;
	end

    # padding
    if not first begin
        while p >= digits begin
    		set p@i8 = '0';
            set p -= 1;
        end
    end
end

proc verify_digit[digit:i32]
begin
    if digit < 0 or digit >= BASE begin
        fatal[ERR_SET_INVALID_DIGIT, sizeof[ERR_SET_INVALID_DIGIT]];
    end
end

proc verify_i32[digit:i32]
begin
    if digit <= ~BASE or digit >= BASE begin
        fatal[ERR_SET_INVALID_I32, sizeof[ERR_SET_INVALID_I32]];
    end
end

proc set_zero[bint:BigInt]
var array:ptr
begin
    set bint->Neg = false;
    set bint->Array->Num = 0;
    set bint->Len = 0s;
end

proc set_BigInt[bint:BigInt, first:i32]
var len:i16
begin
    verify_i32[first];
    if first == 0 begin
        set len = 0s;
    end else begin
        set len = 1s;
    end

    set bint->Neg = first < 0;
    set bint->Array->Num = abs[first];
    set bint->Len = len;
end

proc set_abs_BigInt[bint:BigInt, first:i32]
var len:i16
begin
    verify_digit[first];
    if first == 0 begin
        set len = 0s;
    end else begin
        set len = 1s;
    end

    set bint->Array->Num = first;
    set bint->Len = len;
end

# pushes a digit at the end of the number,
# increasing length by 1
proc push_digit[bint:BigInt, digit:i32]
var cap, len, new_cap:i16,
    array:I32A, new_array:I32A
begin
    verify_digit[digit];
    set cap = bint->Cap;
    set len = bint->Len;
    set array = bint->Array;

    if cap == len begin
        set new_cap = cap*2s;
        if new_cap < 0s begin # detect overflow
            fatal[ERR_TOO_BIG, sizeof[ERR_TOO_BIG]];
        end
        set new_array = alloc_I32A[new_cap];

        copy_I32A[new_array:I32A, array, len:i32];
        _free[array:ptr];

        set bint->Cap = new_cap;
        set bint->Array = new_array:I32A;
        set array = new_array;
    end
    set array[len]->Num = digit;
    set bint->Len = len + 1s;
end

proc put_res[res:i32]
begin
    if res == EQ begin
        put_char['='];
        return;
    end elseif res == MORE begin
        put_char['>'];
        return;
    end elseif res == LESS begin
        put_char['<'];
        return;
    end

    print[WTF, sizeof[WTF]];
    exit 3ss;
end

######################################## $ABSOLUTE ARITHMETIC

proc abs_comp_digit[A:BigInt, digit:i32] i32
var digit_A:i32
begin
    verify_digit[digit];

    if digit == 0 and A->Len == 0s begin
        return EQ; # both are zero
    end

    if A->Len > 1s begin
        return MORE;
    end

    set digit_A = A->Array->Num;

    if digit < digit_A begin
        return MORE;
    end elseif digit_A < digit begin
        return LESS;
    end
    return EQ;
end

# A and C need not to be different objects
proc abs_add_digit[A:BigInt, digit:i32, C:BigInt]
var i:i16,
    curr_digit_C:I32A,
    carry, res:i32
begin
    verify_digit[digit];

    if A->Len == 0s begin
        set_BigInt[C, digit];
        return;
    end
    if C->Len == 0s begin
        push_digit[C, 0];
    end

    set i = 0s;
    set carry = 0;
    set curr_digit_C = C->Array;
    do begin
        if i == C->Len begin
            # push_digit may alter the pointer at arraySlot
            push_digit[C, 0];
        end
        set curr_digit_C = C->Array[i];

        set res = carry;
        if i == 0s begin
            set res += digit;
        end
        if i < A->Len begin
            set res += A->Array[i]->Num;
        end
        if BASE <= res begin
            set carry = 1;
            set curr_digit_C->Num = res - BASE;
        end else begin
            set carry = 0;
            set curr_digit_C->Num = res;
        end
        set i += 1s;
    end while carry > 0;
end

proc abs_sub_digit[A:BigInt, digit:i32, C:BigInt]
var i:i16,
    carry, res:i32,
    curr_digit_A, curr_digit_C:I32A,
begin
    set res = abs_comp_digit[A, digit];

    if res == LESS begin
        # in this case, lenA <= 1
        # and we do sub_digit[C, A] -> C instead
        set_abs_BigInt[C, digit - A->Array->Num];
        return;
    end elseif res == EQ begin
        set_zero[C];
        return;
    end
    # for now on, we know that 1 <= lenA
    # and that A > digit
    set carry = 0;
    set i = 0s;
    do begin
        if i == C->Len begin
            push_digit[C, 0];
        end
        set curr_digit_A = A->Array[i];
        set curr_digit_C = C->Array[i];

        if i < A->Len begin
            set res = curr_digit_A->Num;
        end else begin
            set res = 0;
        end
        if i == 0s begin
            set res -= digit;
        end
        set curr_digit_C->Num = res - carry;

        if curr_digit_C->Num < 0 begin
            set carry = 1;
            set curr_digit_C->Num += BASE;
        end else begin
            set carry = 0;
        end

        set i += 1s;
    end while carry > 0;
    
    remove_trailing_zeros[C];
end

# A may be the same object as C
proc abs_mult_digit[A:BigInt, digit:i32, C:BigInt]
var carry:i32,
    i:i16,
    res:i64,
    curr_digit_A, curr_digit_C:I32A
begin
    verify_digit[digit];

    set C->Len = min_i16[C->Len, A->Len];

    set i = 0s;
    set carry = 0;
    while i < A->Len or carry > 0 begin
        if i == C->Len begin
            push_digit[C, 0];
        end
        set curr_digit_A = A->Array[i];
        set curr_digit_C = C->Array[i];

        set res = carry:i64;
        if i < A->Len begin
            set res += curr_digit_A->Num:i64 * digit:i64;
        end
        set curr_digit_C->Num = (res % BASE:i64):i32;
        set carry = (res / BASE:i64):i32;
       
        set i += 1s;
    end

    remove_trailing_zeros[C];
end

# since 0 <= rem < B
# we can store rem in a i32
# and return it from the procedure
#
# furthermore,
# IDD (intermediate dividend) will have at most 2 digits,
# because B has only 1, so we can store it in an i64
proc abs_div_digit[A:BigInt, b:i32, iQ:BigInt] i32
var idd, q, b64:i64,
    i:i16,
begin
    if b == 0 begin
        fatal[ERR_DIVISION_BY_ZERO, sizeof[ERR_DIVISION_BY_ZERO]];
    end
    set_zero[iQ];

    set b64 = b:i64;

    set idd = 0l;
    set i = A->Len -1s;
    while 0s <= i begin
        set idd *= BASE:i64;
        set idd += A->Array[i]->Num:i64;

        set q = idd / b64;
        set idd -= q*b64;

        mult_base[iQ];
        abs_add_digit[iQ, q:i32, iQ];
        
        set i -= 1s;
    end
    remove_trailing_zeros[iQ];
    # at the end of the loop, idd will be less than BASE,
    # so we can safely cast it
    return idd:i32;
end

# A and B need not to be different objects
proc abs_comp[A, B:BigInt] i32
var i:i16,
    curr_digit_A, curr_digit_B:I32A
begin
    if B->Len < A->Len begin
        return MORE;
    end elseif A->Len < B->Len begin
        return LESS;
    end

    set i = A->Len - 1s; # we go backwards (MSD -> LSD)
    while 0s <= i begin
        set curr_digit_A = A->Array[i];
        set curr_digit_B = B->Array[i];
        
        if curr_digit_B->Num < curr_digit_A->Num begin
            return MORE;
        end elseif curr_digit_A->Num < curr_digit_B->Num begin
            return LESS;
        end
        
        set i -= 1s;
    end

    return EQ;
end

# abs_adds A to B and stores in C
#
# A, B and C need not to be different objects
proc abs_add[A, B, C:BigInt]
var max_len, i:i16,
    carry:i32, curr_digit:I32A,
    res:i32
begin
    set max_len = max_i16[A->Len, B->Len];

    set i = 0s;
    set carry = 0;
    while i < max_len or carry > 0 begin
        if i == C->Len begin
            push_digit[C, 0];
        end
        set curr_digit = C->Array[i];
        set res = carry;
        if i < A->Len begin
            set res += A->Array[i]->Num;
        end
        if i < B->Len begin
            set res += B->Array[i]->Num;
        end
        set curr_digit->Num = res;
        if BASE <= curr_digit->Num begin
            set carry = 1;
            set curr_digit->Num -= BASE;
        end else begin
            set carry = 0;
        end
        set i += 1s;
    end
end

# A, B and C need not to be different objects
proc abs_sub[A, B, C:BigInt]
var i:i16,
    carry, res:i32,
    curr_digit_A, curr_digit_B, curr_digit_C:I32A,
    hold:BigInt
begin
    set res = abs_comp[A, B];
    if res == LESS begin
        set hold = A;
        set A = B;
        set B = hold;
    end elseif res == EQ begin
        set_zero[C];
        return;
    end
    # for now on, we know that lenB <= lenA

    # if B and C are the same object set to zero
    # then C will not be set to A, so we do a separate check
    if abs_comp_digit[B, 0] == EQ begin
        copy_BigInt[C, A];
        return;
    end

    set carry = 0;
    set i = 0s;
    while i < B->Len or carry > 0 begin
        if i == C->Len begin
            push_digit[C, 0];
        end
        set curr_digit_A = A->Array[i];
        set curr_digit_B = B->Array[i];
        set curr_digit_C = C->Array[i];

        set res = ~carry;
        if i < A->Len begin
            set res += curr_digit_A->Num;
        end
        if i < B->Len begin
            set res -= curr_digit_B->Num;
        end
        set curr_digit_C->Num = res;

        if curr_digit_C->Num < 0 begin
            set carry = 1;
            set curr_digit_C->Num += BASE;
        end else begin
            set carry = 0;
        end

        set i += 1s;
    end
    
    remove_trailing_zeros[C];
end

# C must be a different object from A and B
proc abs_mult[A, B, C:BigInt]
var i, j:i16,
    res:i64,
    carry:i32,
    curr_digit_A, curr_digit_B, curr_digit_C:I32A,
begin
    set_zero[C];

    # we might realloc C's array
    set i = 0s;
    while i < A->Len begin
        set curr_digit_A = A->Array[i];
        set j = 0s;
        set carry = 0;
        while j < B->Len or carry > 0 begin
            if i+j == C->Len begin
                push_digit[C, 0];
            end
            set curr_digit_B = B->Array[j];
            set curr_digit_C = C->Array[i+j];
            if j < B->Len begin
                set res = curr_digit_C->Num:i64 +
                          carry:i64 +
                          curr_digit_A->Num:i64 * curr_digit_B->Num:i64;
            end else begin
                set res = curr_digit_C->Num:i64 + carry:i64;
            end

            set curr_digit_C->Num = (res % BASE:i64):i32;
            set carry = (res / BASE:i64):i32;
            
            set j += 1s;
        end
        set i += 1s;
    end

    remove_trailing_zeros[C];
end

const MAXITER = 33s

proc _abs_div[A, B, Q, Rem, scratch:BigInt]
var i, j:i16,
    low, high, guess, res:i32,
begin
    if abs_comp_digit[B, 0] == EQ begin
        fatal[ERR_DIVISION_BY_ZERO, sizeof[ERR_DIVISION_BY_ZERO]];
    end
    set_zero[Q];
    set_zero[Rem];

    set i = A->Len - 1s;
    while 0s <= i begin
        mult_base[Rem];
        abs_add_digit[Rem, A->Array[i]->Num, Rem];

        # find quotient digit
        if abs_comp[Rem, B] == MORE begin
            set low = 1;
            set guess = BASE/2;
            set high = BASE;
            set j = 0s;
            set res = test_guess[Rem, scratch, B, guess];
            while res != EQ and j < MAXITER begin
                if res == LESS begin
                    set low = guess;
                end elseif res == MORE begin 
                    set high = guess;
                end
                set guess = (low + high)/2;
                set res = test_guess[Rem, scratch, B, guess];
                set j += 1s;
            end
            if j >= MAXITER begin
                exit 4ss;
            end

            abs_mult_digit[B, guess, scratch];
            abs_sub[Rem, scratch, Rem];
            mult_base[Q];
            abs_add_digit[Q, guess, Q];
        end elseif abs_comp[Rem, B] == EQ begin
            set_zero[Rem];
            mult_base[Q];
            abs_add_digit[Q, 1, Q];
        end else begin
            if abs_comp_digit[Q, 0] != EQ begin
                mult_base[Q];
            end
        end
        set i -= 1s;
    end
end


# abs_div computes the solution to A = Q*B + Rem
# (where A, B are given and Q, Rem are output pointers)
# by long division
#
# bintQ, bintRem must be different objects from bintA and bintB, although
# bintA may be the same as bintB
proc abs_div[A, B, Q, Rem:BigInt]
var scratch:BigInt
begin
    set scratch = new_BigInt[0];
    _abs_div[A, B, Q, Rem, scratch];
    free_BigInt[scratch];
end

proc test_guess[idd, scratch, B:BigInt, guess:i32] i32
var res:i32
begin
    abs_mult_digit[B, guess, scratch];
    set res = abs_comp[scratch, idd];
    if res == EQ begin
        return EQ;
    end
    if res == LESS begin 
        abs_add[scratch, B, scratch];
        set res = abs_comp[scratch, idd];
        if res == MORE begin # guess*B < IDD and (guess+1)*B > IDD
            return EQ;
        end
        return LESS; # too low
    end
    if res == MORE begin
        return MORE;
    end
    exit 99ss;
end

# start with abs_<op>_digit (done),
#       then abs_<op> (done),
#       then <op>_i32,
#       then <op>

######################################## $INTEGER ARITHMETIC

proc comp_i32[A:BigInt, b:i32] i32
var neg_A, neg_B:bool, res:i32
begin
    set neg_A = A->Neg;
    set neg_B = b < 0;
    set b = abs[b];

    if not neg_A and not neg_B begin
        return abs_comp_digit[A, b];
    end elseif neg_A and neg_B begin
        set res = abs_comp_digit[A, b];
        # invert the result
        if res == MORE begin
            return LESS;
        end elseif res == LESS begin
            return MORE;
        end else begin
            return EQ;
        end
    end elseif not neg_A and neg_B begin
        return MORE;
    end elseif neg_A and not neg_B begin
        return LESS;
    end
    fatal[WTF, sizeof[WTF]];
    return ~1;
end

proc sub_i32[A:BigInt, b:i32, C:BigInt]
var neg_A, neg_B, res:bool
begin
    set neg_A = A->Neg;
    set neg_B = b < 0;
    set b = abs[b];

    if not neg_A and not neg_B begin
    	# (1 * a) - (1 * b) = a-b
        set res = pos_sub_digit_sign[A, b, C];
        set C->Neg = res;
    end elseif neg_A and neg_B begin
    	# (-1 * a) - (-1 * b) = (-1)*(a-b)
        set res = pos_sub_digit_sign[A, b, C];
        set C->Neg = not res;
    end elseif neg_A and not neg_B begin
	    # (-1 * a) - (1 * b)  = (-1)*((1*b) - (-1*a)) = (-1)*(b+a)
        abs_add_digit[A, b, C];
        set C->Neg = true;
    end elseif not neg_A and neg_B begin
    	# (1 * a) - (-1 * b)  = a+b
        abs_add_digit[A, b, C];
        set C->Neg = false;
    end
    if is_zero[C] begin # +0 -0 bug
        set C->Neg = false;
    end
end

proc add_i32[A:BigInt, b:i32, C:BigInt]
var neg_A, neg_B, res:bool
begin
    set neg_A = A->Neg;
    set neg_B = b < 0;
    set b = abs[b];

    if not neg_A and not neg_B begin
        # (1 * a) + (1 * b)   = a+b
        abs_add_digit[A, b, C];
        set C->Neg = false;
    end elseif neg_A and neg_B begin
    	# (-1 * a) + (-1 * b) = (-1)*(a+b)
        abs_add_digit[A, b, C];
        set C->Neg = true;
    end elseif neg_A and not neg_B begin
    	# (-1 * a) + (1 * b)  = b-a = (-1)*(a-b)
        set res = pos_sub_digit_sign[A, b, C];
        set C->Neg = not res;
    end elseif not neg_A and neg_B begin
    	# (1 * a) + (-1 * b)  = a-b
        set res = pos_sub_digit_sign[A, b, C];
        set C->Neg = res;
    end
    if is_zero[C] begin # +0 -0 bug
        set C->Neg = false;
    end
end

proc mult_i32[A:BigInt, b:i32, C:BigInt]
var neg, neg_A, neg_B:bool
begin
    set neg_A = A->Neg;
    set neg_B = b < 0;
    set neg = mult_signal[neg_A, neg_B];
    set b = abs[b];
    abs_mult_digit[A, b, C];

    if is_zero[C] begin
        set C->Neg = false;
    end else begin
        set C->Neg = neg;
    end
end

proc div_i32[A:BigInt, b:i32, Q:BigInt] i32
var neg, neg_A, neg_B:bool, rem:i32
begin
    set neg_A = A->Neg;
    set neg_B = b < 0;
    set neg = mult_signal[neg_A, neg_B];
    set b = abs[b];

    set rem = abs_div_digit[A, b, Q];

    if is_zero[Q] begin
        set Q->Neg = false;
    end else begin
        set Q->Neg = neg;
    end

    if rem != 0 and neg_A != (rem < 0) begin
        set rem = ~rem;
    end
    return rem;
end

proc mult_signal[a, b:bool] bool
begin
    # 1*1 = (-1)*(-1) = 1
    if (not a and not b) or (a and b) begin
        return false;
    # (-1)*1 = 1*(-1) = -1
    end elseif (not a and b) or (a and not b) begin
        return true;
    end
    fatal[WTF, sizeof[WTF]];
    return false;
end

# subtracts two positive numbers
# then returns a sign based on magnitude
proc pos_sub_sign[A, B, C:BigInt] bool
var res:i32
begin
    set res = abs_comp[A, B];
    abs_sub[A, B, C];
    if res == MORE begin
        # if a > b then a-b > 0
        return false;
    end elseif res == LESS begin
        # if a < b then a-b < 0
        return true;
    end
    return false;
end

# same as above, but second number may is an i32
proc pos_sub_digit_sign[A:BigInt, b:i32, C:BigInt] bool
var res:i32
begin
    set res = abs_comp_digit[A, b];
    abs_sub_digit[A, b, C];
    if res == MORE begin
        return false;
    end elseif res == LESS begin
        return true;
    end
    return false;
end

proc is_zero[bint:BigInt] bool
begin
    return bint->Len == 0s;
end

proc mult_base[bint:BigInt]
var len, i:i16, array:I32A
begin
    push_digit[bint, 0];
    set array = bint->Array;
    set i = bint->Len - 1s;
    while 0s < i begin
        set array[i]->Num = array[i - 1s]->Num;
        set i -= 1s;
    end
    set array->Num = 0; # zeroes the first one
end

proc neg[bint:BigInt]
begin
    set bint->Neg = not bint->Neg;
end

proc comp[A, B:BigInt] i32
var neg_A, neg_B:bool, res:i32
begin
    set neg_A = A->Neg;
    set neg_B = B->Neg;
    if not neg_A and not neg_B begin
        return abs_comp[A, B];
    end elseif neg_A and neg_B begin
        set res = abs_comp[A, B];
        # invert the result
        if res == MORE begin
            return LESS;
        end elseif res == LESS begin
            return MORE;
        end else begin
            return EQ;
        end
    end elseif not neg_A and neg_B begin
        return MORE;
    end elseif neg_A and not neg_B begin
        return LESS;
    end
    fatal[WTF, sizeof[WTF]];
    return ~1;
end

proc add[A, B, C:BigInt]
var neg_A, neg_B, res:bool
begin
    set neg_A = A->Neg;
    set neg_B = B->Neg;

    if not neg_A and not neg_B begin
        # (1 * a) + (1 * b)   = 1*(a+b)
        abs_add[A, B, C];
        set C->Neg = false;
    end elseif neg_A and neg_B begin
    	# (-1 * a) + (-1 * b) = -1*(a+b)
        abs_add[A, B, C];
        set C->Neg = true;
    end elseif neg_A and not neg_B begin
    	# (-1 * a) + (1 * b)  = b-a
        set res = pos_sub_sign[B, A, C];
        set C->Neg = res;
    end elseif not neg_A and neg_B begin
    	# (1 * a) + (-1 * b)  = a-b
        set res = pos_sub_sign[A, B, C];
        set C->Neg = res;
    end

    if is_zero[C] begin
        set C->Neg = false;
    end
end

proc sub[A, B, C:BigInt]
var neg_A:bool, neg_B:bool, res:bool
begin
    set neg_A = A->Neg;
    set neg_B = B->Neg;
    if not neg_A and not neg_B begin
    	# (1 * a) - (1 * b) = a-b
        set res = pos_sub_sign[A, B, C];
        set C->Neg = res;
    end elseif neg_A and neg_B begin
    	# (-1 * a) - (-1 * b) = (-1)*(a-b)
        set res = pos_sub_sign[A, B, C];
        set C->Neg = not res;
    end elseif neg_A and not neg_B begin
	    # (-1 * a) - (1 * b)  = (-1)*((1*b) - (-1*a)) = (-1)*(b+a)
        abs_add[A, B, C];
        set C->Neg = true;
    end elseif not neg_A and neg_B begin
    	# (1 * a) - (-1 * b)  = a+b
        abs_add[A, B, C];
        set C->Neg = false;
    end

    if is_zero[C] begin
        set C->Neg = false;
    end
end

proc mult[A, B, C:BigInt]
var neg, neg_A, neg_B:bool
begin
    set neg_A = A->Neg;
    set neg_B = B->Neg;
    set neg = mult_signal[neg_A, neg_B];
    abs_mult[A, B, C];
    if is_zero[C] begin
        set C->Neg = false;
    end else begin
        set C->Neg = neg;
    end
end

proc _div[A, B, Q, Rem, scratch:BigInt]
var neg, neg_A, neg_B:bool
begin
    set neg_A = A->Neg;
    set neg_B = B->Neg;
    set neg = mult_signal[neg_A, neg_B];
    _abs_div[A, B, Q, Rem, scratch];

    if is_zero[Q] begin
        set Q->Neg = false;
    end else begin
        set Q->Neg = neg;
    end

    if is_zero[Rem] begin
        set Rem->Neg = false;
    end else begin
        set Rem->Neg = neg_A;
    end
end

proc div[A, B, Q, Rem:BigInt]
var scratch:BigInt
begin
    set scratch = new_BigInt[0];
    _div[A, B, Q, Rem, scratch];
    free_BigInt[scratch];
end

proc gcd[A, B, out:BigInt, scratch:BigIntArray, size:i32]
var scr_a, scr_b, scr_q, scr_t, scr_div:BigInt,
    actual_out, hold:BigInt
begin
    if size < 4 begin
        fatal[ERR_SCRATCH_TOO_SMALL, sizeof[ERR_SCRATCH_TOO_SMALL]];
    end
    set scr_a = scratch[0]->Int; set_zero[scr_a];
    set scr_b = scratch[1]->Int; set_zero[scr_b];
    set scr_q = scratch[2]->Int; set_zero[scr_q];
    set scr_div = scratch[3]->Int; set_zero[scr_div];
    set actual_out = out;

    copy_BigInt[scr_a, A];
    copy_BigInt[scr_b, B];

    while not is_zero[scr_b] begin
        _div[scr_a, scr_b, scr_q, out, scr_div];
        set hold = scr_a;
        set scr_a = scr_b;
        set scr_b = out;
        set out = hold;
    end
    copy_BigInt[actual_out, scr_a];
    set out->Neg = false;
end

proc pow[A:BigInt, b:i32, C:BigInt, scratch:BigInt]
begin
    set_BigInt[C, 1];
    while 1 < b begin
        mult[C, A, scratch];
        copy_BigInt[C, scratch];
        set b--;
    end
end

proc pow_i32[a, b:i32, C:BigInt]
begin
    verify_i32[a];
    verify_i32[b];

    set_BigInt[C, 1];
    while 0 < b begin
        mult_i32[C, a, C];
        set b--;
    end
end

proc fact[a:i32, out:BigInt]
begin
    verify_i32[a];

    set_BigInt[out, 1];
    while 0 < a begin
        mult_i32[out, a, out];
        set a--;
    end
end
