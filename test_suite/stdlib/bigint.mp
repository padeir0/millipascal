# Here be dragons.
from io import print, fatal
from arrays import I32A, copy_I32A
from malloc import malloc as _malloc, free as _free
from buffutil import set_buff
from ptrutil import distance
from intutil import abs, min_i16, max_i16
from ioutil import put_char, put_int, put_uint, put_bool, oom

export
    BigInt,
    new_BigInt, set_zero, set_BigInt,
    put_BigInt, copy_BigInt, free_BigInt,
    push_digit,

    put_BigInt_header,
    BASE, EQ, MORE, LESS,
    put_res,

    abs_comp_digit,
    abs_add_digit, abs_sub_digit, abs_mult_digit, abs_div_digit,

    abs_comp,
    abs_add, abs_sub, abs_mult, abs_div,

    mult_base, neg,

    comp_i32,
    div_i32, mult_i32, add_i32, sub_i32,

    comp,

    # TODO: finish these and fix -0 +0 bug
    # div, mult, add, sub


data begin
    ERR_SET_INVALID_DIGIT "setting bigint to invalid digit\n";
    ERR_SET_INVALID_I32 "setting bigint to invalid i32\n";
    ERR_TOO_BIG "bigint got too big\n";
    ERR_DIVISION_BY_ZERO "division by zero\n";
    WTF "wtf\n";
end

const begin
    BASE = 1_000_000_000; # 10^9, 9 digits for each i32
    DIGITS_PER_INT = 9;
end

# maximum of I16MAX * 9 = 294903 digits
struct BigInt begin
    Array:I32A;
    Cap,Len:i16;
    Neg:bool;

    _pad1:u8; _pad2:u16;
end

######################################## $ALLOCATION

proc malloc[size:i32] ptr
var p:ptr, ok:bool
begin
    set p, ok = _malloc[size];
    if not ok begin
        oom[];
    end
    return p;
end

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

data str_buff [1_000]

proc put_BigInt[bint:BigInt]
var size:i32,
begin
    set size = write_BigInt[bint, str_buff, sizeof[str_buff]];
    print[str_buff, size];
    set_buff[str_buff, size, 0ss];
end

proc write_BigInt[bint:BigInt, buff:ptr, buff_size:i32] i32
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
    set first = true;
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

const begin
    EQ = 0;
    MORE = 1;
    LESS = 2;
end

proc abs_comp_digit[iA:BigInt, digit:i32] i32
var lenA, digitA:i32
begin
    verify_digit[digit];

    if digit == 0 and iA->Len == 0s begin
        return EQ; # both are zero
    end

    if iA->Len > 1s begin
        return MORE;
    end

    set digitA = iA->Array->Num;

    if digit < digitA begin
        return MORE;
    end elseif digitA < digit begin
        return LESS;
    end
    return EQ;
end

# iA and iC need not to be different objects
proc abs_add_digit[iA:BigInt, digit:i32, iC:BigInt]
var i:i16,
    currDigitC:I32A,
    carry, res:i32
begin
    verify_digit[digit];

    if iA->Len == 0s begin
        set_BigInt[iC, digit];
        return;
    end
    if iC->Len == 0s begin
        push_digit[iC, 0];
    end

    set i = 0s;
    set carry = 0;
    set currDigitC = iC->Array;
    do begin
        if i == iC->Len begin
            # push_digit may alter the pointer at arraySlot
            push_digit[iC, 0];
        end
        set currDigitC = iC->Array[i];

        set res = carry;
        if i == 0s begin
            set res += digit;
        end
        if i < iA->Len begin
            set res += iA->Array[i]->Num;
        end
        if BASE <= res begin
            set carry = 1;
            set currDigitC->Num = res - BASE;
        end else begin
            set carry = 0;
            set currDigitC->Num = res;
        end
        set i += 1s;
    end while carry > 0;
end

proc abs_sub_digit[iA:BigInt, digit:i32, iC:BigInt]
var i:i16,
    carry, res:i32,
    currDigitA, currDigitC:I32A,
begin
    set res = abs_comp_digit[iA, digit];

    if res == LESS begin
        # in this case, lenA <= 1
        # and we do sub_digit[iC, iA] -> iC instead
        set_abs_BigInt[iC, digit - iA->Array->Num];
        return;
    end elseif res == EQ begin
        set_zero[iC];
        return;
    end
    # for now on, we know that 1 <= lenA
    # and that iA > digit
    set carry = 0;
    set i = 0s;
    do begin
        if i == iC->Len begin
            push_digit[iC, 0];
        end
        set currDigitA = iA->Array[i];
        set currDigitC = iC->Array[i];

        if i < iA->Len begin
            set res = currDigitA->Num;
        end else begin
            set res = 0;
        end
        if i == 0s begin
            set res -= digit;
        end
        set currDigitC->Num = res - carry;

        if currDigitC->Num < 0 begin
            set carry = 1;
            set currDigitC->Num += BASE;
        end else begin
            set carry = 0;
        end

        set i += 1s;
    end while carry > 0;
    
    remove_trailing_zeros[iC];
end

# iA may be the same object as iC
proc abs_mult_digit[iA:BigInt, digit:i32, iC:BigInt]
var carry:i32,
    i:i16,
    res:i64,
    currDigitA, currDigitC:I32A
begin
    verify_digit[digit];

    set iC->Len = min_i16[iC->Len, iA->Len];

    set i = 0s;
    set carry = 0;
    while i < iA->Len or carry > 0 begin
        if i == iC->Len begin
            push_digit[iC, 0];
        end
        set currDigitA = iA->Array[i];
        set currDigitC = iC->Array[i];

        set res = carry:i64;
        if i < iA->Len begin
            set res += currDigitA->Num:i64 * digit:i64;
        end
        set currDigitC->Num = (res % BASE:i64):i32;
        set carry = (res / BASE:i64):i32;
       
        set i += 1s;
    end

    remove_trailing_zeros[iC];
end

# since 0 <= rem < B
# we can store rem in a i32
# and return it from the procedure
#
# furthermore,
# IDD (intermediate dividend) will have at most 2 digits,
# because B has only 1, so we can store it in an i64
proc abs_div_digit[iA:BigInt, b:i32, iQ:BigInt] i32
var idd, q, b64:i64,
    i:i16,
begin
    if b == 0 begin
        fatal[ERR_DIVISION_BY_ZERO, sizeof[ERR_DIVISION_BY_ZERO]];
    end
    set_zero[iQ];

    set b64 = b:i64;

    set idd = 0l;
    set i = iA->Len -1s;
    while 0s <= i begin
        set idd *= BASE:i64;
        set idd += iA->Array[i]->Num:i64;

        set q = idd / b64;
        set idd -= q*b64;

        mult_base[iQ];
        abs_add_digit[iQ, q:i32, iQ];
        
        set i -= 1s;
    end
    # at the end of the loop, idd will be less than BASE,
    # so we can safely cast it
    return idd:i32;
end

# iA and iB need not to be different objects
proc abs_comp[iA, iB:BigInt] i32
var i:i16,
    currDigitA, currDigitB:I32A
begin
    if iB->Len < iA->Len begin
        return MORE;
    end elseif iA->Len < iB->Len begin
        return LESS;
    end

    set i = iA->Len - 1s; # we go backwards (MSD -> LSD)
    while 0s <= i begin
        set currDigitA = iA->Array[i];
        set currDigitB = iB->Array[i];
        
        if currDigitB->Num < currDigitA->Num begin
            return MORE;
        end elseif currDigitA->Num < currDigitB->Num begin
            return LESS;
        end
        
        set i -= 1s;
    end

    return EQ;
end

# abs_adds iA to iB and stores in iC
#
# iA, iB and iC need not to be different objects
proc abs_add[iA, iB, iC:BigInt]
var maxLen, i:i16,
    carry:i32, curr_digit:I32A,
    res:i32
begin
    set maxLen = max_i16[iA->Len, iB->Len];

    set i = 0s;
    set carry = 0;
    while i < maxLen or carry > 0 begin
        if i == iC->Len begin
            push_digit[iC, 0];
        end
        set curr_digit = iC->Array[i];
        set res = carry;
        if i < iA->Len begin
            set res += iA->Array[i]->Num;
        end
        if i < iB->Len begin
            set res += iB->Array[i]->Num;
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

# iA, iB and iC need not to be different objects
proc abs_sub[iA, iB, iC:BigInt]
var i:i16,
    carry, res:i32,
    currDigitA, currDigitB, currDigitC:I32A,
    hold:BigInt
begin
    set res = abs_comp[iA, iB];
    if res == LESS begin
        set hold = iA;
        set iA = iB;
        set iB = hold;
    end elseif res == EQ begin
        set_zero[iC];
        return;
    end
    # for now on, we know that lenB <= lenA

    # if iB and iC are the same object set to zero
    # then iC will not be set to iA, so we do a separate check
    if abs_comp_digit[iB, 0] == EQ begin
        copy_BigInt[iC, iA];
        return;
    end

    set carry = 0;
    set i = 0s;
    while i < iB->Len or carry > 0 begin
        if i == iC->Len begin
            push_digit[iC, 0];
        end
        set currDigitA = iA->Array[i];
        set currDigitB = iB->Array[i];
        set currDigitC = iC->Array[i];

        set res = ~carry;
        if i < iA->Len begin
            set res += currDigitA->Num;
        end
        if i < iB->Len begin
            set res -= currDigitB->Num;
        end
        set currDigitC->Num = res;

        if currDigitC->Num < 0 begin
            set carry = 1;
            set currDigitC->Num += BASE;
        end else begin
            set carry = 0;
        end

        set i += 1s;
    end
    
    remove_trailing_zeros[iC];
end

# iC must be a different object from iA and iB
proc abs_mult[iA, iB, iC:BigInt]
var i, j:i16,
    res:i64,
    carry:i32,
    currDigitA, currDigitB, currDigitC:I32A,
begin
    set_zero[iC];

    # we might realloc iC's array
    set i = 0s;
    while i < iA->Len begin
        set currDigitA = iA->Array[i];
        set j = 0s;
        set carry = 0;
        while j < iB->Len or carry > 0 begin
            if i+j == iC->Len begin
                push_digit[iC, 0];
            end
            set currDigitB = iB->Array[j];
            set currDigitC = iC->Array[i+j];
            if j < iB->Len begin
                set res = currDigitC->Num:i64 +
                          carry:i64 +
                          currDigitA->Num:i64 * currDigitB->Num:i64;
            end else begin
                set res = currDigitC->Num:i64 + carry:i64;
            end

            set currDigitC->Num = (res % BASE:i64):i32;
            set carry = (res / BASE:i64):i32;
            
            set j += 1s;
        end
        set i += 1s;
    end

    remove_trailing_zeros[iC];
end


const MAXITER = 33s

# abs_div computes the solution to A = Q*B + Rem
# (where A, B are given and Q, Rem are output pointers)
# by long division
#
# bintQ, bintRem must be different objects from bintA and bintB, although
# bintA may be the same as bintB
proc abs_div[iA, iB, iQ, iRem:BigInt]
var i, j:i16,
    low, high, guess, res:i32,
    iIDD, iScratch:BigInt
begin
    if abs_comp_digit[iB, 0] == EQ begin
        fatal[ERR_DIVISION_BY_ZERO, sizeof[ERR_DIVISION_BY_ZERO]];
    end
    set_zero[iQ];
    set_zero[iRem];
    set iIDD = new_BigInt[0]; # TODO: remove these allocations
    set iScratch = new_BigInt[0];

    set i = iA->Len - 1s;

    while 0s <= i begin
        mult_base[iIDD];
        abs_add_digit[iIDD, iA->Array[i]->Num, iIDD];

        # find quotient digit
        if abs_comp[iIDD, iB] == MORE begin
            set low = 1;
            set guess = BASE/2;
            set high = BASE;
            set j = 0s;
            set res = test_guess[iIDD, iScratch, iB, guess];
            while res != EQ and j < MAXITER begin
                if res == LESS begin
                    set low = guess;
                end elseif res == MORE begin 
                    set high = guess;
                end
                set guess = (low + high)/2;
                set res = test_guess[iIDD, iScratch, iB, guess];
                set j += 1s;
            end
            if j >= MAXITER begin
                exit 4ss;
            end

            copy_BigInt[iScratch, iB];
            abs_mult_digit[iScratch, guess, iScratch];
            abs_sub[iIDD, iScratch, iIDD];
            mult_base[iQ];
            abs_add_digit[iQ, guess, iQ];
        end elseif abs_comp[iIDD, iB] == EQ begin
            set_zero[iIDD];
            mult_base[iQ];
            abs_add_digit[iQ, 1, iQ];
        end else begin
            if abs_comp_digit[iQ, 0] != EQ begin
                mult_base[iQ];
            end
        end
        set i -= 1s;
    end
    copy_BigInt[iRem, iIDD];
    free_BigInt[iIDD];
    free_BigInt[iScratch];
end

proc test_guess[iIDD, iScratch, iB:BigInt, guess:i32] i32
var res:i32
begin
    copy_BigInt[iScratch, iB];
    abs_mult_digit[iScratch, guess, iScratch];
    set res = abs_comp[iScratch, iIDD];
    if res == EQ begin
        return EQ;
    end
    if res == LESS begin 
        abs_add[iScratch, iB, iScratch];
        set res = abs_comp[iScratch, iIDD];
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

proc comp_i32[iA:BigInt, b:i32] i32
var negA, negB:bool, res:i32
begin
    set negA = iA->Neg;
    set negB = b < 0;
    set b = abs[b];

    if not negA and not negB begin
        return abs_comp_digit[iA, b];
    end elseif negA and negB begin
        set res = abs_comp_digit[iA, b];
        # invert the result
        if res == MORE begin
            return LESS;
        end elseif res == LESS begin
            return MORE;
        end else begin
            return EQ;
        end
    end elseif not negA and negB begin
        return MORE;
    end elseif negA and not negB begin
        return LESS;
    end
    fatal[WTF, sizeof[WTF]];
    return ~1;
end

proc sub_i32[iA:BigInt, b:i32, iC:BigInt]
var negA, negB, res:bool
begin
    set negA = iA->Neg;
    set negB = b < 0;
    set b = abs[b];

    if not negA and not negB begin
    	# (1 * a) - (1 * b) = a-b
        set res = pos_sub_digit_sign[iA, b, iC];
        set iC->Neg = res;
    end elseif negA and negB begin
    	# (-1 * a) - (-1 * b) = (-1)*(a-b)
        set res = pos_sub_digit_sign[iA, b, iC];
        set iC->Neg = not res;
    end elseif negA and not negB begin
	    # (-1 * a) - (1 * b)  = (-1)*((1*b) - (-1*a)) = (-1)*(b+a)
        abs_add_digit[iA, b, iC];
        set iC->Neg = true;
    end elseif not negA and negB begin
    	# (1 * a) - (-1 * b)  = a+b
        abs_add_digit[iA, b, iC];
        set iC->Neg = false;
    end
end

proc add_i32[iA:BigInt, b:i32, iC:BigInt]
var negA, negB, res:bool
begin
    set negA = iA->Neg;
    set negB = b < 0;
    set b = abs[b];

    if not negA and not negB begin
        # (1 * a) + (1 * b)   = a+b
        abs_add_digit[iA, b, iC];
        set iC->Neg = false;
    end elseif negA and negB begin
    	# (-1 * a) + (-1 * b) = (-1)*(a+b)
        abs_add_digit[iA, b, iC];
        set iC->Neg = true;
    end elseif negA and not negB begin
    	# (-1 * a) + (1 * b)  = b-a = (-1)*(a-b)
        set res = pos_sub_digit_sign[iA, b, iC];
        set iC->Neg = not res;
    end elseif not negA and negB begin
    	# (1 * a) + (-1 * b)  = a-b
        set res = pos_sub_digit_sign[iA, b, iC];
        set iC->Neg = res;
    end
end

proc mult_i32[iA:BigInt, b:i32, iC:BigInt]
var neg, negA, negB:bool
begin
    set negA = iA->Neg;
    set negB = b < 0;
    set neg = mult_signal[negA, negB];
    set b = abs[b];
    abs_mult_digit[iA, b, iC];
    set iC->Neg = neg;
end

proc div_i32[iA:BigInt, b:i32, iQ:BigInt] i32
var neg, negA, negB:bool, rem:i32
begin
    set negA = iA->Neg;
    set negB = b < 0;
    set neg = mult_signal[negA, negB];
    set b = abs[b];

    set rem = abs_div_digit[iA, b, iQ];
    if abs_comp_digit[iQ, 0] != EQ begin
        set iQ->Neg = neg;
    end

    if rem != 0 and negA != (rem < 0) begin
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
proc pos_sub_sign[iA, iB, iC:BigInt] bool
var res:i32
begin
    set res = abs_comp[iA, iB];
    abs_sub[iA, iB, iC];
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
proc pos_sub_digit_sign[iA:BigInt, b:i32, iC:BigInt] bool
var res:i32
begin
    set res = abs_comp_digit[iA, b];
    abs_sub_digit[iA, b, iC];
    if res == MORE begin
        return false;
    end elseif res == LESS begin
        return true;
    end
    return false;
end

proc comp[iA, iB:BigInt] i32
var negA, negB:bool, res:i32
begin
    set negA = iA->Neg;
    set negB = iB->Neg;
    if not negA and not negB begin
        return abs_comp[iA, iB];
    end elseif negA and negB begin
        set res = abs_comp[iA, iB];
        # invert the result
        if res == MORE begin
            return LESS;
        end elseif res == LESS begin
            return MORE;
        end else begin
            return EQ;
        end
    end elseif not negA and negB begin
        return MORE;
    end elseif negA and not negB begin
        return LESS;
    end
    fatal[WTF, sizeof[WTF]];
    return ~1;
end
