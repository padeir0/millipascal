import bigint as bint
from io import fatal
from ioutil import put_char
from malloc import malloc_panic as malloc, free
from constants import EQ, MORE, LESS

export
    BigRat, BigRatArray,
    new_BigRat, free_BigRat, copy_BigRat, set_BigRat,
    put_decimal, put_BigRat,

    norm,
    comp_norm,
    add, sub, mult, div, neg, inv,
    minimize, sqrt2,

data begin
    NOT_NORMALIZED "rationals not normalized!\n";
    ERR_SCRATCHTOOSMALL "scratch buffer too small!\n";
end

struct BigRatArray begin
    Num:BigRat;
end

struct BigRat begin
    Num, Den:bint::BigInt;
end

proc new_BigRat[num, den:i32] BigRat
var out:BigRat
begin
    set out = malloc[sizeof[BigRat]]:BigRat;
    set out->Num = bint::new_BigInt[num];
    set out->Den = bint::new_BigInt[den];
    return out;
end

proc copy_BigRat[dest, source:BigRat]
begin
    bint::copy_BigInt[dest->Num, source->Num];
    bint::copy_BigInt[dest->Den, source->Den];
end

proc free_BigRat[rat:BigRat]
begin
    bint::free_BigInt[rat->Num];
    bint::free_BigInt[rat->Den];
    free[rat:ptr];
end

proc put_BigRat[rat:BigRat]
begin
    bint::put_BigInt[rat->Num];
    put_char['/'];
    bint::put_BigInt[rat->Den];
end

proc set_BigRat[rat:BigRat, num, den:i32]
begin
    bint::set_BigInt[rat->Num, num];
    bint::set_BigInt[rat->Den, den];
end

# prints (9*digits) decimal places
proc put_decimal[rat:BigRat, digits:i32, scratch:bint::BigIntArray, scratch_size:i32]
var q, r, t, scr:bint::BigInt, i:i32
begin
    if scratch_size < 4 begin
        fatal[ERR_SCRATCHTOOSMALL, sizeof[ERR_SCRATCHTOOSMALL]];
    end
    set q = scratch[0]->Int;
    set r = scratch[1]->Int;
    set t = scratch[2]->Int;
    set scr = scratch[3]->Int;

    bint::_abs_div[rat->Num, rat->Den, q, r, scr];
    bint::_put_BigInt[q, false, true];
    put_char['.'];
    set i = 0;
    while i < digits begin
        bint::copy_BigInt[t, r];
        bint::mult_base[t];
        bint::_abs_div[t, rat->Den, q, r, scr];

        if bint::is_zero[r] begin
            bint::_put_BigInt[q, true, false];
            return;
        end
        bint::_put_BigInt[q, true, true];
        set i++;
    end
end

# A, B, C must be different objects
proc add[A,B,C:BigRat]
begin
    if bint::comp[A->Den, B->Den] == EQ begin
        bint::add[A->Num, B->Num, C->Num];
        bint::copy_BigInt[C->Den, A->Den];
        return;
    end
    bint::mult[A->Num, B->Den, C->Num];
    bint::mult[B->Num, A->Den, C->Den];
    bint::add[C->Num, C->Den, C->Num];
    bint::mult[A->Den, B->Den, C->Den];
end

# A, B, C must be different objects
proc sub[A,B,C:BigRat]
begin
    if bint::comp[A->Den, B->Den] == EQ begin
        bint::sub[A->Num, B->Num, C->Num];
        bint::copy_BigInt[C->Den, A->Den];
        return;
    end
    bint::mult[A->Num, B->Den, C->Num];
    bint::mult[B->Num, A->Den, C->Den];
    bint::sub[C->Num, C->Den, C->Num];
    bint::mult[A->Den, B->Den, C->Den];
end

proc mult[A,B,C:BigRat]
begin
    bint::mult[A->Num, B->Num, C->Num];
    bint::mult[A->Den, B->Den, C->Den];
end

proc div[A,B,C:BigRat]
begin
    bint::mult[A->Num, B->Den, C->Num];
    bint::mult[A->Den, B->Num, C->Den];
end

proc neg[A:BigRat]
begin
    bint::neg[A->Num];
end

proc inv[A, B:BigRat]
begin
    bint::copy_BigInt[B->Num, A->Den];
    bint::copy_BigInt[B->Den, A->Num];
end

# normalizes A and B to have the same Denominator
proc norm[A, B:BigRat, scratch:bint::BigInt]
begin
    bint::mult[A->Num, B->Den, scratch];
    bint::copy_BigInt[A->Num, scratch];

    bint::mult[B->Num, A->Den, scratch];
    bint::copy_BigInt[B->Num, scratch];

    bint::mult[A->Den, B->Den, scratch];
    bint::copy_BigInt[B->Den, scratch];
    bint::copy_BigInt[A->Den, scratch];
end

proc comp_norm[A, B:BigRat] i32
var X,Y:bint::BigInt, res:i32
begin
    if bint::comp[A->Den, B->Den] != EQ begin
        fatal[NOT_NORMALIZED, sizeof[NOT_NORMALIZED]];
    end
    return bint::comp[A->Num, B->Num];
end

proc minimize[A:BigRat, scratch:bint::BigIntArray, size:i32]
var scr_int, scr_q, scr_r, scr_div:bint::BigInt
begin
    if size < 5 begin # non-overlapping: 1 here, 4 in gcd
        fatal[ERR_SCRATCHTOOSMALL, sizeof[ERR_SCRATCHTOOSMALL]];
    end

    set scr_int = scratch[0]->Int;
    bint::gcd[A->Num, A->Den, scr_int, scratch[1], size-1];

    set scr_q = scratch[1]->Int;
    set scr_r = scratch[2]->Int;
    set scr_div = scratch[3]->Int;
    bint::_div[A->Num, scr_int, scr_q, scr_r, scr_div];
    bint::copy_BigInt[A->Num, scr_q];
    bint::_div[A->Den, scr_int, scr_q, scr_r, scr_div];
    bint::copy_BigInt[A->Den, scr_q];
end

proc sqrt2[
    out:BigRat,
    n:i32,
    scratch:BigRatArray,
    scratch_size:i32,
]
var i:i32,
    a_0, a_1, a_2:BigRat
begin
    if scratch_size < 3 begin
        fatal[ERR_SCRATCHTOOSMALL, sizeof[ERR_SCRATCHTOOSMALL]];
    end

    set a_0 = scratch[0]->Num;
    set a_1 = scratch[1]->Num;
    set a_2 = scratch[2]->Num;

    set_BigRat[out, 1, 1];
    set i = 0;
    while i < n begin
        bint::mult_i32[out->Den, 2, a_0->Den];
        bint::copy_BigInt[a_0->Num, out->Num];
        inv[out, a_1];
        add[a_0, a_1, a_2];
        copy_BigRat[out, a_2];
        set i++;
    end
end
