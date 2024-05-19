from io import print, fatal
from ioutil import put_ln, put_char, put_i32
from intutil import max
from bigrat import BigRat, BigRatArray, new_BigRat, free_BigRat, copy_BigRat, put_BigRat, minimize, add as add_rat, put_decimal, sqrt2, set_BigRat_B, mult as mult_rat, inv, div as div_rat
from bigint import BigInt, BigIntArray, new_BigInt, free_BigInt, put_BigInt, set_BigInt, pow, div, add_i32, mult, mult_i32, fact, pow_i32
from memsizes import _1KB, _1MB
from malloc import set_pool, set_heap, empty, print_region_use
from conv import I32MAX
import debug_util as debug

data begin
    pool1  [1 * _1KB];
    backup [8 * _1MB];
end

proc setup[]
var size:i32
begin
    set size = max[sizeof[BigInt], sizeof[BigRat]];
    set_pool[pool1, sizeof[pool1], 0, size];
    set_heap[backup, sizeof[backup], 0, I32MAX];
    debug::setup[];
end

const scratch_size = 5
data scratch:BigIntArray [scratch_size]
data scratch_rat:BigRatArray [scratch_size]

proc alloc_scratch[]
var i:i32
begin
    set i = 0;
    while i < scratch_size begin
        set scratch[i]->Int = new_BigInt[0];
        set scratch_rat[i]->Num = new_BigRat[0, 1];
        set i++;
    end
end

proc free_scratch[]
var i:i32
begin
    set i = 0;
    while i < scratch_size begin
        free_BigInt[scratch[i]->Int];
        free_BigRat[scratch_rat[i]->Num];
        set i++;
    end
end

data bad "test went wrong!\n"
proc went_wrong[]
begin
    fatal[bad, sizeof[bad]];
    exit 1ss;
end

proc main
begin
    setup[];
    BENCH1[];
    BENCH2[];
    BENCH3[];
    BENCH4[];
    BENCH5[];
    debug::print_counts[];
end

# adds two rationals together a hundred thousand times
# minimizing the fraction each time
# calculates: 2/7 + \sum_{i=0}^{100_000} 3/5
proc BENCH1
var A, B, C:BigRat, i:i32
begin
    alloc_scratch[];
    set A = new_BigRat[1,4];
    set B = new_BigRat[3,5];
    set C = new_BigRat[0,1];

    set i = 0;
    while i <= 100_000 begin
        add_rat[A, B, C];
        copy_BigRat[A, C];
        minimize[A, scratch, scratch_size];
        
        set i++;
    end

    free_BigRat[A];
    free_BigRat[B];
    free_BigRat[C];
    free_scratch[];

    if not empty[] begin
        exit 1ss;
    end
end

# calculates (2^4096)/(3^2058)
proc BENCH2
var a, b, out1, out2, scratch:BigInt
begin
    set a = new_BigInt[0];
    set b = new_BigInt[0];
    set out1 = new_BigInt[0];
    set out2 = new_BigInt[0];
    set scratch = new_BigInt[0];

    pow_i32[2, 4096, out1];
    pow_i32[3, 2048, out2];
    div[out1, out2, a, b];

    free_BigInt[a];
    free_BigInt[b];
    free_BigInt[out1];
    free_BigInt[out2];
    free_BigInt[scratch];

    if not empty[] begin
        exit 2ss;
    end
end

# calculates pi using Leibniz series
proc BENCH3
var out, rat, scr:BigRat, i:i32
begin
    alloc_scratch[];
    set out = new_BigRat[0, 1];
    set rat = new_BigRat[1, 1];
    set scr = new_BigRat[1, 1];

    set i = 0;
    while i <= 1_000 begin
        if i % 2 == 0 begin
            set rat->Num->Neg = false;
        end else begin
            set rat->Num->Neg = true;
        end

        add_rat[out, rat, scr];
        set out <> scr;

        if i % 100 == 0 begin
            minimize[out, scratch, scratch_size];
        end

        add_i32[rat->Den, 2, rat->Den];
        set i++;
    end

    copy_BigRat[scr, out];
    mult_i32[out->Num, 4, scr->Num];
    put_char['p'];
    put_char['i'];
    put_char['~'];
    put_decimal[scr, 4, scratch, scratch_size];
    put_ln[];

    free_scratch[];
    free_BigRat[out];
    free_BigRat[rat];
    free_BigRat[scr];

    if not empty[] begin
        exit 3ss;
    end
end

# computes and prints 10 thousand digits of sqrt(2)
proc BENCH4
var out:BigRat
begin
    alloc_scratch[];

    set out = new_BigRat[0, 1];
    sqrt2[out, 15, scratch_rat, scratch_size];

    put_decimal[out, 1111, scratch, scratch_size];
    put_ln[];
    free_scratch[];
    free_BigRat[out];

    if not empty[] begin
        exit 4ss;
    end
end

const N = 200

# computes and prints 60 digits of pi
proc BENCH5
var k:i32, t, scr, out:BigRat
begin
    alloc_scratch[];
    set t = new_BigRat[0,1];
    set scr = new_BigRat[0,1];
    set out = new_BigRat[0,1];

    set k = 0;
    while k < N begin
        if k == 50 or k == 100 begin
            minimize[out, scratch, scratch_size];
            minimize[t, scratch, scratch_size];
            minimize[scr, scratch, scratch_size];
            free_scratch[];
            alloc_scratch[];
        end

        term[k, t];
        add_rat[out, t, scr];
        copy_BigRat[out, scr];

        set k++;
    end

    put_char['p'];
    put_char['i'];
    put_char['~'];
    mult_i32[out->Num, 2, out->Num];
    put_decimal[out, 7, scratch, scratch_size];
    put_ln[];

    free_scratch[];
    free_BigRat[t];
    free_BigRat[scr];
    free_BigRat[out];

    if not empty[] begin
        exit 5ss;
    end
end

# out = ((2^k)*((k!)^2)) / (2k+1)!
proc term[k:i32, out:BigRat]
var a,b,c:BigInt
begin
    if scratch_size < 4 begin
        exit 57ss;
    end

    set a = scratch[0]->Int;
    set b = scratch[1]->Int;
    set c = scratch[2]->Int;

    fact[k, a];
    mult[a, a, b];
    pow_i32[2, k, a];
    mult[a, b, c];

    fact[2*k+1, a];

    set_BigRat_B[out, c, a];
end
