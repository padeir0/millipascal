from io import print, fatal
from ioutil import put_ln, put_char, put_i32
from intutil import max
from bigrat import BigRat, new_BigRat, free_BigRat, copy_BigRat, minimize, add
from bigint import BigInt, BigIntArray, new_BigInt, free_BigInt, put_BigInt, pow, div
from memsizes import _1KB
from malloc import set_pool, set_heap, empty
from conv import I16MAX

data begin
    pool1  [1 * _1KB];
    backup [16 * _1KB];
end

proc setup[]
var size:i32
begin
    set size = max[sizeof[BigInt], sizeof[BigRat]];
    set_pool[pool1, sizeof[pool1], 0s, size:i16];
    set_heap[backup, sizeof[backup], 0s, I16MAX];
end

const scratch_size = 5
data scratch:BigIntArray [scratch_size]

proc alloc_scratch[]
var i:i32
begin
    set i = 0;
    while i < scratch_size begin
        set scratch[i]->Int = new_BigInt[0];
        set i++;
    end
end

proc free_scratch[]
var i:i32
begin
    set i = 0;
    while i < scratch_size begin
        free_BigInt[scratch[i]->Int];
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
end

# adds two rationals together a hundred thousand times
# minimizing the fraction each time
proc BENCH1
var A, B, C:BigRat, i:i32
begin
    alloc_scratch[];
    set A = new_BigRat[2,7];
    set B = new_BigRat[3,5];
    set C = new_BigRat[0,1];

    while i <= 100_000 begin
        add[A, B, C];
        copy_BigRat[A, C];
        minimize[A, scratch, scratch_size];
        
        set i++;
    end

    free_BigRat[A];
    free_BigRat[B];
    free_BigRat[C];
    free_scratch[];

    if not empty[] begin
        exit 1ss; # test is not implemented yet
    end
end

proc BENCH2
var a, b, out1, out2, scratch:BigInt
begin
    set a = new_BigInt[2];
    set b = new_BigInt[3];
    set out1 = new_BigInt[0];
    set out2 = new_BigInt[0];
    set scratch = new_BigInt[0];

    pow[a, 4096, out1, scratch];
    pow[b, 2048, out2, scratch];
    div[out1, out2, a, b];

    free_BigInt[a];
    free_BigInt[b];
    free_BigInt[out1];
    free_BigInt[out2];
    free_BigInt[scratch];
    if not empty[] begin
        exit 1ss;
    end
end
