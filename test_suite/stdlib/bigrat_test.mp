from io import fatal
from ioutil import put_ln, put_char
from intutil import max
from bigrat import all
from bigint import BigInt, new_BigInt, free_BigInt
from memsizes import _1KB
from malloc import set_pool, set_heap
from conv import I16MAX

data begin
    pool1  [1 * _1KB];
    backup [2 * _1KB];
end

proc setup[]
var size:i32
begin
    set size = max[sizeof[BigInt], sizeof[BigRat]];
    set_pool[pool1, sizeof[pool1], 0s, size:i16];
    set_heap[backup, sizeof[backup], 0s, I16MAX];
end

data bad "test went wrong!\n"
proc went_wrong[]
begin
    fatal[bad, sizeof[bad]];
    exit 1ss;
end

proc main
var A, B, C:BigRat, scratch:BigInt
begin
    setup[];
    set A = new_BigRat[1,7];
    set B = new_BigRat[2,14];
    set C = new_BigRat[0,1];
    set scratch = new_BigInt[0];
    norm[A, B, scratch];

    put_BigRat[A];
    put_char['+'];
    put_BigRat[B];
    put_char['+'];
    put_BigRat[B];
    put_char['='];
    add[A, B, C];
    add[C, B, A];
    put_BigRat[A];
    put_ln[];
    free_BigRat[A];
    free_BigRat[B];
    free_BigRat[C];
    free_BigInt[scratch];
    exit 1ss; # test is not implemented yet
end
