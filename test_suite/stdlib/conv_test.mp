from conv import all
from io import print

data begin
    buff1 [128];
    buff2 [128];
end

proc main
begin
    test_u64_dec[];
    test_i64_dec[];
end

data nl "\n"

proc test_u64_dec
var n:u64,
    p:ptr, size:i32,
    res:u64, status:i8,
begin
    set n = 0ul;
    while n < 1_000_000ul begin
        set p, size = u64_to_dec[n, buff1, sizeof[buff1]];
        set res, status = dec_to_u64[p, size];
        if status != OK begin
            print[p, size];
            print[nl, sizeof[nl]];
            exit 20ss + status;
        end
        if res != n begin
            print[p, size];
            print[nl, sizeof[nl]];
            exit 10ss;
        end
        set n += 11ul;
    end
end

proc test_i64_dec
var n:i64,
    p:ptr, size:i32,
    res:i64, status:i8,
begin
    set n = ~500_000l;
    while n < 500_000l begin
        set p, size = i64_to_dec[n, buff1, sizeof[buff1]];
        set res, status = dec_to_i64[p, size];
        if status != OK begin
            print[p, size];
            print[nl, sizeof[nl]];
            exit 30ss + status;
        end
        if res != n begin
            print[p, size];
            print[nl, sizeof[nl]];
            exit 11ss;
        end
        set n += 11l;
    end
end
