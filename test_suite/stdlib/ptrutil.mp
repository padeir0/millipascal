from io import fatal
from conv import u64_to_i32, OK
export distance

data OUTOFRANGE "distance doens't fit in an i32\n"

proc distance[a,b:ptr] i32
var res:i32, status:i8
begin
    set res, status = u64_to_i32[distance_u64[a,b]];
    if status != OK begin
        fatal[OUTOFRANGE, sizeof[OUTOFRANGE]];
    end
    return res;
end

proc distance_u64[a,b:ptr] u64
begin
    if a > b begin
        return a:u64 - b:u64;
    end elseif a < b begin
        return b:u64 - a:u64;
    end else begin
        return 0ul;
    end
end
