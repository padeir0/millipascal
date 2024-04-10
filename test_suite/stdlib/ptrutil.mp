from castutil import u64_to_i32
export distance

proc distance[a,b:ptr] i32
begin
    return u64_to_i32[distance_u64[a,b]];
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
