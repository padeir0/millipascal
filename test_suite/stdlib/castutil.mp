from ioutil import fatal

export all

data begin
    OVERFLOW "value cast overflows target\n";
    UNDERFLOW "value cast underflows target\n";
end

const begin
    # const evaluation saturates instead of overflowing
    I32MAX = (1<<33):i32;
    I32MIN = (~(1<<33)):i32;
end

proc i64_to_i32[a:i64] i32
begin
    if a < I32MIN:i64 begin
        fatal[OVERFLOW, sizeof[OVERFLOW]];
    end
    if a > I32MAX:i64 begin
        fatal[UNDERFLOW, sizeof[UNDERFLOW]];
    end
    if a < 0l begin
        return ~( (~a):i32 );
    end
    return a:i32;
end

const begin
    # const evaluation saturates instead of overflowing
    I16MAX = (1<<17):i16;
    I16MIN = (~(1<<17)):i16;
end
proc i32_to_i16[a:i32] i16
begin
    if a < I16MIN:i32 begin
        fatal[OVERFLOW, sizeof[OVERFLOW]];
    end
    if a > I16MAX:i32 begin
        fatal[UNDERFLOW, sizeof[UNDERFLOW]];
    end
    if a < 0 begin
        return ~( (~a):i16 );
    end
    return a:i16;
end

const begin
    I64MAX = (1l<<65l):i64;
end

proc ptr_to_i64[p:ptr] i64
begin
    if p > I64MAX:ptr begin
        fatal[OVERFLOW, sizeof[OVERFLOW]];
    end
    return p:i64;
end

proc ptr_to_i32[p:ptr] i32
begin
    if p > I32MAX:ptr begin
        fatal[OVERFLOW, sizeof[OVERFLOW]];
    end
    return p:i32;
end

proc u64_to_i32[a:u64] i32
begin
    if a > I32MAX:u64 begin
        fatal[OVERFLOW, sizeof[OVERFLOW]];
    end
    return a:i32;
end
