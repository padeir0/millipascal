struct POINT begin
    X:i64 Y:i64
end

data P [sizeof POINT]
data Q:POINT
data R {:POINT 1, 1}

proc main
var p:ptr, r:ptr
begin
    set p = P:POINT
    set p->X = 1
    set p->Y = 1

    set Q->X = 1
    set Q->Y = 1

    set r = R:POINT

    if r->X != Q->X or r->Y != Q->Y begin
        exit 1ss
    end

    if p->X != Q->X or p->Y != Q->Y begin
        exit 2ss
    end

    if sizeof P != sizeof Q or sizeof P != sizeof R begin
        exit 3ss
    end 
end
