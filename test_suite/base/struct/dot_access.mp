struct Point begin
    X:i64;
    Y:i64;
end

data P:Point []

proc main
begin
    set (P.X)@i64 = 1l;
    set (P.Y)@i64 = 2l;
    if P->X != 1l or P->Y != 2l begin
        exit 1ss;
    end
    if P.X != P + Point.X begin
        exit 2ss;
    end
    if P.Y != P + Point.Y begin
        exit 3ss;
    end
end
