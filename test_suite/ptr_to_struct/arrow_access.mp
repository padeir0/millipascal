type Point is ptr ^ begin
    X:i64;
    Y:i64;
end

data P:Point []

proc main
begin
    set P->X = 1;
    set P->Y = 2;

    if P->X != 1 or P->Y != 2 begin
        exit 1ss;
    end
end
