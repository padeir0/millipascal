type Point is ptr ^ begin
    X:i64;
    Y:i64;
end

data P:Point []

proc main
begin
    set @P.X = 1;
    set @P.Y = 2;
    if @P.X != 1 or @P.Y != 2 begin
        exit 1ss;
    end
    if P.X != P + Point.X begin
        exit 2ss;
    end
    if P.Y != P + Point.Y begin
        exit 3ss;
    end
end
