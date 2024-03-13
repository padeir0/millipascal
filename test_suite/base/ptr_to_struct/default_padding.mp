type A is ptr^begin
    X:i32 {0};
    Y:i64 {A.X + sizeof[A.X]};
    Z:i64 {Y + sizeof[Y]};
end

type B is ptr^begin
    X:i32;
    Y:i64;
    Z:i64;
end

proc main
begin
    if A.X != B.X begin
        exit 1ss;
    end
    if A.Y != B.Y begin
        exit 2ss;
    end
    if A.Z != B.Z begin
        exit 3ss;
    end
end
