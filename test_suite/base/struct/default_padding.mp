struct A [sizeof[i32] + 2*sizeof[i64]]
begin
    X:i32 {0};
    Y:i64 {A.X + sizeof[A.X]};
    Z:i64 {A.Y + sizeof[A.Y]};
end

struct B begin
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
    if sizeof[A] != sizeof[B] begin
        exit 4ss;
    end
end
