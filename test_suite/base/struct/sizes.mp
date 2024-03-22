struct A begin
    X, Y:i64;
end

struct B begin
    A,B,C,D:i32;
end

proc main
begin
    if sizeof[A] != 2*sizeof[i64] begin
        exit 1ss;
    end
    if sizeof[B] != 4*sizeof[i32] begin
        exit 2ss;
    end
    if sizeof[B] != sizeof[A] begin
        exit 3ss;
    end
    if sizeof[A] != sizeof[B] or
        sizeof[A] != sizeof[ptr]
    begin
        exit 4ss;
    end
    if sizeof[A.X] != sizeof[i64] begin
        exit 5ss;
    end
end
