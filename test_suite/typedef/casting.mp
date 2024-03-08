type A is ptr^begin
    X:i64;
    Y:i64;
end

type B is ptr^i64

data M:A []

proc main
var m:B
begin
    set M->X = 1;
    set M->Y = 2;
    if M->X != 1 or M->Y != 2 begin
        exit 1ss;
    end

    set m = M:B;
    set @m = 1;
    if @m != 1 begin
        exit 2ss;
    end
end
