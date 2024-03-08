type A is i64
type B is i64


proc main
var a:A, b:B
begin
    set a = 1:A;
    set b = 1:B;

    if a == b begin
        exit 2ss;
    end
    exit 1ss;
end
