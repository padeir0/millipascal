type A is i64
type B is i64

proc main
var a:A, b:B
begin
    set a, b = F[];
    if a == 1:A and b == 2:A begin
        exit 1ss;
    end
    exit 2ss;
end

proc F[] A, A
begin
    return 1:A, 2:A;
end
