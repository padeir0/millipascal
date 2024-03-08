type A is i64
type B is i64

proc main
var a:A, b:B
begin
    set a = 1:A;
    set b = 1:B;

    F1[b];
    F2[a];
    exit 1ss;
end

proc F1[a:A]
begin
end
proc F2[b:B]
begin
end
