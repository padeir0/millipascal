type A as i64
type B as i64

proc main
var a:A, b:B
begin
    set a = 2;
    if Square[a] != 4 begin
        exit 1ss;
    end
    set b = Square[a];
    if Square[b] != 16 begin
        exit 2ss;
    end
end

proc Square[a:i64] A
begin
    return a*a;
end
