type A is i64

proc main
var a:A
begin
    set a = 2:A;
    if Square[a] != 4:A begin
        exit 1ss;
    end
    if Square[Square[a]] != 16:A begin
        exit 2ss;
    end
    if Square[Square[Square[a]]] != 256:A begin
        exit 3ss;
    end
end

proc Square[a:A] A
begin
    return a*a;
end
