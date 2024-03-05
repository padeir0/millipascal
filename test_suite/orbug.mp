proc main
var a, b, c:i64
begin
    set a = 0;
    set b = 1;
    set c = 2;
    if f[a, b] != 1 or f[a, c] != 2 begin
        exit 1ss;
    end
end

proc f[a, b:i64] i64
begin
    return a+b;
end
