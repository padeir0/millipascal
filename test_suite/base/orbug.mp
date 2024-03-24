proc main
var a, b, c:i32
begin
    set a = 0;
    set b = 1;
    set c = 2;
    if f[a, b] != 1 or f[a, c] != 2 begin
        exit 1ss;
    end
end

proc f[a, b:i32] i32
begin
    return a+b;
end
