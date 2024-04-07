proc M<stack>[a, b:i64] i64
begin
    return a + b;
end

proc main
var p:proc<reg>[i64,i64][i64]
begin
    set p = M;
    exit 1ss;
end
