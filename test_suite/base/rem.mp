proc main
var a, b:i32
begin
    set a = ~7;
    set b = 7;
    if a%5 != ~2 begin
        exit 1ss;
    end
    if b%5 != 2 begin
        exit 2ss;
    end
end
