type A is i64

proc main
var a:A
begin
    set a = 2:A;
    if a*a != 4:A begin
        exit 1ss;
    end
    if a/a != 1:A begin
        exit 2ss;
    end
    if a+a != 4:A begin
        exit 3ss;
    end
    if a-a != 0:A begin
        exit 4ss;
    end
end
