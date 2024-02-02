from ioutil import put_int, put_char

proc main
var a, b
begin
    set a = ~7
    set b = 7
    if a%5 != ~2 begin
        exit 1ss
    end if
    if b%5 != 2 begin
        exit 2ss
    end if
end proc
